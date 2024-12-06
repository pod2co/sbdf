use crate::{
    ColumnMetadata, ColumnSlice, Decimal, EncodedValue, FileHeader, Metadata, Object, Property,
    SbdfError, SectionId, TableMetadata, TableSlice, ValueArrayEncoding, ValueType,
    COLUMN_METADATA_NAME, COLUMN_METADATA_TYPE,
};
use std::io::{Cursor, Read};

#[derive(Debug)]
pub struct SbdfReader<'a> {
    cursor: Cursor<&'a [u8]>,
}

impl<'a> SbdfReader<'a> {
    pub fn new(cursor: &'a [u8]) -> Self {
        let cursor = Cursor::new(cursor);
        SbdfReader { cursor }
    }

    fn read_byte(&mut self) -> Result<u8, SbdfError> {
        let mut buffer = [0; 1];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(buffer[0]),
            Err(_) => Err(SbdfError::InvalidByte),
        }
    }

    fn read_7bit_packed_int(&mut self) -> Result<i32, SbdfError> {
        let mut value = 0;

        for i in 0..5 {
            let byte = self.read_byte()?;
            value |= ((byte & 0x7f) as i32) << (7 * i);
            if byte & 0x80 == 0 {
                break;
            }
        }

        Ok(value)
    }

    fn read_int(&mut self) -> Result<i32, SbdfError> {
        let mut buffer = [0; 4];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(i32::from_le_bytes(buffer)),
            Err(_) => Err(SbdfError::InvalidInt),
        }
    }

    fn read_long(&mut self) -> Result<i64, SbdfError> {
        let mut buffer = [0; 8];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(i64::from_le_bytes(buffer)),
            Err(_) => Err(SbdfError::InvalidLong),
        }
    }

    fn read_float(&mut self) -> Result<f32, SbdfError> {
        let mut buffer = [0; 4];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(f32::from_le_bytes(buffer)),
            Err(_) => Err(SbdfError::InvalidFloat),
        }
    }

    fn read_double(&mut self) -> Result<f64, SbdfError> {
        let mut buffer = [0; 8];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(f64::from_le_bytes(buffer)),
            Err(_) => Err(SbdfError::InvalidDouble),
        }
    }

    fn read_string(&mut self, size: usize) -> Result<String, SbdfError> {
        let mut buffer = vec![0; size];
        self.cursor
            .read_exact(&mut buffer)
            .map_err(|_| SbdfError::InvalidString)?;

        Ok(String::from_utf8(buffer).map_err(|_| SbdfError::InvalidString)?)
    }

    fn read_bool(&mut self) -> Result<bool, SbdfError> {
        let byte = self.read_byte()?;
        match byte {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(SbdfError::InvalidBool),
        }
    }

    fn read_bytes(&mut self, size: usize) -> Result<Vec<u8>, SbdfError> {
        let mut buffer = vec![0; size];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(buffer),
            Err(_) => Err(SbdfError::InvalidByte),
        }
    }

    fn read_decimal(&mut self) -> Result<Decimal, SbdfError> {
        let mut buffer = [0; 16];
        match self.cursor.read_exact(&mut buffer) {
            Ok(()) => Ok(buffer),
            Err(_) => Err(SbdfError::InvalidByte),
        }
    }

    fn read_multiple<T, F>(&mut self, count: usize, read_value: F) -> Result<Vec<T>, SbdfError>
    where
        F: Fn(&mut Self) -> Result<T, SbdfError>,
    {
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(read_value(self)?);
        }
        Ok(values)
    }

    fn read_value_type(&mut self) -> Result<ValueType, SbdfError> {
        self.read_byte()?.try_into()
    }

    fn read_objects(
        &mut self,
        value_type: ValueType,
        count: usize,
        is_packed_array: bool,
    ) -> Result<Object, SbdfError> {
        Ok(match (value_type, count) {
            (ValueType::Bool, 1) => Object::Bool(self.read_bool()?),
            (ValueType::Int, 1) => Object::Int(self.read_int()?),
            (ValueType::Long, 1) => Object::Long(self.read_long()?),
            (ValueType::Float, 1) => Object::Float(self.read_float()?),
            (ValueType::Double, 1) => Object::Double(self.read_double()?),
            (ValueType::DateTime, 1) => Object::DateTime(self.read_long()?),
            (ValueType::Date, 1) => Object::Date(self.read_long()?),
            (ValueType::Time, 1) => Object::Time(self.read_long()?),
            (ValueType::TimeSpan, 1) => Object::TimeSpan(self.read_long()?),
            (ValueType::String, 1) => {
                let length = if is_packed_array {
                    // Ignore byte size.
                    let _ = self.read_int()?;
                    self.read_7bit_packed_int()?
                } else {
                    self.read_int()?
                } as usize;
                Object::String(self.read_string(length)?)
            }
            (ValueType::Binary, 1) => {
                let length = if is_packed_array {
                    // Ignore byte size.
                    let _ = self.read_int()?;
                    self.read_7bit_packed_int()?
                } else {
                    self.read_int()?
                } as usize;
                Object::Binary(self.read_bytes(length)?.into_boxed_slice())
            }
            (ValueType::Decimal, 1) => Object::Decimal(self.read_decimal()?),
            (ValueType::Bool, _) => Object::BoolArray(
                self.read_multiple(count, SbdfReader::read_bool)?
                    .into_boxed_slice(),
            ),
            (ValueType::Int, _) => Object::IntArray(
                self.read_multiple(count, SbdfReader::read_int)?
                    .into_boxed_slice(),
            ),
            (ValueType::Long, _) => Object::LongArray(
                self.read_multiple(count, |reader| reader.read_long())
                    .map_err(|_| SbdfError::InvalidObject)?
                    .into_boxed_slice(),
            ),
            (ValueType::Float, _) => Object::FloatArray(
                self.read_multiple(count, SbdfReader::read_float)?
                    .into_boxed_slice(),
            ),
            (ValueType::Double, _) => Object::DoubleArray(
                self.read_multiple(count, SbdfReader::read_double)?
                    .into_boxed_slice(),
            ),
            (ValueType::DateTime, _) => Object::DateTimeArray(
                self.read_multiple(count, SbdfReader::read_long)?
                    .into_boxed_slice(),
            ),
            (ValueType::Date, _) => Object::DateArray(
                self.read_multiple(count, SbdfReader::read_long)?
                    .into_boxed_slice(),
            ),
            (ValueType::Time, _) => Object::TimeArray(
                self.read_multiple(count, SbdfReader::read_long)?
                    .into_boxed_slice(),
            ),
            (ValueType::TimeSpan, _) => {
                Object::TimeSpanArray(self.read_multiple(count, SbdfReader::read_long)?)
            }
            (ValueType::String, _) => {
                let mut result = Vec::with_capacity(count);

                if is_packed_array {
                    // Ignore byte size.
                    let _ = self.read_int()?;

                    for _ in 0..count {
                        let length = self.read_7bit_packed_int()? as usize;
                        result.push(self.read_string(length)?);
                    }
                } else {
                    for _ in 0..count {
                        let length = self.read_int()? as usize;
                        result.push(self.read_string(length)?);
                    }
                }

                Object::StringArray(result.into_boxed_slice())
            }
            (ValueType::Binary, _) => {
                let mut result = Vec::with_capacity(count);

                if is_packed_array {
                    // Ignore byte size.
                    let _ = self.read_int()?;

                    for _ in 0..count {
                        let length = self.read_7bit_packed_int()? as usize;
                        result.push(self.read_bytes(length)?.into_boxed_slice());
                    }
                } else {
                    for _ in 0..count {
                        let length = self.read_int()? as usize;
                        result.push(self.read_bytes(length)?.into_boxed_slice());
                    }
                }

                Object::BinaryArray(result.into_boxed_slice())
            }
            (ValueType::Decimal, _) => {
                Object::DecimalArray(self.read_multiple(count, SbdfReader::read_decimal)?)
            }
        })
    }

    fn read_object(&mut self, value_type: ValueType) -> Result<Object, SbdfError> {
        self.read_objects(value_type, 1, false)
    }

    pub fn read_section_id(&mut self) -> Result<SectionId, SbdfError> {
        if self.read_byte()? != 0xdfu8 {
            return Err(SbdfError::MagicNumberMismatch);
        }

        if self.read_byte()? != 0x5bu8 {
            return Err(SbdfError::MagicNumberMismatch);
        }

        self.read_byte().and_then(|value| value.try_into())
    }

    pub fn expect_section_id(&mut self, expected: SectionId) -> Result<(), SbdfError> {
        let actual = self.read_section_id()?;
        if actual != expected {
            return Err(SbdfError::WrongSectionId { expected, actual });
        }
        Ok(())
    }

    pub fn read_file_header(&mut self) -> Result<FileHeader, SbdfError> {
        let major_version = self.read_byte()?;
        let minor_version = self.read_byte()?;

        if major_version != 1 || minor_version != 0 {
            return Err(SbdfError::UnsupportedVersion {
                major_version,
                minor_version,
            });
        }

        Ok(FileHeader {
            major_version,
            minor_version,
        })
    }

    pub fn read_metadata_value(&mut self, value_type: ValueType) -> Result<Object, SbdfError> {
        match self.read_byte()? {
            0 => value_type.default_object(),
            1 => self.read_object(value_type),
            _ => Err(SbdfError::MetadataValueArrayLengthMustBeZeroOrOne),
        }
    }

    fn read_metadata(&mut self) -> Result<Metadata, SbdfError> {
        let length = self.read_int()? as usize;
        let name = self.read_string(length)?;

        let value_type = self.read_value_type()?;
        let value = self.read_metadata_value(value_type)?;
        let _default_value = self.read_metadata_value(value_type)?;

        Ok(Metadata { name, value })
    }

    pub fn read_table_metadata(&mut self) -> Result<TableMetadata, SbdfError> {
        let count: usize = self
            .read_int()?
            .try_into()
            .map_err(|_| SbdfError::InvalidSize)?;

        let mut table_metadata = Vec::with_capacity(count);

        for _ in 0..count {
            table_metadata.push(self.read_metadata()?);
        }

        let column_count = self.read_int()? as usize;
        let mut columns = Vec::with_capacity(column_count);

        let metadata_count = self.read_int()? as usize;
        let mut metadata = Vec::with_capacity(metadata_count);

        for _ in 0..metadata_count {
            let length = self.read_int()? as usize;
            let name = self.read_string(length)?;
            let value_type = self.read_value_type()?;
            let object = self.read_metadata_value(value_type)?;
            metadata.push((name, value_type, object));
        }

        for _ in 0..column_count {
            let mut maybe_name = None;
            let mut maybe_type = None;

            let mut column_metadata = Vec::with_capacity(metadata_count.saturating_sub(2));

            for j in 0..metadata_count {
                let has_metadata = self.read_byte()? != 0;
                if has_metadata {
                    let (name, ty, _default_value) = &metadata[j];
                    let value = self.read_object(*ty)?;

                    // Add metadata to the current column.
                    match name.as_str() {
                        COLUMN_METADATA_NAME => {
                            maybe_name = match value {
                                Object::String(name) => Some(name),
                                _ => return Err(SbdfError::InvalidMetadata),
                            };
                        }
                        COLUMN_METADATA_TYPE => {
                            maybe_type = match value {
                                Object::Binary(ty_raw) => {
                                    if ty_raw.len() != 1 {
                                        return Err(SbdfError::InvalidMetadata);
                                    }

                                    Some(ty_raw[0].try_into()?)
                                }
                                _ => return Err(SbdfError::InvalidMetadata),
                            }
                        }
                        _ => {
                            column_metadata.push(Metadata {
                                name: name.clone(),
                                value,
                            });
                        }
                    }
                }
            }

            column_metadata.shrink_to_fit();
            columns.push(ColumnMetadata {
                name: maybe_name.ok_or(SbdfError::InvalidMetadata)?,
                ty: maybe_type.ok_or(SbdfError::InvalidMetadata)?,
                other: column_metadata.into_boxed_slice(),
            });
        }

        Ok(TableMetadata {
            metadata: table_metadata.into_boxed_slice(),
            columns: columns.into_boxed_slice(),
        })
    }

    fn read_object_packed_array(&mut self, value_type: ValueType) -> Result<Object, SbdfError> {
        let count = self.read_int()? as usize;
        self.read_objects(value_type, count, true)
    }

    fn read_value_array(&mut self) -> Result<EncodedValue, SbdfError> {
        let encoding: ValueArrayEncoding = self.read_byte()?.try_into()?;
        let value_type = self.read_value_type()?;
        Ok(match encoding {
            ValueArrayEncoding::Plain => {
                let value = self.read_object_packed_array(value_type)?;
                EncodedValue::Plain { value }
            }
            ValueArrayEncoding::RunLength => {
                let _item_count = self.read_int()?;

                // The run lengths are byte arrays, so we can just read them directly instead of
                // going through the object deserialization process.
                let run_length_count = self.read_int()? as usize;
                let run_lengths = self.read_bytes(run_length_count)?;

                let values = self.read_object_packed_array(value_type)?;
                EncodedValue::RunLength {
                    run_lengths: run_lengths.into_boxed_slice(),
                    values,
                }
            }
            ValueArrayEncoding::BitArray => {
                let bit_count = self.read_int()? as usize;
                // Round up to the nearest byte.
                const BITS_PER_BYTE: usize = 8;
                let byte_length = bit_count.div_ceil(BITS_PER_BYTE);
                let bytes = self.read_bytes(byte_length)?;
                EncodedValue::BitArray {
                    bit_count,
                    bytes: bytes.into_boxed_slice(),
                }
            }
        })
    }

    fn read_properties(&mut self) -> Result<Box<[Property]>, SbdfError> {
        let count = self.read_int()? as usize;
        let mut properties = Vec::with_capacity(count);

        for _ in 0..count {
            let length = self.read_int()? as usize;
            let name = self.read_string(length)?;
            let values = self.read_value_array()?;

            properties.push(Property { name, values });
        }

        Ok(properties.into_boxed_slice())
    }

    fn read_column_slice(&mut self) -> Result<ColumnSlice, SbdfError> {
        self.expect_section_id(SectionId::ColumnSlice)?;

        let values = self.read_value_array()?;
        let properties = self.read_properties()?;

        Ok(ColumnSlice { values, properties })
    }

    pub fn read_table_slice(
        &mut self,
        table_metadata: &TableMetadata,
    ) -> Result<TableSlice, SbdfError> {
        let column_count = self.read_int()? as usize;

        if table_metadata.columns.len() != column_count {
            return Err(SbdfError::ColumnCountMismatch);
        }

        let mut column_slices = Vec::with_capacity(column_count);

        for _ in 0..column_count {
            column_slices.push(self.read_column_slice()?);
        }

        Ok(TableSlice {
            column_slices: column_slices.into_boxed_slice(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_byte() {
        let buffer = [0x12, 0x34];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_byte().unwrap(), 0x12);
        assert_eq!(reader.read_byte().unwrap(), 0x34);
    }

    #[test]
    fn read_7bit_packed_int() {
        let buffer = [0x80, 0x08, 0x01, 0];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_7bit_packed_int().unwrap(), 1024);
        assert_eq!(reader.read_7bit_packed_int().unwrap(), 1);
        assert_eq!(reader.read_7bit_packed_int().unwrap(), 0);
    }

    #[test]
    fn read_int() {
        let buffer = [0x0, 0x4, 0x0, 0x0];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_int().unwrap(), 1024);
    }

    #[test]
    fn read_long() {
        let buffer = [0x0, 0x4, 0x0, 0x0, 0x0, 0x4, 0x0, 0x0];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_long().unwrap(), 1024 | 1024 << 32);
    }

    #[test]
    fn read_float() {
        let buffer = 123.456f32.to_le_bytes();
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_float().unwrap(), 123.456);
    }

    #[test]
    fn read_double() {
        let buffer = 123.456f64.to_le_bytes();
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_double().unwrap(), 123.456);
    }

    #[test]
    fn read_string() {
        let buffer = b"Hello, world!".to_vec();
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_string(buffer.len()).unwrap(), "Hello, world!");
    }

    #[test]
    fn read_bool() {
        let buffer = [0, 1];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_bool().unwrap(), false);
        assert_eq!(reader.read_bool().unwrap(), true);
    }

    #[test]
    fn read_bytes() {
        let buffer = b"Hello, world!".to_vec();
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_bytes(buffer.len()).unwrap(), buffer);
    }

    #[test]
    fn read_decimal() {
        let buffer = [1; 16];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_decimal().unwrap(), buffer);
    }

    #[test]
    fn read_value_type() {
        let buffer = [ValueType::TimeSpan as u8, ValueType::String as u8];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_value_type().unwrap(), ValueType::TimeSpan);
        assert_eq!(reader.read_value_type().unwrap(), ValueType::String);
    }

    #[test]
    fn read_section_id() {
        let buffer = [0xdf, 0x5b, 0x2];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(reader.read_section_id().unwrap(), SectionId::TableMetadata);
    }

    #[test]
    fn read_file_header() {
        let buffer = [0x1, 0x0];
        let mut reader = SbdfReader::new(&buffer);
        assert_eq!(
            reader.read_file_header().unwrap(),
            FileHeader {
                major_version: 1,
                minor_version: 0
            }
        );
    }
}
