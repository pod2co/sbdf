use std::io::Cursor;

use crate::{
    BinaryArray, BoolArray, ColumnMetadataType, ColumnProperties, ColumnSlice, DateArray,
    DateTimeArray, Decimal, DecimalArray, DoubleArray, EncodedBitArray, EncodedRunLength,
    EncodedValue, FileHeader, FloatArray, IntArray, LongArray, Metadata, Object, SbdfError,
    SectionId, StringArray, TableMetadata, TableSlice, TimeArray, TimeSpanArray,
    ValueArrayEncoding, ValueType,
};

fn bytes_needed_for_7bit_packed_int(value: i32) -> i32 {
    if value < 1 << 7 {
        return 1;
    }

    if value < 1 << 14 {
        return 2;
    }

    if value < 1 << 21 {
        return 3;
    }

    if value < 1 << 28 {
        return 4;
    }

    5
}

#[derive(Debug)]
pub struct SbdfWriter<'a> {
    cursor: Cursor<&'a mut Vec<u8>>,
}

impl<'a> SbdfWriter<'a> {
    pub fn new(cursor: Cursor<&'a mut Vec<u8>>) -> SbdfWriter<'a> {
        SbdfWriter { cursor }
    }

    fn write_byte(&mut self, byte: u8) -> Result<(), SbdfError> {
        self.cursor.get_mut().push(byte);
        Ok(())
    }

    fn write_7bit_packed_int(&mut self, mut value: i32) -> Result<(), SbdfError> {
        for _ in 0..5 {
            let byte = (value & 0x7f) as u8;

            value >>= 7;

            if value == 0 {
                self.write_byte(byte)?;
                break;
            } else {
                // Set continuation bit if we have any more bytes.
                self.write_byte(byte | 0x80)?;
            }
        }

        Ok(())
    }

    fn write_bytes_without_length(&mut self, value: &[u8]) -> Result<(), SbdfError> {
        self.cursor.get_mut().extend_from_slice(value);
        Ok(())
    }

    fn write_int(&mut self, value: i32) -> Result<(), SbdfError> {
        self.write_bytes_without_length(&value.to_le_bytes())
    }

    fn write_long(&mut self, value: i64) -> Result<(), SbdfError> {
        self.write_bytes_without_length(&value.to_le_bytes())
    }

    fn write_float(&mut self, value: f32) -> Result<(), SbdfError> {
        self.write_bytes_without_length(&value.to_le_bytes())
    }

    fn write_double(&mut self, value: f64) -> Result<(), SbdfError> {
        self.write_bytes_without_length(&value.to_le_bytes())
    }

    fn write_string(&mut self, value: &str, is_packed_array: bool) -> Result<(), SbdfError> {
        let bytes = value.as_bytes();
        let len: i32 = bytes
            .len()
            .try_into()
            .map_err(|_| SbdfError::StringTooLong)?;

        if is_packed_array {
            self.write_7bit_packed_int(len)?;
        } else {
            self.write_int(len)?;
        }

        self.write_bytes_without_length(bytes)
    }

    fn write_bool(&mut self, value: bool) -> Result<(), SbdfError> {
        self.write_byte(if value { 1 } else { 0 })
    }

    fn write_bytes(&mut self, value: &[u8], is_packed_array: bool) -> Result<(), SbdfError> {
        let len: i32 = value
            .len()
            .try_into()
            .map_err(|_| SbdfError::BytesTooLong)?;

        if is_packed_array {
            self.write_7bit_packed_int(len)?;
        } else {
            self.write_int(len)?;
        }

        self.write_bytes_without_length(value)
    }

    fn write_decimal(&mut self, value: &Decimal) -> Result<(), SbdfError> {
        self.write_bytes_without_length(value)
    }

    fn write_multiple<T, F>(&mut self, values: &[T], write_fn: F) -> Result<(), SbdfError>
    where
        F: Fn(&mut Self, &T) -> Result<(), SbdfError>,
    {
        for value in values.iter() {
            write_fn(self, value)?;
        }

        Ok(())
    }

    fn write_value_type(&mut self, value: ValueType) -> Result<(), SbdfError> {
        self.write_byte(value as u8)
    }

    fn write_object(&mut self, object: &Object, is_packed_array: bool) -> Result<(), SbdfError> {
        let checked_add_or_err =
            |a: i32, b: i32| a.checked_add(b).ok_or(SbdfError::TooManyValuesInArray);

        match object {
            Object::Bool(b) => self.write_bool(*b),
            Object::Int(i) => self.write_int(*i),
            Object::Long(l) => self.write_long(*l),
            Object::Float(f) => self.write_float(*f),
            Object::Double(d) => self.write_double(*d),
            Object::DateTime(dt) => self.write_long(dt.0),
            Object::Date(d) => self.write_long(d.0),
            Object::Time(t) => self.write_long(*t),
            Object::TimeSpan(ts) => self.write_long(*ts),
            Object::String(s) => {
                if is_packed_array {
                    // Write the byte size of the string, even though it won't be used by readers.
                    let string_len: i32 = s
                        .as_bytes()
                        .len()
                        .try_into()
                        .map_err(|_| SbdfError::StringTooLong)?;

                    let byte_len = checked_add_or_err(
                        bytes_needed_for_7bit_packed_int(string_len),
                        string_len,
                    )?;
                    self.write_int(byte_len)?;
                }

                self.write_string(s, is_packed_array)
            }
            Object::Binary(b) => {
                if is_packed_array {
                    // Write the byte size of the binary, even though it won't be used by readers.
                    let binary_len: i32 =
                        b.len().try_into().map_err(|_| SbdfError::BytesTooLong)?;

                    let byte_len = checked_add_or_err(
                        bytes_needed_for_7bit_packed_int(binary_len),
                        binary_len,
                    )?;
                    self.write_int(byte_len)?;
                }

                self.write_bytes(b, is_packed_array)
            }
            Object::Decimal(d) => self.write_decimal(d),
            Object::BoolArray(BoolArray(a)) => self.write_multiple(a, |w, ts| w.write_bool(*ts)),
            Object::IntArray(IntArray(a)) => self.write_multiple(a, |w, ts| w.write_int(*ts)),
            Object::LongArray(LongArray(a)) => self.write_multiple(a, |w, ts| w.write_long(*ts)),
            Object::FloatArray(FloatArray(a)) => self.write_multiple(a, |w, ts| w.write_float(*ts)),
            Object::DoubleArray(DoubleArray(a)) => {
                self.write_multiple(a, |w, ts| w.write_double(*ts))
            }
            Object::DateTimeArray(DateTimeArray(a)) => {
                self.write_multiple(a, |w, ts| w.write_long(*ts))
            }
            Object::DateArray(DateArray(a)) => self.write_multiple(a, |w, ts| w.write_long(*ts)),
            Object::TimeArray(TimeArray(a)) => self.write_multiple(a, |w, ts| w.write_long(*ts)),
            Object::TimeSpanArray(TimeSpanArray(a)) => {
                self.write_multiple(a, |w, ts| w.write_long(*ts))
            }
            Object::StringArray(StringArray(a)) => {
                if is_packed_array {
                    // Write the byte size of the string array, even though it won't be used by readers.
                    let mut total_byte_size = 0i32;

                    for s in a.iter() {
                        let string_len: i32 = s
                            .as_bytes()
                            .len()
                            .try_into()
                            .map_err(|_| SbdfError::StringTooLong)?;

                        total_byte_size = checked_add_or_err(
                            total_byte_size,
                            bytes_needed_for_7bit_packed_int(string_len),
                        )?;
                        total_byte_size = checked_add_or_err(total_byte_size, string_len)?;
                    }

                    self.write_int(total_byte_size)?;
                }

                for s in a.iter() {
                    self.write_string(s, is_packed_array)?;
                }

                Ok(())
            }
            Object::BinaryArray(BinaryArray(a)) => {
                if is_packed_array {
                    // Write the byte size of the binary array, even though it won't be used by readers.
                    let mut total_byte_size = 0i32;

                    for b in a.iter() {
                        let binary_len: i32 =
                            b.len().try_into().map_err(|_| SbdfError::BytesTooLong)?;

                        total_byte_size = checked_add_or_err(
                            total_byte_size,
                            bytes_needed_for_7bit_packed_int(binary_len),
                        )?;
                        total_byte_size = checked_add_or_err(total_byte_size, binary_len)?;
                    }

                    self.write_int(total_byte_size)?;
                }

                for b in a.iter() {
                    self.write_bytes(b, is_packed_array)?;
                }

                Ok(())
            }
            Object::DecimalArray(DecimalArray(a)) => {
                self.write_multiple(a, SbdfWriter::write_decimal)
            }
        }
    }

    fn write_unpacked_object(&mut self, value: &Object) -> Result<(), SbdfError> {
        self.write_object(value, false)
    }

    pub fn write_section_id(&mut self, section_id: SectionId) -> Result<(), SbdfError> {
        // Write the magic number followed by the section ID.
        self.write_byte(0xdfu8)?;
        self.write_byte(0x5bu8)?;
        self.write_byte(section_id as u8)?;
        Ok(())
    }

    pub fn write_file_header(&mut self, file_header: &FileHeader) -> Result<(), SbdfError> {
        self.write_byte(file_header.major_version)?;
        self.write_byte(file_header.minor_version)?;
        Ok(())
    }

    fn write_metadata_value(&mut self, value: Option<&Object>) -> Result<(), SbdfError> {
        // Currently we don't bother to check if the count is <= 1. Instead we assume this could
        // happen earlier in a builder where it can be checked.
        match value {
            None => self.write_byte(0),
            Some(value) => {
                self.write_byte(1)?;
                self.write_unpacked_object(value)
            }
        }
    }

    fn write_metadata(&mut self, metadata: &Metadata) -> Result<(), SbdfError> {
        self.write_string(&metadata.name, false)?;
        self.write_value_type(metadata.value.value_type())?;
        self.write_metadata_value(Some(&metadata.value))?;
        self.write_metadata_value(metadata.default_value.as_ref())?;

        Ok(())
    }

    pub fn write_table_metadata(
        &mut self,
        table_metadata: &TableMetadata,
    ) -> Result<(), SbdfError> {
        let table_metadata_count: i32 = table_metadata
            .metadata
            .len()
            .try_into()
            .map_err(|_| SbdfError::TooManyMetadata)?;
        self.write_int(table_metadata_count)?;

        for metadata in table_metadata.metadata.iter() {
            self.write_metadata(metadata)?;
        }

        let column_count: i32 = table_metadata
            .columns
            .len()
            .try_into()
            .map_err(|_| SbdfError::TooManyColumns)?;
        self.write_int(column_count)?;

        // Find unique metadata across all columns by (name, type, default value). We
        // enumerate here so that we can restore the original order of the metadata later, as done
        // in other implementations.
        let mut unique_metadata = table_metadata
            .columns
            .iter()
            .flat_map(|c| c.metadata_types())
            .enumerate()
            .collect::<Vec<_>>();

        // Sort and deduplicate metadata by name only. We assume that no two columns will have the
        // same name but different types or default values, which is consistent with how this is
        // done in other implementations.
        //
        // We include the original order so we'll keep the lowest index of any duplicates, which is
        // useful for restoring the original order later.
        unique_metadata.sort_unstable_by_key(
            |&(original_order, ColumnMetadataType { name, .. })| (name, original_order),
        );
        unique_metadata.dedup_by_key(|&mut (_, ColumnMetadataType { name, .. })| name);

        let unique_metadata_count: i32 = unique_metadata
            .len()
            .try_into()
            .map_err(|_| SbdfError::TooManyMetadata)?;
        self.write_int(unique_metadata_count)?;

        // Restore the original order of the metadata.
        unique_metadata.sort_unstable_by_key(|(original_order, _)| *original_order);

        for (
            _,
            ColumnMetadataType {
                name,
                ty,
                default_value,
            },
        ) in unique_metadata.iter()
        {
            self.write_string(name, false)?;
            self.write_value_type(*ty)?;
            self.write_metadata_value(*default_value)?;
        }

        for column in table_metadata.columns.iter() {
            for (_, ColumnMetadataType { name, .. }) in unique_metadata.iter() {
                match column.get(name) {
                    Some(value) => {
                        self.write_bool(true)?;

                        self.write_unpacked_object(value.as_ref())?;
                    }
                    None => {
                        self.write_bool(false)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn write_object_packed_array(&mut self, value: &Object) -> Result<(), SbdfError> {
        let count = match value {
            Object::Bool(_)
            | Object::Int(_)
            | Object::Long(_)
            | Object::Float(_)
            | Object::Double(_)
            | Object::DateTime(_)
            | Object::Date(_)
            | Object::Time(_)
            | Object::TimeSpan(_)
            | Object::String(_)
            | Object::Binary(_)
            | Object::Decimal(_) => 1usize,
            Object::BoolArray(BoolArray(a)) => a.len(),
            Object::IntArray(IntArray(a)) => a.len(),
            Object::LongArray(LongArray(a)) => a.len(),
            Object::FloatArray(FloatArray(a)) => a.len(),
            Object::DoubleArray(DoubleArray(a)) => a.len(),
            Object::DateTimeArray(DateTimeArray(a)) => a.len(),
            Object::DateArray(DateArray(a)) => a.len(),
            Object::TimeArray(TimeArray(a)) => a.len(),
            Object::TimeSpanArray(TimeSpanArray(a)) => a.len(),
            Object::StringArray(StringArray(a)) => a.len(),
            Object::BinaryArray(BinaryArray(a)) => a.len(),
            Object::DecimalArray(DecimalArray(a)) => a.len(),
        }
        .try_into()
        .map_err(|_| SbdfError::TooManyValuesInArray)?;
        self.write_int(count)?;

        self.write_object(value, true)
    }

    fn write_encoded_plain(&mut self, value: &Object) -> Result<(), SbdfError> {
        self.write_byte(ValueArrayEncoding::Plain as u8)?;
        self.write_value_type(value.value_type())?;
        self.write_object_packed_array(value)?;

        Ok(())
    }

    fn write_encoded_bit_array(&mut self, bit_array: &EncodedBitArray) -> Result<(), SbdfError> {
        self.write_byte(ValueArrayEncoding::BitArray as u8)?;
        self.write_value_type(ValueType::Bool)?;
        let bit_count: i32 = (bit_array.bit_count)
            .try_into()
            .map_err(|_| SbdfError::BytesTooLong)?;
        self.write_int(bit_count)?;
        self.write_bytes_without_length(&bit_array.bytes)?;

        Ok(())
    }

    fn write_value_array(&mut self, values: &EncodedValue) -> Result<(), SbdfError> {
        match values {
            EncodedValue::Plain(value) => self.write_encoded_plain(value),
            EncodedValue::RunLength(run_length) => {
                let EncodedRunLength {
                    repetitions,
                    values,
                } = run_length;

                self.write_byte(ValueArrayEncoding::RunLength as u8)?;
                self.write_value_type(values.value_type())?;

                let total_elements: i32 = run_length
                    .total_elements()?
                    .try_into()
                    .map_err(|_| SbdfError::TooManyValuesInArray)?;

                self.write_int(total_elements)?;
                self.write_bytes(&repetitions, false)?;
                self.write_object_packed_array(values)?;

                Ok(())
            }
            EncodedValue::BitArray(bit_array) => self.write_encoded_bit_array(bit_array),
        }
    }

    fn write_properties(&mut self, properties: &ColumnProperties) -> Result<(), SbdfError> {
        let mut count = 0;

        if properties.is_invalid.is_some() {
            count += 1;
        }

        if properties.error_code.is_some() {
            count += 1;
        }

        if properties.has_replaced_value.is_some() {
            count += 1;
        }

        count += properties.other.len();

        let count: i32 = count.try_into().map_err(|_| SbdfError::TooManyProperties)?;
        self.write_int(count)?;

        if let Some(bit_array) = &properties.is_invalid {
            self.write_string("IsInvalid", false)?;
            self.write_encoded_bit_array(bit_array)?;
        }

        if let Some(object) = &properties.error_code {
            self.write_string("ErrorCode", false)?;
            self.write_value_array(object)?;
        }

        if let Some(bit_array) = &properties.has_replaced_value {
            self.write_string("HasReplacedValue", false)?;
            self.write_encoded_bit_array(bit_array)?;
        }

        for property in properties.other.iter() {
            self.write_string(&property.name, false)?;
            self.write_value_array(&property.values)?;
        }

        Ok(())
    }

    fn write_column_slice(&mut self, column_slice: &ColumnSlice) -> Result<(), SbdfError> {
        self.write_section_id(SectionId::ColumnSlice)?;
        self.write_value_array(&column_slice.values)?;
        self.write_properties(&column_slice.properties)?;

        Ok(())
    }

    pub fn write_table_slice(&mut self, table_slice: &TableSlice) -> Result<(), SbdfError> {
        let column_count: i32 = table_slice
            .column_slices
            .len()
            .try_into()
            .map_err(|_| SbdfError::TooManyColumns)?;
        self.write_int(column_count)?;

        for column_slice in table_slice.column_slices.iter() {
            self.write_column_slice(column_slice)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_byte() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_byte(0x12).unwrap();
        writer.write_byte(0x34).unwrap();
        assert_eq!(buffer, [0x12, 0x34]);
    }

    #[test]
    fn write_7bit_packed_int() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_7bit_packed_int(1024).unwrap();
        writer.write_7bit_packed_int(1).unwrap();
        writer.write_7bit_packed_int(0).unwrap();
        assert_eq!(buffer, [0x80, 0x08, 0x01, 0]);
    }

    #[test]
    fn write_int() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_int(1024).unwrap();
        assert_eq!(buffer, [0x0, 0x4, 0x0, 0x0]);
    }

    #[test]
    fn write_long() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_long(1024 | 1024 << 32).unwrap();
        assert_eq!(buffer, [0x0, 0x4, 0x0, 0x0, 0x0, 0x4, 0x0, 0x0]);
    }

    #[test]
    fn write_float() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_float(123.456f32).unwrap();
        assert_eq!(buffer, 123.456f32.to_le_bytes());
    }

    #[test]
    fn write_double() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_double(123.456).unwrap();
        assert_eq!(buffer, 123.456f64.to_le_bytes());
    }

    #[test]
    fn write_string_unpacked() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_string("Hello, world!", false).unwrap();

        let mut expected_bytes = Vec::new();
        let text = b"Hello, world!";
        let length = (text.len() as i32).to_le_bytes();
        expected_bytes.extend_from_slice(&length);
        expected_bytes.extend_from_slice(text);

        assert_eq!(buffer, expected_bytes);
    }

    #[test]
    fn write_string_packed() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_string("Hello, world!", true).unwrap();

        let mut expected_bytes = Vec::new();
        let text = b"Hello, world!";
        // Length is short enough to fit into a single byte without a continuation bit.
        let length = text.len() as u8;
        expected_bytes.push(length);
        expected_bytes.extend_from_slice(text);

        assert_eq!(buffer, expected_bytes);
    }

    #[test]
    fn write_bool() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_bool(false).unwrap();
        writer.write_bool(true).unwrap();
        assert_eq!(buffer, [0, 1]);
    }

    #[test]
    fn write_bytes_unpacked() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_bytes(b"Hello, world!", false).unwrap();

        let mut expected_bytes = Vec::new();
        let text = b"Hello, world!";
        let length = (text.len() as i32).to_le_bytes();
        expected_bytes.extend_from_slice(&length);
        expected_bytes.extend_from_slice(text);

        assert_eq!(buffer, expected_bytes);
    }

    #[test]
    fn write_bytes_packed() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_bytes(b"Hello, world!", true).unwrap();

        let mut expected_bytes = Vec::new();
        let text = b"Hello, world!";
        // Length is short enough to fit into a single byte without a continuation bit.
        let length = text.len() as u8;
        expected_bytes.push(length);
        expected_bytes.extend_from_slice(text);

        assert_eq!(buffer, expected_bytes);
    }

    #[test]
    fn write_decimal() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_decimal(&[1; 16]).unwrap();
        assert_eq!(buffer, [1; 16]);
    }

    #[test]
    fn write_value_type() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_value_type(ValueType::TimeSpan).unwrap();
        writer.write_value_type(ValueType::String).unwrap();
        assert_eq!(buffer, [0x9, 0xa]);
    }

    #[test]
    fn write_section_id() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_section_id(SectionId::TableMetadata).unwrap();
        assert_eq!(buffer, [0xdf, 0x5b, 0x2]);
    }

    #[test]
    fn write_file_header() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer
            .write_file_header(&FileHeader {
                major_version: 1,
                minor_version: 0,
            })
            .unwrap();
        assert_eq!(buffer, [0x1, 0x0]);
    }
}
