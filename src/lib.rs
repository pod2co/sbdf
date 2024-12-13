use reader::SbdfReader;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, io::Cursor, iter};
use thiserror::Error;
use writer::SbdfWriter;

pub(crate) mod reader;
pub(crate) mod writer;

pub const COLUMN_METADATA_NAME: &str = "Name";
pub const COLUMN_METADATA_TYPE: &str = "DataType";
pub const PROPERTY_IS_INVALID: &str = "IsInvalid";
pub const PROPERTY_ERROR_CODE: &str = "ErrorCode";
pub const PROPERTY_HAS_REPLACED_VALUE: &str = "HasReplacedValue";

pub(crate) const BITS_PER_BYTE: usize = 8;

/// Specified by Spotfire as a minimum date/time of 1583/01/01 00:00:00.
pub const MIN_DATE_MILLISECONDS: i64 = 49923043200000i64;

#[derive(Error, Debug)]
pub enum SbdfError {
    #[error("invalid bytes")]
    InvalidBytes,
    #[error("invalid int")]
    InvalidInt,
    #[error("invalid long")]
    InvalidLong,
    #[error("invalid float")]
    InvalidFloat,
    #[error("invalid double")]
    InvalidDouble,
    #[error("invalid string")]
    InvalidString,
    #[error("invalid bool")]
    InvalidBool,
    #[error("invalid section id {section_id}")]
    InvalidSectionId { section_id: u8 },
    #[error("wrong section id, expected {expected:?}, got {actual:?}")]
    WrongSectionId {
        expected: SectionId,
        actual: SectionId,
    },
    #[error("magic number mismatch")]
    MagicNumberMismatch,
    #[error("unsupported version {major_version}.{minor_version}")]
    UnsupportedVersion {
        major_version: u8,
        minor_version: u8,
    },
    #[error("invalid size")]
    InvalidSize,
    #[error("metadata value array length must be zero or one")]
    MetadataValueArrayLengthMustBeZeroOrOne,
    #[error("invalid value type")]
    InvalidValueType,
    #[error("unknown type id")]
    UnknownTypeId,
    #[error("invalid object")]
    InvalidObject,
    #[error("invalid metadata")]
    InvalidMetadata,
    #[error("column count mismatch")]
    ColumnCountMismatch,
    #[error("invalid encoding")]
    InvalidEncoding,
    #[error("string too long")]
    StringTooLong,
    #[error("bytes too long")]
    BytesTooLong,
    #[error("too many columns")]
    TooManyColumns,
    #[error("too many metadata")]
    TooManyMetadata,
    #[error("too many properties")]
    TooManyProperties,
    #[error("too many values in array")]
    TooManyValuesInArray,
    #[error("invalid run-length encoded object")]
    InvalidRunLengthEncodedObject,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum SectionId {
    FileHeader = 0x1,
    TableMetadata = 0x2,
    TableSlice = 0x3,
    ColumnSlice = 0x4,
    TableEnd = 0x5,
}

impl TryFrom<u8> for SectionId {
    type Error = SbdfError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x1 => Ok(Self::FileHeader),
            0x2 => Ok(Self::TableMetadata),
            0x3 => Ok(Self::TableSlice),
            0x4 => Ok(Self::ColumnSlice),
            0x5 => Ok(Self::TableEnd),
            section_id => Err(SbdfError::InvalidSectionId { section_id }),
        }
    }
}

/// The SBDF file format.
#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct Sbdf {
    pub file_header: FileHeader,
    pub table_metadata: TableMetadata,
    pub table_slices: Box<[TableSlice]>,
}

impl Sbdf {
    /// Parse an SBDF file from bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Sbdf, SbdfError> {
        let mut reader = SbdfReader::new(bytes);

        reader.expect_section_id(SectionId::FileHeader)?;
        let file_header = reader.read_file_header()?;

        reader.expect_section_id(SectionId::TableMetadata)?;
        let table_metadata = reader.read_table_metadata()?;

        let mut table_slices = Vec::new();

        loop {
            match reader.read_section_id() {
                Ok(SectionId::TableSlice) => {
                    let table_slice = reader.read_table_slice(&table_metadata)?;
                    table_slices.push(table_slice);
                }
                Ok(SectionId::TableEnd) => break,
                Err(err) => return Err(err),
                Ok(section_id) => {
                    return Err(SbdfError::WrongSectionId {
                        expected: SectionId::TableSlice,
                        actual: section_id,
                    })
                }
            }
        }

        Ok(Sbdf {
            file_header,
            table_metadata,
            table_slices: table_slices.into_boxed_slice(),
        })
    }

    /// Convert the SBDF file to bytes.
    pub fn to_bytes(&self) -> Result<Vec<u8>, SbdfError> {
        let mut bytes = Vec::new();
        let cursor = Cursor::new(&mut bytes);

        let mut writer = SbdfWriter::new(cursor);

        writer.write_section_id(SectionId::FileHeader)?;
        writer.write_file_header(&self.file_header)?;

        writer.write_section_id(SectionId::TableMetadata)?;
        writer.write_table_metadata(&self.table_metadata)?;

        for table_slice in self.table_slices.iter() {
            writer.write_section_id(SectionId::TableSlice)?;
            writer.write_table_slice(table_slice)?;
        }

        writer.write_section_id(SectionId::TableEnd)?;

        Ok(bytes)
    }
}

/// The file header contains the version of the file format.
#[derive(Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct FileHeader {
    pub major_version: u8,
    pub minor_version: u8,
}

/// The table metadata contains the metadata for the table and its columns.
#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct TableMetadata {
    pub metadata: Box<[Metadata]>,
    pub columns: Box<[ColumnMetadata]>,
}

impl TableMetadata {
    pub fn metadata(&self) -> &[Metadata] {
        &self.metadata
    }

    pub fn columns(&self) -> &[ColumnMetadata] {
        &self.columns
    }
}

pub struct ColumnMetadataType<'a> {
    pub name: &'a str,
    pub ty: ValueType,
    pub default_value: Option<&'a Object>,
}

/// The metadata for a column.
// Even though name and type are considered plain metadata, in practice they're always expected to
// exist, so split them out here for faster access.
#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct ColumnMetadata {
    pub name: String,
    pub ty: ValueType,
    pub other: Box<[Metadata]>,
}

impl ColumnMetadata {
    fn get<'a>(&'a self, key: &str) -> Option<Cow<'a, Object>> {
        match key {
            COLUMN_METADATA_NAME => Some(Cow::Owned(Object::String(self.name.clone()))),
            COLUMN_METADATA_TYPE => Some(Cow::Owned(Object::Binary(Box::new([self.ty as u8])))),
            _ => self
                .other
                .iter()
                .find(|metadata| metadata.name == key)
                .map(|metadata| Cow::Borrowed(&metadata.value)),
        }
    }

    fn metadata_types(&self) -> impl Iterator<Item = ColumnMetadataType<'_>> + '_ {
        // Always add in name and type metadata. These will be skipped for columns because they're
        // not included into `other`.
        [
            ColumnMetadataType {
                name: COLUMN_METADATA_NAME,
                ty: ValueType::String,
                default_value: None,
            },
            ColumnMetadataType {
                name: COLUMN_METADATA_TYPE,
                ty: ValueType::Binary,
                default_value: None,
            },
        ]
        .into_iter()
        .chain(self.other.iter().map(|metadata| ColumnMetadataType {
            name: &metadata.name,
            ty: metadata.value.value_type(),
            default_value: metadata.default_value.as_ref(),
        }))
    }
}

/// The metadata for a column.
#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct Metadata {
    pub name: String,
    pub value: Object,
    pub default_value: Option<Object>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[repr(u8)]
pub enum ValueType {
    Bool = 0x1,
    Int = 0x2,
    Long = 0x3,
    Float = 0x4,
    Double = 0x5,
    DateTime = 0x6,
    Date = 0x7,
    Time = 0x8,
    TimeSpan = 0x9,
    String = 0xa,
    Binary = 0xc,
    Decimal = 0xd,
}

impl ValueType {
    fn default_object(&self) -> Result<Object, SbdfError> {
        match self {
            Self::Bool => Ok(Object::Bool(false)),
            Self::Int => Ok(Object::Int(0)),
            Self::Long => Ok(Object::Long(0)),
            Self::Float => Ok(Object::Float(0.0)),
            Self::Double => Ok(Object::Double(0.0)),
            Self::DateTime => Ok(Object::DateTime(DateTime(0))),
            Self::Date => Ok(Object::Date(Date(0))),
            Self::Time => Ok(Object::Time(0)),
            Self::TimeSpan => Ok(Object::TimeSpan(0)),
            Self::String => Ok(Object::String(String::new())),
            Self::Binary => Ok(Object::Binary(Box::new([]))),
            Self::Decimal => Ok(Object::Decimal([0; 16])),
        }
    }
}

impl TryFrom<u8> for ValueType {
    type Error = SbdfError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x1 => Ok(Self::Bool),
            0x2 => Ok(Self::Int),
            0x3 => Ok(Self::Long),
            0x4 => Ok(Self::Float),
            0x5 => Ok(Self::Double),
            0x6 => Ok(Self::DateTime),
            0x7 => Ok(Self::Date),
            0x8 => Ok(Self::Time),
            0x9 => Ok(Self::TimeSpan),
            0xa => Ok(Self::String),
            0xc => Ok(Self::Binary),
            0xd => Ok(Self::Decimal),
            _ => Err(SbdfError::InvalidValueType),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct BoolArray(pub Box<[bool]>);

impl BoolArray {
    pub fn encode_to_bit_array(&self) -> EncodedBitArray {
        let mut bytes = Vec::with_capacity(self.0.len().div_ceil(BITS_PER_BYTE));

        // Write the bits in the most significant bit first.
        for chunk in self.0.chunks(BITS_PER_BYTE) {
            let mut byte = 0;

            for (i, &bit) in chunk.iter().enumerate() {
                if bit {
                    byte |= 0x80 >> i;
                }
            }

            bytes.push(byte);
        }

        EncodedBitArray {
            bit_count: self.0.len(),
            bytes: bytes.into_boxed_slice(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct IntArray(pub Box<[i32]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct LongArray(pub Box<[i64]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct FloatArray(pub Box<[f32]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct DoubleArray(pub Box<[f64]>);

/// Milliseconds since 01/01/01 00:00:00.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct DateTime(i64);

impl DateTime {
    pub const MIN: DateTime = DateTime(MIN_DATE_MILLISECONDS);
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct DateTimeArray(pub Box<[i64]>);

/// Milliseconds since 01/01/01 00:00:00.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct Date(i64);

impl Date {
    pub const MIN: Date = Date(MIN_DATE_MILLISECONDS);
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct DateArray(pub Box<[i64]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct TimeArray(pub Box<[i64]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct TimeSpanArray(pub Box<[i64]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct BinaryArray(pub Box<[Box<[u8]>]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct StringArray(pub Box<[String]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[repr(transparent)]
pub struct DecimalArray(pub Box<[Decimal]>);

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum Object {
    Bool(bool),
    BoolArray(BoolArray),
    Int(i32),
    IntArray(IntArray),
    Long(i64),
    LongArray(LongArray),
    Float(f32),
    FloatArray(FloatArray),
    Double(f64),
    DoubleArray(DoubleArray),
    DateTime(DateTime),
    DateTimeArray(DateTimeArray),
    Date(Date),
    DateArray(DateArray),
    Time(i64),
    TimeArray(TimeArray),
    /// Milliseconds.
    TimeSpan(i64),
    TimeSpanArray(TimeSpanArray),
    String(String),
    StringArray(StringArray),
    Binary(Box<[u8]>),
    BinaryArray(BinaryArray),
    Decimal(Decimal),
    DecimalArray(DecimalArray),
}

impl Object {
    fn value_type(&self) -> ValueType {
        match self {
            Self::Bool(_) | Self::BoolArray(_) => ValueType::Bool,
            Self::Int(_) | Self::IntArray(_) => ValueType::Int,
            Self::Long(_) | Self::LongArray(_) => ValueType::Long,
            Self::Float(_) | Self::FloatArray(_) => ValueType::Float,
            Self::Double(_) | Self::DoubleArray(_) => ValueType::Double,
            Self::DateTime(_) | Self::DateTimeArray(_) => ValueType::DateTime,
            Self::Date(_) | Self::DateArray(_) => ValueType::Date,
            Self::Time(_) | Self::TimeArray(_) => ValueType::Time,
            Self::TimeSpan(_) | Self::TimeSpanArray(_) => ValueType::TimeSpan,
            Self::String(_) | Self::StringArray(_) => ValueType::String,
            Self::Binary(_) | Self::BinaryArray(_) => ValueType::Binary,
            Self::Decimal(_) | Self::DecimalArray(_) => ValueType::Decimal,
        }
    }
}

/// IEEE754 128-bit decimal.
pub type Decimal = [u8; 16];

#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct TableSlice {
    pub column_slices: Box<[ColumnSlice]>,
}

#[derive(Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct ColumnProperties {
    /// The standard "IsInvalid" property used to mark invalid or null values.
    pub is_invalid: Option<EncodedBitArray>,
    /// The standard "ErrorCode" property.
    pub error_code: Option<EncodedValue>,
    /// The standard "HasReplacedValue" property.
    pub has_replaced_value: Option<EncodedBitArray>,
    pub other: Box<[Property]>,
}

#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct ColumnSlice {
    pub values: EncodedValue,
    pub properties: ColumnProperties,
}

impl ColumnSlice {
    pub fn load_values<'a>(&'a self) -> Result<Cow<'a, Object>, SbdfError> {
        self.values.decode()
    }
}

/// A property is a named value associated with a column slice.
#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct Property {
    pub name: String,
    pub values: EncodedValue,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[repr(u8)]
pub enum ValueArrayEncoding {
    Plain = 1,
    RunLength = 2,
    BitArray = 3,
}

impl TryFrom<u8> for ValueArrayEncoding {
    type Error = SbdfError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Plain),
            2 => Ok(Self::RunLength),
            3 => Ok(Self::BitArray),
            _ => Err(SbdfError::InvalidEncoding),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct EncodedRunLength {
    /// The additional repetitions to apply to each value at the same index. If the value
    /// should only be included once (i.e., the run length is 1), the repetitions should be 0.
    /// If the value should be included twice (run length of 2), the repetitions should be 1,
    /// and so on.
    repetitions: Box<[u8]>,
    values: Object,
}

impl EncodedRunLength {
    /// Count the total number of elements in the run-length encoded array after decoding.
    pub fn total_elements(&self) -> Result<usize, SbdfError> {
        let mut item_count = 0usize;

        for run_length in self.repetitions.iter() {
            // The value is always included at least once, followed by the additional
            // repetitions.
            item_count = item_count
                .checked_add(1)
                .and_then(|x| x.checked_add(*run_length as usize))
                .ok_or(SbdfError::TooManyValuesInArray)?;
        }

        Ok(item_count)
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum EncodedValue {
    Plain { value: Object },
    RunLength(EncodedRunLength),
    BitArray(EncodedBitArray),
}

impl EncodedValue {
    pub fn decode(&self) -> Result<Cow<'_, Object>, SbdfError> {
        match self {
            EncodedValue::Plain { value } => {
                // Already unpacked, so just return a borrowed reference.
                Ok(Cow::Borrowed(value))
            }
            EncodedValue::RunLength(run_length) => {
                fn repeat_value<T>(
                    run_length: &EncodedRunLength,
                    encoded_values: &[T],
                ) -> Result<Box<[T]>, SbdfError>
                where
                    T: Clone,
                {
                    let mut values: Vec<T> = Vec::with_capacity(run_length.total_elements()?);
                    let repetitions = &run_length.repetitions;

                    for (repetitions_for_encoded_value, encoded_value) in
                        repetitions.iter().zip(encoded_values.iter())
                    {
                        // The value plus however many additional times it should be repeated.
                        let total_times_to_include =
                            1usize + *repetitions_for_encoded_value as usize;
                        values.extend(
                            iter::repeat_with(|| encoded_value.clone())
                                .take(total_times_to_include),
                        );
                    }

                    Ok(values.into_boxed_slice())
                }

                Ok(Cow::Owned(match &run_length.values {
                    Object::BoolArray(BoolArray(bool_array)) => {
                        Object::BoolArray(BoolArray(repeat_value(run_length, bool_array)?))
                    }
                    Object::IntArray(IntArray(int_array)) => {
                        Object::IntArray(IntArray(repeat_value(run_length, int_array)?))
                    }
                    Object::LongArray(LongArray(long_array)) => {
                        Object::LongArray(LongArray(repeat_value(run_length, long_array)?))
                    }
                    Object::FloatArray(FloatArray(float_array)) => {
                        Object::FloatArray(FloatArray(repeat_value(run_length, float_array)?))
                    }
                    Object::DoubleArray(DoubleArray(double_array)) => {
                        Object::DoubleArray(DoubleArray(repeat_value(run_length, double_array)?))
                    }
                    Object::DateTimeArray(DateTimeArray(date_time_array)) => Object::DateTimeArray(
                        DateTimeArray(repeat_value(run_length, date_time_array)?),
                    ),
                    Object::DateArray(DateArray(date_array)) => {
                        Object::DateArray(DateArray(repeat_value(run_length, date_array)?))
                    }
                    Object::TimeArray(TimeArray(time_array)) => {
                        Object::TimeArray(TimeArray(repeat_value(run_length, time_array)?))
                    }
                    Object::TimeSpanArray(TimeSpanArray(time_span_array)) => Object::TimeSpanArray(
                        TimeSpanArray(repeat_value(run_length, time_span_array)?),
                    ),
                    Object::StringArray(StringArray(string_array)) => {
                        Object::StringArray(StringArray(repeat_value(run_length, string_array)?))
                    }
                    Object::BinaryArray(BinaryArray(binary_array)) => {
                        Object::BinaryArray(BinaryArray(repeat_value(run_length, binary_array)?))
                    }
                    Object::DecimalArray(DecimalArray(decimal_array)) => {
                        Object::DecimalArray(DecimalArray(repeat_value(run_length, decimal_array)?))
                    }
                    // It's possible we might want to handle this by repeating the single value for
                    // each repetition, but it's not clear this is ever used in practice. For now we
                    // always expect the values to be arrays.
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
                    | Object::Decimal(_) => return Err(SbdfError::InvalidRunLengthEncodedObject),
                }))
            }
            EncodedValue::BitArray(bit_array) => {
                Ok(Cow::Owned(Object::BoolArray(bit_array.decode()?)))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct EncodedBitArray {
    pub bit_count: usize,
    pub bytes: Box<[u8]>,
}

impl EncodedBitArray {
    pub fn decode(&self) -> Result<BoolArray, SbdfError> {
        let EncodedBitArray { bit_count, bytes } = self;

        let mut values = Vec::with_capacity(bytes.len() * size_of::<u8>());

        for byte in bytes.iter() {
            for i in 0..8 {
                // Read the most significant bit first.
                let bit = (byte << i) & 0x80;
                values.push(bit != 0);
            }
        }

        // Trim the values to the actual bit count.
        values.truncate(*bit_count);

        Ok(BoolArray(values.into_boxed_slice()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_bool_to_bit_array() {
        let bool_array = BoolArray(
            vec![
                false, false, true, false, false, true, true, false, true, false, false, false,
                true, false, true, false, false, true, true,
            ]
            .into_boxed_slice(),
        );
        let bit_array = bool_array.encode_to_bit_array();
        let decoded = bit_array.decode().unwrap();

        assert_eq!(decoded, bool_array);
    }
}
