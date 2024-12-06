use reader::SbdfReader;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use thiserror::Error;

pub(crate) mod reader;
pub(crate) mod writer;

const COLUMN_METADATA_NAME: &'static str = "Name";
const COLUMN_METADATA_TYPE: &'static str = "DataType";

#[derive(Error, Debug)]
pub enum SbdfError {
    #[error("invalid byte")]
    InvalidByte,
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

#[derive(Debug, Deserialize, Serialize)]
pub struct Sbdf {
    file_header: FileHeader,
    table_metadata: TableMetadata,
    table_slices: Box<[TableSlice]>,
}

impl Sbdf {
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

    pub fn table_slices(&self) -> &[TableSlice] {
        &self.table_slices
    }
}

#[derive(Debug, Eq, PartialEq, Deserialize, Serialize)]
struct FileHeader {
    major_version: u8,
    minor_version: u8,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct TableMetadata {
    metadata: Box<[Metadata]>,
    columns: Box<[ColumnMetadata]>,
}

impl TableMetadata {
    pub fn metadata(&self) -> &[Metadata] {
        &self.metadata
    }

    pub fn columns(&self) -> &[ColumnMetadata] {
        &self.columns
    }
}

// Even though name and type are considered plain metadata, in practice they're always expected to
// exist, so split them out here for faster access.
#[derive(Debug, Deserialize, Serialize)]
pub struct ColumnMetadata {
    pub name: String,
    pub ty: ValueType,
    pub other: Box<[Metadata]>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Metadata {
    pub name: String,
    pub value: Object,
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
            Self::DateTime => Ok(Object::DateTime(0)),
            Self::Date => Ok(Object::Date(0)),
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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Object {
    Bool(bool),
    BoolArray(Box<[bool]>),
    Int(i32),
    IntArray(Box<[i32]>),
    Long(i64),
    LongArray(Box<[i64]>),
    Float(f32),
    FloatArray(Box<[f32]>),
    Double(f64),
    DoubleArray(Box<[f64]>),
    /// Milliseconds since 01/01/01 00:00:00.
    DateTime(i64),
    DateTimeArray(Box<[i64]>),
    /// Milliseconds since 01/01/01 00:00:00.
    Date(i64),
    DateArray(Box<[i64]>),
    /// Milliseconds since 01/01/01 00:00:00.
    Time(i64),
    TimeArray(Box<[i64]>),
    /// Milliseconds.
    TimeSpan(i64),
    TimeSpanArray(Vec<i64>),
    String(String),
    StringArray(Box<[String]>),
    Binary(Box<[u8]>),
    BinaryArray(Box<[Box<[u8]>]>),
    Decimal(Decimal),
    DecimalArray(Vec<Decimal>),
}

/// IEEE754 128-bit decimal.
pub type Decimal = [u8; 16];

#[derive(Debug, Deserialize, Serialize)]
pub struct TableSlice {
    column_slices: Box<[ColumnSlice]>,
}

impl TableSlice {
    pub fn columns(&self) -> &[ColumnSlice] {
        &self.column_slices
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ColumnSlice {
    pub values: EncodedValue,
    pub properties: Box<[Property]>,
}

impl ColumnSlice {
    pub fn load_values<'a>(&'a self) -> Result<Cow<'a, Object>, SbdfError> {
        match &self.values {
            EncodedValue::Plain { value } => {
                // Already unpacked, so just return a borrowed reference.
                Ok(Cow::Borrowed(value))
            }
            EncodedValue::RunLength { .. } => {
                // TODO: It seems like these should all be array types, but it's not clear where
                // that's enforced right now.
                unimplemented!("run length loading")
            }
            EncodedValue::BitArray { bit_count, bytes } => {
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

                Ok(Cow::Owned(Object::BoolArray(values.into_boxed_slice())))
            }
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Property {
    name: String,
    values: EncodedValue,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[repr(u8)]
enum ValueArrayEncoding {
    Plain = 0x1,
    RunLength = 0x2,
    BitArray = 0x3,
}

impl TryFrom<u8> for ValueArrayEncoding {
    type Error = SbdfError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x1 => Ok(Self::Plain),
            0x2 => Ok(Self::RunLength),
            0x3 => Ok(Self::BitArray),
            _ => Err(SbdfError::InvalidEncoding),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum EncodedValue {
    Plain {
        value: Object,
    },
    RunLength {
        run_lengths: Box<[u8]>,
        values: Object,
    },
    BitArray {
        bit_count: usize,
        bytes: Box<[u8]>,
    },
}
