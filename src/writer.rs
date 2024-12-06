use std::io::Cursor;

use crate::{Decimal, FileHeader, SbdfError, SectionId, ValueType};

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

    fn write_int(&mut self, value: i32) -> Result<(), SbdfError> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_long(&mut self, value: i64) -> Result<(), SbdfError> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_float(&mut self, value: f32) -> Result<(), SbdfError> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_double(&mut self, value: f64) -> Result<(), SbdfError> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_string(&mut self, value: &str) -> Result<(), SbdfError> {
        self.write_bytes(value.as_bytes())
    }

    fn write_bool(&mut self, value: bool) -> Result<(), SbdfError> {
        self.write_byte(if value { 1 } else { 0 })
    }

    fn write_bytes(&mut self, value: &[u8]) -> Result<(), SbdfError> {
        self.cursor.get_mut().extend_from_slice(value);
        Ok(())
    }

    fn write_decimal(&mut self, value: Decimal) -> Result<(), SbdfError> {
        self.write_bytes(&value)
    }

    fn write_value_type(&mut self, value: ValueType) -> Result<(), SbdfError> {
        self.write_byte(value as u8)
    }

    fn write_section_id(&mut self, section_id: SectionId) -> Result<(), SbdfError> {
        // Write the magic number followed by the section ID.
        self.write_byte(0xdfu8)?;
        self.write_byte(0x5bu8)?;
        self.write_byte(section_id as u8)?;
        Ok(())
    }

    fn write_file_header(&mut self, file_header: &FileHeader) -> Result<(), SbdfError> {
        self.write_byte(file_header.major_version)?;
        self.write_byte(file_header.minor_version)?;
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
    fn write_string() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_string("Hello, world!").unwrap();
        assert_eq!(buffer, "Hello, world!".as_bytes());
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
    fn write_bytes() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_bytes(b"Hello, world!").unwrap();
        assert_eq!(buffer, b"Hello, world!");
    }

    #[test]
    fn write_decimal() {
        let mut buffer = Vec::new();
        let mut writer = SbdfWriter::new(Cursor::new(&mut buffer));
        writer.write_decimal([1; 16]).unwrap();
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
