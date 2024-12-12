use std::{fs::File, io::Read};

use insta::assert_ron_snapshot;
use sbdf::{BoolArray, ColumnSlice, Sbdf};

#[test]
fn sample_with_null() {
    // Data table pasted from clipboard CSV containing:
    //
    // ```
    // StringColA,DateColB
    // ,2024/01/01
    // Abc,
    // Def,InvalidDateTest
    // ,
    // PreviousRowWasNull,2025/01/05
    // ```
    // with the first column selected as string, and the second column as date.
    //
    // We want to test the null handling for both columns using the "IsInvalid" property.

    let mut file = File::open("tests/samples/null.sbdf").unwrap();
    let mut in_bytes = Vec::new();
    file.read_to_end(&mut in_bytes).unwrap();
    let sbdf = Sbdf::from_bytes(&in_bytes).unwrap();

    insta::assert_ron_snapshot!(sbdf);

    let out_bytes = sbdf.to_bytes().unwrap();
    assert_eq!(in_bytes, out_bytes);

    let is_invalid_property = |col_idx| -> BoolArray {
        let column: &ColumnSlice = &sbdf.table_slices[0].column_slices[col_idx];
        let is_invalid_property = column.properties.is_invalid.as_ref().unwrap();
        is_invalid_property.decode().unwrap()
    };

    let string_is_invalid = is_invalid_property(0);
    assert_ron_snapshot!(string_is_invalid, @r#"
    BoolArray([
      true,
      false,
      false,
      true,
      false,
    ])
    "#);

    let date_is_invalid = is_invalid_property(1);
    assert_ron_snapshot!(date_is_invalid, @r#"
    BoolArray([
      false,
      true,
      true,
      true,
      false,
    ])
    "#);
}
