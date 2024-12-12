use sbdf::{BoolArray, ColumnProperties, ColumnSlice, EncodedBitArray, EncodedValue, Object, Sbdf};
use std::{fs::File, io::Read};

fn read_file(file_name: &str) -> Sbdf {
    let mut file = File::open(file_name).unwrap();
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes).unwrap();
    Sbdf::from_bytes(&bytes).unwrap()
}

macro_rules! test_snapshot {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let sbdf = read_file($file);
            insta::assert_ron_snapshot!(sbdf);
        }
    };
}

test_snapshot!(
    sample_0_binary,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Binary.sbdf"
);
test_snapshot!(
    sample_0_boolean,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Boolean.sbdf"
);
test_snapshot!(
    sample_0_date,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Date.sbdf"
);
test_snapshot!(
    sample_0_datetime,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_DateTime.sbdf"
);
test_snapshot!(
    sample_0_decimal,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Decimal.sbdf"
);
test_snapshot!(
    sample_0_double,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Double.sbdf"
);
test_snapshot!(
    sample_0_float,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Float.sbdf"
);
test_snapshot!(
    sample_0_integer,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Integer.sbdf"
);
test_snapshot!(
    sample_0_long,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Long.sbdf"
);
test_snapshot!(
    sample_0_string,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_String.sbdf"
);
test_snapshot!(
    sample_0_time,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_Time.sbdf"
);
test_snapshot!(
    sample_0_timespan,
    "tests/ported/spotfire-sbdf-c/tests/samples/0_TimeSpan.sbdf"
);
test_snapshot!(
    sample_0,
    "tests/ported/spotfire-sbdf-c/tests/samples/0.sbdf"
);
test_snapshot!(
    sample_1_binary,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Binary.sbdf"
);
test_snapshot!(
    sample_1_boolean,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Boolean.sbdf"
);
test_snapshot!(
    sample_1_date,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Date.sbdf"
);
test_snapshot!(
    sample_1_datetime,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_DateTime.sbdf"
);
test_snapshot!(
    sample_1_decimal,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Decimal.sbdf"
);
test_snapshot!(
    sample_1_double,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Double.sbdf"
);
test_snapshot!(
    sample_1_float,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Float.sbdf"
);
test_snapshot!(
    sample_1_integer,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Integer.sbdf"
);
test_snapshot!(
    sample_1_long,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Long.sbdf"
);
test_snapshot!(
    sample_1_string,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_String.sbdf"
);
test_snapshot!(
    sample_1_time,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_Time.sbdf"
);
test_snapshot!(
    sample_1_timespan,
    "tests/ported/spotfire-sbdf-c/tests/samples/1_TimeSpan.sbdf"
);
test_snapshot!(
    sample_1,
    "tests/ported/spotfire-sbdf-c/tests/samples/1.sbdf"
);
test_snapshot!(
    sample_100_binary,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Binary.sbdf"
);
test_snapshot!(
    sample_100_boolean,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Boolean.sbdf"
);
test_snapshot!(
    sample_100_date,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Date.sbdf"
);
test_snapshot!(
    sample_100_datetime,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_DateTime.sbdf"
);
test_snapshot!(
    sample_100_decimal,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Decimal.sbdf"
);
test_snapshot!(
    sample_100_double,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Double.sbdf"
);
test_snapshot!(
    sample_100_float,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Float.sbdf"
);
test_snapshot!(
    sample_100_integer,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Integer.sbdf"
);
test_snapshot!(
    sample_100_long,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Long.sbdf"
);
test_snapshot!(
    sample_100_string,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_String.sbdf"
);
test_snapshot!(
    sample_100_time,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_Time.sbdf"
);
test_snapshot!(
    sample_100_timespan,
    "tests/ported/spotfire-sbdf-c/tests/samples/100_TimeSpan.sbdf"
);
test_snapshot!(
    sample_100,
    "tests/ported/spotfire-sbdf-c/tests/samples/100.sbdf"
);
test_snapshot!(
    sample_no_columns_binary,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Binary.sbdf"
);
test_snapshot!(
    sample_no_columns_boolean,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Boolean.sbdf"
);
test_snapshot!(
    sample_no_columns_date,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Date.sbdf"
);
test_snapshot!(
    sample_no_columns_datetime,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_DateTime.sbdf"
);
test_snapshot!(
    sample_no_columns_decimal,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Decimal.sbdf"
);
test_snapshot!(
    sample_no_columns_double,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Double.sbdf"
);
test_snapshot!(
    sample_no_columns_float,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Float.sbdf"
);
test_snapshot!(
    sample_no_columns_integer,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Integer.sbdf"
);
test_snapshot!(
    sample_no_columns_long,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Long.sbdf"
);
test_snapshot!(
    sample_no_columns_string,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_String.sbdf"
);
test_snapshot!(
    sample_no_columns_time,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_Time.sbdf"
);
test_snapshot!(
    sample_no_columns_timespan,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns_TimeSpan.sbdf"
);
test_snapshot!(
    sample_no_columns,
    "tests/ported/spotfire-sbdf-c/tests/samples/no_columns.sbdf"
);
test_snapshot!(
    root_2x4_cs,
    "tests/ported/spotfire-sbdf-c/tests/2x4_cs.sbdf"
);
test_snapshot!(
    root_3x3_cs,
    "tests/ported/spotfire-sbdf-c/tests/2x4_cs.sbdf"
);

/// We don't want to snapshot certain samples because they are too large.
///
/// Instead we'll just make sure we can read the file without an error.
macro_rules! test_no_snapshot {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let _sbdf = read_file($file);
        }
    };
}
test_no_snapshot!(
    sample_10001_binary,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Binary.sbdf"
);
test_no_snapshot!(
    sample_10001_boolean,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Boolean.sbdf"
);
test_no_snapshot!(
    sample_10001_date,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Date.sbdf"
);
test_no_snapshot!(
    sample_10001_datetime,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_DateTime.sbdf"
);
test_no_snapshot!(
    sample_10001_decimal,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Decimal.sbdf"
);
test_no_snapshot!(
    sample_10001_double,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Double.sbdf"
);
test_no_snapshot!(
    sample_10001_float,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Float.sbdf"
);
test_no_snapshot!(
    sample_10001_integer,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Integer.sbdf"
);
test_no_snapshot!(
    sample_10001_long,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Long.sbdf"
);
test_no_snapshot!(
    sample_10001_string,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_String.sbdf"
);
test_no_snapshot!(
    sample_10001_time,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_Time.sbdf"
);
test_no_snapshot!(
    sample_10001_timespan,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001_TimeSpan.sbdf"
);
test_no_snapshot!(
    sample_10001,
    "tests/ported/spotfire-sbdf-c/tests/samples/10001.sbdf"
);

#[test]
fn loads_values() {
    let sbdf = read_file("tests/ported/spotfire-sbdf-c/tests/3x3_cs.sbdf");
    let all_values = sbdf
        .table_slices
        .iter()
        .map(|table_slice| {
            table_slice
                .column_slices
                .iter()
                .map(|column| column.load_values().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    insta::assert_ron_snapshot!((&sbdf, all_values));
}

#[test]
fn decodes_bit_arrays() {
    let test1 = (
        EncodedValue::BitArray(EncodedBitArray {
            bit_count: 8,
            bytes: vec![38].into_boxed_slice(),
        }),
        vec![false, false, true, false, false, true, true, false].into_boxed_slice(),
    );
    let test2 = (
        EncodedValue::BitArray(EncodedBitArray {
            bit_count: 10,
            bytes: vec![147, 64].into_boxed_slice(),
        }),
        vec![
            true, false, false, true, false, false, true, true, false, true,
        ]
        .into_boxed_slice(),
    );
    let cases = vec![test1, test2];

    for (values, expected) in cases {
        let slice = ColumnSlice {
            values,
            properties: ColumnProperties::default(),
        };

        let result = slice.load_values();

        match result.unwrap().as_ref() {
            Object::BoolArray(BoolArray(ba)) => {
                assert_eq!(ba, &expected);
            }
            _ => panic!("expected bool array"),
        }
    }
}
