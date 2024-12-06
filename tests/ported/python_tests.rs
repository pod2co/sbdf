use sbdf::Sbdf;
use std::{fs::File, io::Read};

fn read_file(file_name: &str) -> Sbdf {
    let mut file = File::open(file_name).unwrap();
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes).unwrap();
    Sbdf::from_bytes(&bytes).unwrap()
}

/// Ported from test_sbdf.py `test_read_0`
#[test]
fn read_0() {
    let sbdf = read_file("tests/ported/spotfire-sbdf-c/tests/samples/0.sbdf");
    insta::assert_ron_snapshot!(sbdf);
}

/// Ported from test_sbdf.py `test_read_1`
#[test]
fn read_1() {
    let sbdf = read_file("tests/ported/spotfire-sbdf-c/tests/samples/1.sbdf");
    insta::assert_ron_snapshot!(sbdf);
}
