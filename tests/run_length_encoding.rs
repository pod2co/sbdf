use std::{fs::File, io::Read};

use sbdf::Sbdf;

#[test]
fn sample_with_run_length_encoding() {
    // Check the snapshot and that we can round-trip the run-length encoded arrays.

    let mut file = File::open("tests/samples/rle.sbdf").unwrap();
    let mut in_bytes = Vec::new();
    file.read_to_end(&mut in_bytes).unwrap();
    let sbdf = Sbdf::from_bytes(&in_bytes).unwrap();

    insta::assert_ron_snapshot!(sbdf);

    let out_bytes = sbdf.to_bytes().unwrap();
    assert_eq!(in_bytes, out_bytes);

    // Snapshot all decoded arrays.
    let decoded = sbdf.table_slices[0]
        .column_slices
        .iter()
        .map(|column| column.load_values().unwrap())
        .collect::<Vec<_>>();

    insta::assert_ron_snapshot!(decoded);
}
