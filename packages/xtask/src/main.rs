use xtask_base::{
    CommonCmds,
    ci::{CI, StandardVersions},
    generate_open_source_files,
};

fn main() {
    CommonCmds::run(
        CI::standard_workflow(
            StandardVersions {
                rustc_stable_version: "1.88.0",
                rustc_nightly_version: "nightly-2025-06-28",
                udeps_version: "0.1.56",
            },
            &[],
        ),
        |check| generate_open_source_files(2023, check),
    )
}
