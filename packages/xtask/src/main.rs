use xtask_base::{
    CommonCmds,
    ci::{CI, StandardVersions},
    generate_open_source_files,
};

fn main() {
    CommonCmds::run(
        CI::standard_workflow(
            StandardVersions {
                rustc_stable_version: "1.85.1",
                rustc_nightly_version: "nightly-2025-03-15",
                udeps_version: "0.1.55",
            },
            &[],
        ),
        |check| generate_open_source_files(2023, check),
    )
}
