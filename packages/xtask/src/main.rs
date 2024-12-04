use xtask_base::{
    ci::{StandardVersions, CI},
    generate_open_source_files, CommonCmds,
};

fn main() {
    CommonCmds::run(
        CI::standard_workflow(
            StandardVersions {
                rustc_stable_version: "1.83.0",
                rustc_nightly_version: "nightly-2024-11-26",
                udeps_version: "0.1.53",
            },
            &[],
        ),
        |check| generate_open_source_files(2023, check),
    )
}
