use xtask_base::{
    ci::{StandardVersions, CI},
    generate_open_source_files, CommonCmds,
};

fn main() {
    CommonCmds::run(
        CI::standard_workflow(StandardVersions::default(), &[]),
        |check| generate_open_source_files(2023, check),
    )
}
