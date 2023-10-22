use xtask_base::{ci::CI, generate_open_source_files, CommonCmds};

fn main() {
    CommonCmds::run(CI::standard_workflow(&[]), |check| {
        generate_open_source_files(2023, check)
    })
}
