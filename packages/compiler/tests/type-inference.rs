use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parser::{expr, Expr},
};
use bandit_types::{context::Context, Pretty};
use winnow::Parser;

#[test]
fn one() {
    let int_type = Expr::variable("Int");
    let mut global_items = HashMap::new();
    global_items.insert("one", int_type.to_infer().unwrap());
    global_items.insert("Int", Expr::type_of_type().to_infer().unwrap());
    let ctx = &mut Context::new(global_items);

    let tokens: Vec<SrcToken> = Token::layout("one").collect();
    let mut expr = expr.parse(&tokens).unwrap().to_infer().unwrap();
    expr.normalize(ctx).unwrap();
    assert_eq!(expr.to_pretty_string(80), "(one : Int)");
}
