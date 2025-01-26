use std::collections::HashMap;

use bandit_parser::{
    lex::{SrcToken, Token},
    parser::{expr, Expr},
};
use bandit_types::{context::Context, Pretty};
use winnow::Parser;

fn test_with_ctx(input: &str, expected: &str) {
    let type_of_type = Expr::type_of_type().to_infer().unwrap();
    let int_type = Expr::variable("Int").to_infer().unwrap();
    let bool_type = Expr::variable("Bool").to_infer().unwrap();
    let mut global_items = HashMap::new();
    global_items.insert("Int", type_of_type.clone());
    global_items.insert("one", int_type);
    global_items.insert("Bool", type_of_type.clone());
    global_items.insert("true", bool_type);

    let add_tokens: Vec<_> = Token::layout("Int -> Int -> Int").collect();
    global_items.insert("add", expr.parse(&add_tokens).unwrap().to_infer().unwrap());

    let ctx = &mut Context::new(global_items);

    let tokens: Vec<SrcToken> = Token::layout(input).collect();
    let mut expr = expr.parse(&tokens).unwrap().to_infer().unwrap();
    expr.normalize(ctx).unwrap();
    assert_eq!(expr.to_pretty_string(80), expected);
}

#[test]
fn one() {
    test_with_ctx("one", "(one : Int)");
}

#[test]
fn partial_add() {
    test_with_ctx(
        "add one",
        "(((add : (Int -> (Int -> Int))) (one : Int)) : (Int -> Int))",
    );
}
