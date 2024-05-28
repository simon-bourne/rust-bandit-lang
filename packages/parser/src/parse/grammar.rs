use chumsky::{
    pratt::{self, left, right, Associativity, Infix},
    primitive::empty,
    recursive::recursive,
    IterParser, Parser,
};

use super::{
    comma, grouped, identifier, in_block, keyword, line_end, operator, optional_line_end,
    parenthesized, TTParser,
};
use crate::{
    ast::{
        Data, DataDeclaration, Expression, Field, Function, Item, Operator, TypeConstructor,
        TypeExpression, TypeParameter, Visibility, VisibilityItems, WhereClause, AST,
    },
    lex::{Grouping, Keyword, NamedOperator},
};

pub fn parser<'src>() -> impl TTParser<'src, AST<'src>> {
    trait_item().repeated().collect().map(|items| AST { items })
}

fn trait_item<'src>() -> impl TTParser<'src, Item<'src>> {
    data().map(Item::Data).or(function().map(Item::Function))
}

fn data_declaration<'src>() -> impl TTParser<'src, DataDeclaration<'src>> {
    let parameters = in_block(type_parameter().separated_by(line_end()).collect())
        .or(type_parameter().repeated().collect());

    keyword(Keyword::Data)
        .ignore_then(identifier())
        .then(parameters)
        .then(where_clause(expression()))
        .map(|((name, parameters), where_clause)| DataDeclaration {
            name,
            parameters,
            where_clause,
        })
}

fn function<'src>() -> impl TTParser<'src, Function<'src>> {
    let unit = grouped(empty(), Grouping::Parentheses);
    identifier()
        .then_ignore(unit.clone())
        .operator(NamedOperator::Assign)
        .then_ignore(unit)
        .map(|name| Function { name })
}

fn field<'src>() -> impl TTParser<'src, Field<'src>> {
    let name = identifier();

    name.then_ignore(operator(NamedOperator::HasType))
        .or_not()
        .then(type_expression(expression()))
        .map(|(name, typ)| Field { name, typ })
}

fn type_parameter<'src>() -> impl TTParser<'src, TypeParameter<'src>> {
    recursive(|parameter| {
        identifier()
            .then(
                operator(NamedOperator::HasType)
                    .ignore_then(type_expression(expression()))
                    .or_not(),
            )
            .map(|(name, kind)| TypeParameter::new(name, kind))
            .or(parenthesized(parameter).map(TypeParameter::parenthesized))
    })
}

fn expression<'src>() -> impl TTParser<'src, Expression<'src>> {
    recursive(|expression| {
        let ident = identifier().map(Expression::Variable);
        let parenthesized =
            parenthesized(expression.clone()).map(|e| Expression::Parenthesized(Box::new(e)));
        let atom = ident.or(parenthesized);

        // Function application is an implicit operator, and has higher precedence than
        // everything else.
        let application = atom.clone().foldl(atom.repeated(), Expression::apply);

        let operators = application.clone().pratt((
            infix(right(1), NamedOperator::To),
            infix(left(5), NamedOperator::Equal),
        ));

        // TODO: Add quantification
        let type_annotated = operators
            .clone()
            .then_ignore(operator(NamedOperator::HasType))
            .then(type_expression(expression.clone()))
            .map(|(expression, type_expression)| {
                Expression::type_annotation(expression, type_expression)
            });

        type_annotated.or(operators)
    })
}

fn type_expression<'src>(
    expression: impl TTParser<'src, Expression<'src>>,
) -> impl TTParser<'src, TypeExpression<'src>> {
    expression
        .clone()
        .then(where_clause(expression))
        .map(|(expression, where_clause)| TypeExpression::new(expression, where_clause))
}

fn data<'src>() -> impl TTParser<'src, Data<'src>> {
    data_declaration()
        .then(visibility_items(type_constructor()))
        .map(|(declaration, constructors)| Data::new(declaration, constructors))
}

fn type_constructor<'src>() -> impl TTParser<'src, TypeConstructor<'src>> {
    let name = identifier();
    name.keyword(Keyword::Of)
        .then(item_list(field()))
        .map(|(name, parameters)| TypeConstructor::new(name, parameters))
        .or(name.map(TypeConstructor::empty))
}

fn visibility_items<'src, T: 'src>(
    parser: impl TTParser<'src, T>,
) -> impl TTParser<'src, VisibilityItems<T>> {
    let public = keyword(Keyword::Public).to(Visibility::Public);
    let private = keyword(Keyword::Private).to(Visibility::Private);

    optional_line_end(public.or(private))
        .then(item_list(parser))
        .map(|(visibility, items)| VisibilityItems::new(visibility, items))
}

fn where_clause<'src>(
    expression: impl TTParser<'src, Expression<'src>>,
) -> impl TTParser<'src, WhereClause<'src>> {
    optional_line_end(keyword(Keyword::Where))
        .ignore_then(item_list(expression))
        .or_not()
        .map(|where_clause| WhereClause(where_clause.unwrap_or_default()))
}

fn item_list<'src, T: 'src>(item: impl TTParser<'src, T>) -> impl TTParser<'src, Vec<T>> {
    in_block(item.clone().separated_by(line_end()).at_least(1).collect())
        .or(item.separated_by(comma()).at_least(1).collect())
}

fn infix<'src>(
    associativity: Associativity,
    name: NamedOperator,
) -> Infix<
    impl TTParser<'src, Operator>,
    impl Fn(Expression<'src>, Operator, Expression<'src>) -> Expression<'src> + Clone,
    Operator,
    (Expression<'src>, Operator, Expression<'src>),
> {
    pratt::infix(associativity, operator(name), Expression::binary_operator)
}

// Windows line endings are a pain, so just skip the parser tests
#[cfg(all(test, not(target_os = "windows")))]
mod tests {
    use std::io::Write;

    use chumsky::{prelude::Input, Parser};
    use goldenfile::Mint;
    use indoc::indoc;

    use crate::{
        lex::{Span, Token},
        parse::grammar::parser,
    };

    #[test]
    fn data_declaration() {
        parse(
            "data-declaration",
            indoc!(
                r#"
                    data MyType
                        a
                        b : Type
                        c : Type -> Type -> Type
                    public
                        X of item : Int
                "#
            ),
        )
    }

    #[test]
    fn data_declaration_where() {
        parse(
            "data-declaration-where",
            indoc!(
                r#"
                    data MyType a (((b : (Type)))) (c : Type -> Type -> Type
                    where a == b, b == c, Ord a) public X of item : Int
                "#
            ),
        )
    }

    #[test]
    fn function() {
        parse("function", r#"my_function() = ()"#);
    }

    fn parse(name: &str, src: &str) {
        let tokens = Token::layout(src).collect::<Vec<_>>();
        let len = src.len();
        let ast = parser().parse(tokens.spanned(Span::new(len, len)));

        let mut mint = Mint::new("tests/goldenfiles");
        let mut output = mint.new_goldenfile(format!("{name}.txt")).unwrap();
        write!(output, "{ast:#?}").unwrap();
    }
}
