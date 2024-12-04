use chumsky::{
    pratt::{self, left, right, Associativity, Infix},
    primitive::{choice, empty},
    recursive::recursive,
    IterParser, Parser,
};

use super::{
    comma, grouped, identifier, in_block, keyword, line_end, operator, optional_line_end,
    parenthesized, TTParser,
};
use crate::{
    ast::{
        Data, DataDeclaration, Expression, Function, Item, Operator, TypeConstructor, Visibility,
        VisibilityItems, WhereClause, AST,
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
    let parameters = in_block(expression().separated_by(line_end()).collect())
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

fn type_parameter<'src>() -> impl TTParser<'src, Expression<'src>> {
    identifier()
        .map(Expression::Variable)
        .or(parenthesized(expression()))
}

fn expression<'src>() -> impl TTParser<'src, Expression<'src>> {
    recursive(|expression| {
        let ident = identifier().map(Expression::Variable);
        let parenthesized =
            parenthesized(expression.clone()).map(|e| Expression::Parenthesized(Box::new(e)));
        let item_list = in_block(
            expression
                .clone()
                .then_ignore(line_end())
                .repeated()
                .foldr(expression.clone(), Expression::line_separator),
        );
        let atom = choice((ident, parenthesized, item_list));

        // Function application is an implicit operator, and has higher precedence than
        // everything else.
        let application = atom.clone().foldl(atom.repeated(), Expression::apply);

        let operators = application.clone().pratt((
            infix(left(1), NamedOperator::HasType),
            infix(right(2), NamedOperator::To),
            pratt::infix(right(3), comma(), Expression::line_separator),
            infix(left(4), NamedOperator::Equal),
        ));

        operators
            .then(
                optional_line_end(keyword(Keyword::Where))
                    .ignore_then(expression)
                    .or_not(),
            )
            .map(|(expr, where_clause)| match where_clause {
                None => expr,
                Some(where_clause) => Expression::where_clause(expr, where_clause),
            })
    })
}

type InfixArgs<'src> = (Expression<'src>, Operator, Expression<'src>);

fn infix<'src>(
    associativity: Associativity,
    name: NamedOperator,
) -> Infix<
    impl TTParser<'src, Operator>,
    impl Fn(Expression<'src>, Operator, Expression<'src>) -> Expression<'src> + Clone,
    Operator,
    InfixArgs<'src>,
> {
    pratt::infix(associativity, operator(name), Expression::binary_operator)
}

fn data<'src>() -> impl TTParser<'src, Data<'src>> {
    data_declaration()
        .then(visibility_items(type_constructor()))
        .map(|(declaration, constructors)| Data::new(declaration, constructors))
}

fn type_constructor<'src>() -> impl TTParser<'src, TypeConstructor<'src>> {
    let name = identifier();
    name.keyword(Keyword::Of)
        .then(expression())
        .map(|(name, parameters)| TypeConstructor::new(name, Some(parameters)))
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
        .ignore_then(expression)
        .or_not()
        .map(WhereClause)
}

fn item_list<'src, T: 'src>(item: impl TTParser<'src, T>) -> impl TTParser<'src, Vec<T>> {
    in_block(item.clone().separated_by(line_end()).at_least(1).collect())
        .or(item.separated_by(comma()).at_least(1).collect())
}

#[cfg(test)]
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
