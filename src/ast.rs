#[derive(Debug)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    ExpressionStatement(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    IntegerLiteral {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    PrefixExpression {
        operator: PrefixOperator,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOperator {
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NEQ,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOperator {
    MINUS,
    BANG,
}
