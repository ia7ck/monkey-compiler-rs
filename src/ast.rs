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
    InfixExpression {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
}
