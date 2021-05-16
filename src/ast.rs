#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
    pub fn statements(self) -> Vec<Statement> {
        self.statements
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement { name: String, value: Expression },
    ExpressionStatement(Expression),
    BlockStatement(Vec<Statement>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    Boolean(bool),
    PrefixExpression {
        operator: PrefixOperator,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    MINUS,
    BANG,
}
