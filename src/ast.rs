#[derive(Debug)]
pub enum Node {
    IntegerLiteral {
        value: i64,
    },
    InfixExpression {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
}

#[derive(Debug)]
pub enum Operator {
    Plus,
}
