use crate::ast::{Expression, Operator, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{bail, ensure, Result};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST,
    SUM,
    PRODUCT,
    CALL,
}

impl Token {
    fn precedence(&self) -> Precedence {
        use Precedence::*;
        use Token::*;
        match self {
            ILLEGAL(_) => LOWEST,
            EOF => LOWEST,
            INT(_) => LOWEST,
            PLUS => SUM,
            ASTERISK => PRODUCT,
            SEMICOLON => LOWEST,
            LPAREN => CALL,
            RPAREN => LOWEST,
        }
    }
}

pub struct Parser<'a> {
    l: Lexer<'a>,
    cur: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Self {
        let mut parser = Parser {
            l,
            cur: Token::EOF,
            peek: Token::EOF,
        };
        parser.next_token();
        parser.next_token();
        parser
    }
    fn next_token(&mut self) {
        self.cur = self.peek.clone();
        self.peek = self.l.next().unwrap_or(Token::EOF);
    }
    fn cur_token_is(&self, token: Token) -> bool {
        self.cur == token
    }
    fn peek_token_is(&self, token: Token) -> bool {
        self.peek == token
    }
    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();
        while !self.cur_token_is(Token::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(Program { statements })
    }
    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur {
            _ => {
                let exp_stmt = self.parse_expression_statement()?;
                Ok(exp_stmt)
            }
        }
    }
    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let exp = self.parse_expression(Precedence::LOWEST)?;
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement(exp))
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        use Expression::*;
        use Token::*;
        let mut exp = match &self.cur {
            INT(literal) => {
                let value = literal.parse::<i64>()?;
                IntegerLiteral { value }
            }
            LPAREN => self.parse_grouped_expression()?,
            _ => {
                bail!("cannot parse: {:?}", self.cur);
            }
        };
        while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek.precedence() {
            exp = match &self.peek {
                PLUS | ASTERISK => {
                    self.next_token();
                    self.parse_infix_expression(exp)?
                }
                _ => return Ok(exp),
            };
        }
        Ok(exp)
    }
    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        ensure!(self.cur == Token::LPAREN); // (
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        ensure!(self.peek == Token::RPAREN); // )
        self.next_token();
        Ok(exp)
    }
    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let op = match &self.cur {
            Token::PLUS => Operator::PLUS,
            Token::ASTERISK => Operator::ASTERISK,
            token => {
                bail!("unexpected operator: {:?}", token);
            }
        };
        let precedence = self.cur.precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::InfixExpression {
            left: Box::new(left),
            operator: op,
            right: Box::new(right),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Operator, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::fmt::{Display, Formatter};

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse().unwrap()
    }

    #[test]
    fn test_integer_literal_expression() {
        let program = parse("123;");
        let statements = program.statements;
        assert_eq!(statements.len(), 1);
        let stmt = statements[0].clone();
        assert_eq!(
            stmt,
            Statement::ExpressionStatement(Expression::IntegerLiteral { value: 123 })
        );
    }

    impl Display for Program {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            for stmt in &self.statements {
                writeln!(f, "{}", stmt)?
            }
            Ok(())
        }
    }

    impl Display for Statement {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use Statement::*;
            match self {
                ExpressionStatement(exp) => {
                    write!(f, "{}", exp)
                }
            }
        }
    }

    impl Display for Expression {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use Expression::*;
            match self {
                IntegerLiteral { value } => {
                    write!(f, "{}", value)
                }
                InfixExpression {
                    left,
                    operator,
                    right,
                } => {
                    write!(f, "({} {} {})", left, operator, right)
                }
            }
        }
    }

    impl Display for Operator {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use Operator::*;
            match self {
                PLUS => {
                    write!(f, "+")
                }
                ASTERISK => {
                    write!(f, "*")
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("1 + 2 + 3", "((1 + 2) + 3)"),
            ("1 + 2 * 3", "(1 + (2 * 3))"),
            ("1 + (2 + 3)", "(1 + (2 + 3))"),
        ];
        for (input, expected) in tests {
            let program = parse(input);
            let actual = format!("{}", program);
            assert_eq!(expected, actual.trim_end());
        }
    }
}
