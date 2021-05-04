use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{bail, ensure, Result};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESS,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

impl Token {
    fn precedence(&self) -> Precedence {
        use Precedence::*;
        use Token::*;
        match self {
            PLUS | MINUS => SUM,
            ASTERISK | SLASH => PRODUCT,
            LT | GT => LESS,
            EQ | NEQ => EQUALS,
            LPAREN => CALL,
            _ => LOWEST,
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
            // Token::LET => {},
            // Token::RETURN => {},
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
            MINUS | BANG => self.parse_prefix_expression()?,
            LPAREN => self.parse_grouped_expression()?,
            TRUE => Boolean { value: true },
            FALSE => Boolean { value: false },
            IF => self.parse_if_expression()?,
            _ => {
                bail!("cannot parse: {:?}", self.cur);
            }
        };
        while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek.precedence() {
            exp = match &self.peek {
                PLUS | MINUS | ASTERISK | SLASH | LT | GT | EQ | NEQ => {
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
    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let op = match &self.cur {
            Token::MINUS => PrefixOperator::MINUS,
            Token::BANG => PrefixOperator::BANG,
            token => {
                bail!("unexpected operator: {:?}", token);
            }
        };
        self.next_token();
        let right = self.parse_expression(Precedence::PREFIX)?;
        Ok(Expression::PrefixExpression {
            operator: op,
            right: Box::new(right),
        })
    }
    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let op = match &self.cur {
            Token::PLUS => InfixOperator::PLUS,
            Token::MINUS => InfixOperator::MINUS,
            Token::ASTERISK => InfixOperator::ASTERISK,
            Token::SLASH => InfixOperator::SLASH,
            Token::LT => InfixOperator::LT,
            Token::GT => InfixOperator::GT,
            Token::EQ => InfixOperator::EQ,
            Token::NEQ => InfixOperator::NEQ,
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
    fn parse_if_expression(&mut self) -> Result<Expression> {
        ensure!(self.peek == Token::LPAREN);
        self.next_token();
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        ensure!(self.peek == Token::RPAREN);
        self.next_token();
        ensure!(self.peek == Token::LBRACE);
        self.next_token();
        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token_is(Token::ELSE) {
            self.next_token();
            ensure!(self.peek == Token::LBRACE);
            self.next_token();
            let alt = self.parse_block_statement()?;
            Some(Box::new(alt))
        } else {
            None
        };
        Ok(Expression::IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }
    fn parse_block_statement(&mut self) -> Result<Statement> {
        self.next_token();
        let mut statements = Vec::new();
        while !self.cur_token_is(Token::RBRACE) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(Statement::BlockStatement(statements))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
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

    #[test]
    fn test_if_else_expression() {
        use Expression::*;
        use Statement::*;
        let program = parse("if (1 < 2) { 3; 4 } else { 5; };");
        let statements = program.statements;
        assert_eq!(statements.len(), 1);
        let stmt = statements[0].clone();
        assert_eq!(
            stmt,
            ExpressionStatement(IfExpression {
                condition: Box::new(InfixExpression {
                    left: Box::new(IntegerLiteral { value: 1 }),
                    operator: InfixOperator::LT,
                    right: Box::new(IntegerLiteral { value: 2 })
                }),
                consequence: Box::new(BlockStatement(vec![
                    ExpressionStatement(IntegerLiteral { value: 3 }),
                    ExpressionStatement(IntegerLiteral { value: 4 }),
                ])),
                #[rustfmt::skip]
                alternative: Some(Box::new(BlockStatement(vec![
                    ExpressionStatement(IntegerLiteral { value: 5 }),
                ]))),
            })
        )
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
                    writeln!(f, "{}", exp)
                }
                BlockStatement(..) => {
                    unimplemented!()
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
                Boolean { value } => {
                    write!(f, "{}", value)
                }
                PrefixExpression { operator, right } => {
                    write!(f, "({}{})", operator, right)
                }
                InfixExpression {
                    left,
                    operator,
                    right,
                } => {
                    write!(f, "({} {} {})", left, operator, right)
                }
                IfExpression { .. } => {
                    unimplemented!()
                }
            }
        }
    }

    impl Display for PrefixOperator {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use PrefixOperator::*;
            match self {
                MINUS => {
                    write!(f, "-")
                }
                BANG => {
                    write!(f, "!")
                }
            }
        }
    }

    impl Display for InfixOperator {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use InfixOperator::*;
            match self {
                PLUS => {
                    write!(f, "+")
                }
                MINUS => {
                    write!(f, "-")
                }
                ASTERISK => {
                    write!(f, "*")
                }
                SLASH => {
                    write!(f, "/")
                }
                LT => {
                    write!(f, "<")
                }
                GT => {
                    write!(f, ">")
                }
                EQ => {
                    write!(f, "==")
                }
                NEQ => {
                    write!(f, "!=")
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
            ("1 + 2 == 3", "((1 + 2) == 3)"),
            ("1 < 2 != 3 > 4", "((1 < 2) != (3 > 4))"),
            ("-1 * 2", "((-1) * 2)"),
            ("1 * -2", "(1 * (-2))"),
        ];
        for (input, expected) in tests {
            let program = parse(input);
            let actual = format!("{}", program);
            assert_eq!(expected, actual.trim_end());
        }
    }
}
