use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{bail, Result};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESS,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
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
            LBRACKET => INDEX,
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
        // self.cur <- self.peek
        // self.peek <- self.l.next()
        std::mem::swap(&mut self.cur, &mut self.peek);
        self.peek = self.l.next().unwrap_or(Token::EOF);
    }
    fn cur_token_is(&self, token: &Token) -> bool {
        &self.cur == token
    }
    fn peek_token_is(&self, token: &Token) -> bool {
        &self.peek == token
    }
    fn expect_peek(&self, token: &Token) -> Result<()> {
        if !self.peek_token_is(token) {
            bail!(
                "expected next token to be {:?}, got {:?} instead",
                token,
                self.peek
            );
        }
        Ok(())
    }
    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();
        while !self.cur_token_is(&Token::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(Program::new(statements))
    }
    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur {
            Token::LET => {
                let let_stmt = self.parse_let_statement()?;
                Ok(let_stmt)
            }
            Token::RETURN => {
                let return_stmt = self.parse_return_statement()?;
                Ok(return_stmt)
            }
            _ => {
                let exp_stmt = self.parse_expression_statement()?;
                Ok(exp_stmt)
            }
        }
    }
    fn parse_let_statement(&mut self) -> Result<Statement> {
        match &self.peek {
            Token::IDENT(literal) => {
                let name = literal.to_string();
                self.next_token(); // self.cur <- IDENT

                self.expect_peek(&Token::ASSIGN)?; // =
                self.next_token(); // self.cur <- ASSIGN

                self.next_token();
                let value = self.parse_expression(Precedence::LOWEST)?;

                if self.peek_token_is(&Token::SEMICOLON) {
                    self.next_token();
                }
                Ok(Statement::LetStatement { name, value })
            }
            peek => bail!("expected next token to be LET, got {:?} instead", peek),
        }
    }
    fn parse_return_statement(&mut self) -> Result<Statement> {
        assert_eq!(self.cur, Token::RETURN);
        self.next_token();

        let return_value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::ReturnStatement(return_value))
    }
    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let exp = self.parse_expression(Precedence::LOWEST)?;
        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement(exp))
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        use Expression::*;
        use Token::*;
        let mut exp = match &self.cur {
            IDENT(literal) => Identifier(literal.to_string()),
            INT(literal) => {
                let value = literal.parse::<i64>()?;
                IntegerLiteral(value)
            }
            STRING(literal) => StringLiteral(literal.to_string()),
            MINUS | BANG => self.parse_prefix_expression()?,
            LPAREN => self.parse_grouped_expression()?,
            TRUE => Boolean(true),
            FALSE => Boolean(false),
            IF => self.parse_if_expression()?,
            LBRACKET => self.parse_array_literal()?,
            LBRACE => self.parse_hash_literal()?,
            FUNCTION => self.parse_function_literal()?,
            _ => {
                bail!("cannot parse: {:?}", self.cur);
            }
        };
        while !self.peek_token_is(&Token::SEMICOLON) && precedence < self.peek.precedence() {
            exp = match &self.peek {
                PLUS | MINUS | ASTERISK | SLASH | LT | GT | EQ | NEQ => {
                    self.next_token();
                    self.parse_infix_expression(exp)?
                }
                LPAREN => {
                    self.next_token();
                    self.parse_call_expression(exp)?
                }
                LBRACKET => {
                    self.next_token();
                    self.parse_index_expression(exp)?
                }
                _ => return Ok(exp),
            };
        }
        Ok(exp)
    }
    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        assert_eq!(self.cur, Token::LPAREN); // (
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        self.expect_peek(&Token::RPAREN)?; // )
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
        self.expect_peek(&Token::LPAREN)?; // (
        self.next_token(); // self.cur <- LPAREN

        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;

        self.expect_peek(&Token::RPAREN)?; // )
        self.next_token(); // self.cur <- RPAREN

        self.expect_peek(&Token::LBRACE)?; // {
        self.next_token(); // self.cur <- LBRACE

        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token_is(&Token::ELSE) {
            self.next_token();

            self.expect_peek(&Token::LBRACE)?; // {
            self.next_token(); // self.cur <- LBRACE

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
        while !self.cur_token_is(&Token::RBRACE) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(Statement::BlockStatement(statements))
    }
    fn parse_array_literal(&mut self) -> Result<Expression> {
        assert_eq!(self.cur, Token::LBRACKET); // [
        self.next_token();
        let elements = self.parse_expression_list(Token::RBRACKET)?; // ]
        Ok(Expression::ArrayLiteral(elements))
    }
    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>> {
        if self.cur_token_is(&end) {
            return Ok(vec![]);
        }
        let mut result = Vec::new();
        result.push(self.parse_expression(Precedence::LOWEST)?);
        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token(); // ,
            result.push(self.parse_expression(Precedence::LOWEST)?);
        }
        self.expect_peek(&end)?;
        self.next_token();
        Ok(result)
    }
    fn parse_hash_literal(&mut self) -> Result<Expression> {
        assert_eq!(self.cur, Token::LBRACE); // {
        let mut pairs = Vec::new();
        while !self.peek_token_is(&Token::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST)?;

            self.expect_peek(&Token::COLON)?; // :
            self.next_token();

            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST)?;
            pairs.push((key, value));

            if !self.peek_token_is(&Token::RBRACE) {
                self.expect_peek(&Token::COMMA)?;
                self.next_token();
            }
        }
        self.expect_peek(&Token::RBRACE)?; // }
        self.next_token();
        Ok(Expression::HashLiteral(pairs))
    }
    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression> {
        assert_eq!(self.cur, Token::LBRACKET); // [
        self.next_token();

        let index = self.parse_expression(Precedence::LOWEST)?;

        self.expect_peek(&Token::RBRACKET)?; // ]
        self.next_token();

        Ok(Expression::IndexExpression {
            left: Box::new(left),
            index: Box::new(index),
        })
    }
    fn parse_function_literal(&mut self) -> Result<Expression> {
        assert_eq!(self.cur, Token::FUNCTION); // fn
        self.expect_peek(&Token::LPAREN)?; // (
        self.next_token();

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&Token::LBRACE)?; // {
        self.next_token();

        let body = self.parse_block_statement()?;
        Ok(Expression::FunctionLiteral {
            parameters,
            body: Box::new(body),
        })
    }
    fn parse_function_parameters(&mut self) -> Result<Vec<Expression>> {
        if self.peek_token_is(&Token::RPAREN) {
            self.next_token();
            return Ok(vec![]);
        }

        self.next_token();
        let mut identifiers = Vec::new();
        let mut push_identifier = |current_token: &Token| -> Result<()> {
            match current_token {
                Token::IDENT(literal) => {
                    identifiers.push(Expression::Identifier(literal.to_string()));
                }
                token => {
                    bail!(
                        "expected current token to be IDENT, got {:?} instead",
                        token
                    );
                }
            }
            Ok(())
        };
        push_identifier(&self.cur)?;

        while self.peek_token_is(&Token::COMMA) {
            self.next_token();
            self.next_token();
            push_identifier(&self.cur)?;
        }
        self.expect_peek(&Token::RPAREN)?;
        self.next_token();

        Ok(identifiers)
    }
    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        assert_eq!(self.cur, Token::LPAREN);
        self.next_token();

        let arguments = self.parse_expression_list(Token::RPAREN)?;
        Ok(Expression::CallExpression {
            function: Box::new(function),
            arguments,
        })
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
    fn test_let_statements() {
        let program = parse("let x = 5; let y = z;");
        let statements = program.statements();
        assert_eq!(statements.len(), 2);

        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::LetStatement {
                name: "x".to_string(),
                value: Expression::IntegerLiteral(5)
            }
        );

        let stmt = &statements[1];
        assert_eq!(
            stmt,
            &Statement::LetStatement {
                name: "y".to_string(),
                value: Expression::Identifier("z".to_string()),
            }
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let program = parse("123;");
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::ExpressionStatement(Expression::IntegerLiteral(123))
        );
    }

    #[test]
    fn test_if_else_expression() {
        use Expression::*;
        use Statement::*;
        let program = parse("if (1 < 2) { 3; 4 } else { 5; };");
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &ExpressionStatement(IfExpression {
                condition: Box::new(InfixExpression {
                    left: Box::new(IntegerLiteral(1)),
                    operator: InfixOperator::LT,
                    right: Box::new(IntegerLiteral(2))
                }),
                consequence: Box::new(BlockStatement(vec![
                    ExpressionStatement(IntegerLiteral(3)),
                    ExpressionStatement(IntegerLiteral(4)),
                ])),
                #[rustfmt::skip]
                alternative: Some(Box::new(BlockStatement(vec![
                    ExpressionStatement(IntegerLiteral (5)),
                ]))),
            })
        )
    }

    #[test]
    fn test_array_literal_expression() {
        let program = parse("[1, 2 + 3];");
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::ExpressionStatement(Expression::ArrayLiteral(vec![
                Expression::IntegerLiteral(1),
                Expression::InfixExpression {
                    left: Box::new(Expression::IntegerLiteral(2)),
                    operator: InfixOperator::PLUS,
                    right: Box::new(Expression::IntegerLiteral(3)),
                }
            ]))
        )
    }

    #[test]
    fn test_hash_literal_expression() {
        let program = parse(r#"  {"key": 123, 456: "val"};  "#);
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::ExpressionStatement(Expression::HashLiteral(vec![
                (
                    Expression::StringLiteral("key".to_string()),
                    Expression::IntegerLiteral(123)
                ),
                (
                    Expression::IntegerLiteral(456),
                    Expression::StringLiteral("val".to_string())
                ),
            ]))
        )
    }

    #[test]
    fn test_index_expression() {
        let program = parse("arr[1 + 2]");
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::ExpressionStatement(Expression::IndexExpression {
                left: Box::new(Expression::Identifier("arr".to_string())),
                index: Box::new(Expression::InfixExpression {
                    left: Box::new(Expression::IntegerLiteral(1)),
                    operator: InfixOperator::PLUS,
                    right: Box::new(Expression::IntegerLiteral(2))
                })
            })
        )
    }

    #[test]
    fn test_function_literal_expression() {
        let program = parse("fn(x) { return x + 1; }");
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::ExpressionStatement(Expression::FunctionLiteral {
                parameters: vec![Expression::Identifier("x".to_string())],
                body: Box::new(Statement::BlockStatement(vec![Statement::ReturnStatement(
                    Expression::InfixExpression {
                        left: Box::new(Expression::Identifier("x".to_string())),
                        operator: InfixOperator::PLUS,
                        right: Box::new(Expression::IntegerLiteral(1)),
                    }
                )])),
            })
        );
    }

    #[test]
    fn test_call_expression() {
        let program = parse("add(1, 2 + 3)");
        let statements = program.statements();
        assert_eq!(statements.len(), 1);
        let stmt = &statements[0];
        assert_eq!(
            stmt,
            &Statement::ExpressionStatement(Expression::CallExpression {
                function: Box::new(Expression::Identifier("add".to_string())),
                arguments: vec![
                    Expression::IntegerLiteral(1),
                    Expression::InfixExpression {
                        left: Box::new(Expression::IntegerLiteral(2)),
                        operator: InfixOperator::PLUS,
                        right: Box::new(Expression::IntegerLiteral(3))
                    }
                ]
            })
        )
    }

    impl Display for Statement {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use Statement::*;
            match self {
                LetStatement { .. } => unimplemented!(),
                ReturnStatement(..) => unimplemented!(),
                ExpressionStatement(exp) => write!(f, "{}", exp),
                BlockStatement(..) => unimplemented!(),
            }
        }
    }

    impl Display for Expression {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use Expression::*;
            match self {
                Identifier { .. } => unimplemented!(),
                IntegerLiteral(value) => write!(f, "{}", value),
                StringLiteral(..) => unimplemented!(),
                Boolean(value) => write!(f, "{}", value),
                PrefixExpression { operator, right } => write!(f, "({}{})", operator, right),
                InfixExpression {
                    left,
                    operator,
                    right,
                } => write!(f, "({} {} {})", left, operator, right),
                IfExpression { .. } => unimplemented!(),
                ArrayLiteral(..) => unimplemented!(),
                HashLiteral(..) => unimplemented!(),
                IndexExpression { .. } => unimplemented!(),
                FunctionLiteral { .. } => unimplemented!(),
                CallExpression { .. } => unimplemented!(),
            }
        }
    }

    impl Display for PrefixOperator {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use PrefixOperator::*;
            match self {
                MINUS => write!(f, "-"),
                BANG => write!(f, "!"),
            }
        }
    }

    impl Display for InfixOperator {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            use InfixOperator::*;
            match self {
                PLUS => write!(f, "+"),
                MINUS => write!(f, "-"),
                ASTERISK => write!(f, "*"),
                SLASH => write!(f, "/"),
                LT => write!(f, "<"),
                GT => write!(f, ">"),
                EQ => write!(f, "=="),
                NEQ => write!(f, "!="),
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
            let statements = program.statements();
            assert_eq!(statements.len(), 1);
            let stmt = &statements[0];
            let stmt = format!("{}", stmt);
            assert_eq!(expected, stmt);
        }
    }
}
