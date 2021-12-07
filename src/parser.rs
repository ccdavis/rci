use crate::expression::*;
use crate::lex::Token;
use crate::lex::TokenType;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

struct ParseError {
    t: Token,
    message: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn report_error(&self, error: ParseError) {
        // TODO: Call interpreter.error() here to report errors
        match error.t.token_type {
            TokenType::Eof => eprintln!("{} at {:?}. At end of input. ", error.message, error.t),
            _ => eprintln!("{} at {:?}", error.message, error.t),
        }
    }

    pub fn error(&self, error: ParseError) {
        self.report_error(error);
        panic!("Unrecoverable error.");
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        } // each type
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_finished() {
            false
        } else {
            // Using print() isn't the most efficient but it's convenient for now.
            //You can't simply compare the token types directly because some carry
            // data and the comparison will fail if the data differs. But, we only
            // care about the varient part, not the data.
            //
            // The pure Rust solution would have you make methods on TokenType
            // like "is_number(), is_left_brace() etc which works but is pretty
            // verbose. The other solution is to use the Strum crate and pull
            // out the enum discriminant value only.
            // NOTE: looks like this works now too:
            // fn variant_eq(a: &Op, b: &Op) -> bool {
            //    std::mem::discriminant(a) == std::mem::discriminant(b)
            //	}
            self.peek().token_type.print() == token_type.print()
        }
    }
    fn advance(&mut self) -> Token {
        if !self.is_finished() {
            self.current += 1
        }
        self.previous()
    }

    fn is_finished(&self) -> bool {
        match self.peek().token_type {
            TokenType::Eof => true,
            _ => false,
        }
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            Err(ParseError {
                t: self.peek().clone(),
                message: message.to_string(),
            })
        }
    }

    // The idea is to consume tokens until we reach the end of the next statement.
    fn synchronize(&mut self) {
        self.advance();
        use TokenType::*;
        while !self.is_finished() {
            if matches!(self.previous().token_type, SemiColon) {
                return;
            }

            match self.peek().token_type {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => {}
            }
        }
    }

    /*
    Expression grammar from The Crafting Interpreters book

    expression     → equality ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                   | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                   | "(" expression ")"

    */

    pub fn parse(&mut self) -> Expr {
        self.expression()
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        use TokenType::*;
        let mut expr = self.comparison();
        while self.matches(&[LessEqual, Equal]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison();
            expr = Expr::binary(expr, operator, right);
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
		use TokenType::*;
        let mut expr = self.term();        
        while self.matches(&[Less, LessEqual, Greater, GreaterEqual,LessGreater]) {
            let operator = self.previous();
            let right = self.term();
            expr = Expr::binary(expr, operator, right);
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous(); // a + or  -
            let right = self.factor();
            expr = Expr::binary(expr, operator, right);
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary(); // get any leading - or 'not'
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::binary(expr, operator, right);
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if self.matches(&[TokenType::Not, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary();
            Expr::unary(operator, right)
        } else {
            match self.primary() {
                Err(parse_error) => {
                    self.report_error(parse_error);
                    panic!("Unrecoverable error.")
                }
                Ok(primary_expr) => primary_expr,
            }
        }
    }

	
    fn primary(&mut self) -> Result<Expr, ParseError> {
        use TokenType::*;
        match self.peek().token_type {
            True | False | Nil => {
                self.advance();
                Ok(Expr::literal(self.previous()))
            }
            Number(value) => {
                self.advance();
                Ok(Expr::literal(self.previous()))
            }
            Str(value) => {
                self.advance();
                Ok(Expr::literal(self.previous()))
            }
            LeftParen => {
                self.advance();
                let mut expr = self.expression();
                match self.consume(RightParen, "Expect ')' after expression.") {
                    Ok(_) => {}
                    Err(parse_error) => self.report_error(parse_error),
                }
                Ok(Expr::grouping(expr))
            }
            _ => {
                let l = self.peek().line;
                let c = self.peek().column;
                let type_name = self.peek().token_type.print();
                let message = format!("Error parsing primary expression at {}, {}. Found {} but expected a number, string, true, false, or nil.",
					l,c,&type_name);
                Err(ParseError {
                    t: self.peek(),
                    message: message,
                })
            }
        }
    }
}
