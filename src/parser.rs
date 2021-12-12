use crate::expression::*;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::statement::Stmt;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
	errors : Vec<ParseError>,
}

pub struct ParseError {
    t: Token,
    message: String,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 , errors: Vec::new()}
    }

    pub fn report_error(&mut self, error: ParseError) {
        // TODO: Call interpreter.error() here to report errors		
        match error.t.token_type {
            TokenType::Eof => {				
				eprintln!("{} at {:?}. At end of input. ", error.message, error.t);				
			},
            _ => {
				eprintln!("{} at {:?}", error.message, error.t);
			},
        }
		self.errors.push(error);
    }

    pub fn error(&mut self, error: ParseError) {
        self.report_error(error);        
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
	
	// Need specialized consume() functions because can't pass 
	// enums with values like Identifier(String), Number(i64) etc.
    fn consume_identifier(&mut self, message: &str) -> Result<Token, ParseError> {
		match self.peek().token_type {
			TokenType::Identifier(_) =>Ok(self.advance()),			
			_ => Err(
				ParseError {
					t: self.peek().clone(),
					message: message.to_string(),
				}
			),
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

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while !self.is_finished() {
			match self.declaration() { 
				Ok(stmt) => statements.push(stmt),            
				Err(parse_error) => self.error(parse_error),
			}
        }
        statements
    }
	
	fn declaration(&mut self) -> Result<Stmt,ParseError> {
		if self.matches(&[TokenType::Var]){
			return self.var_declaration();
		}
		
		let result = self.statement();
		if result.is_err() {
			self.synchronize();
		}
		result		
	}
	
	fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
		let v:Token  = self.consume_identifier("Expect variable name")?;
		if self.matches(&[TokenType::Equal]) {
			let initializer = self.expression()?;
			self.consume(TokenType::SemiColon, "expect ';'")?;
			Ok(Stmt::var_stmt(v, initializer))
		} else {
			Err( ParseError 
				{ t : v, 
				message: "Variable declaration requires initial value assignment with '='.".to_string()
			} )
		}
	}

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        use TokenType::*;
        if self.matches(&[Print]) {
            return self.print_statement();
        }
		
        self.expression_statement()
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::SemiColon, "Expect ';' after expression.")?;
        Ok(Stmt::expression_stmt(expr))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::SemiColon, "Expected ';'")?;
		Ok(Stmt::print_stmt(expr))		
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut expr = self.comparison()?;
        while self.matches(&[LessEqual, Equal]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut expr = self.term()?;
        while self.matches(&[Less, LessEqual, Greater, GreaterEqual, LessGreater]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous(); // a + or  -
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?; // get any leading - or 'not'
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.matches(&[TokenType::Not, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            Ok(Expr::unary(operator, right))
        } else {
			self.primary()            
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
			Identifier(name) => {
				self.advance();								
				Ok(Expr::variable(name))
			}
            LeftParen => {
                self.advance();
                let mut expr = self.expression()?;
                self.consume(RightParen, "Expect ')' after expression.")?;
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
