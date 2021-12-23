use crate::expression::*;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::statement::Stmt;
use crate::types::*;
use crate::symbol_table::*;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<ParseError>,
}

pub struct ParseError {
    t: Token,
    message: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn report_error(&mut self, error: ParseError) {
        // TODO: Call interpreter.error() here to report errors
        match error.t.token_type {
            TokenType::Eof => {
                eprintln!("{} at {:?}. At end of input. ", error.message, error.t);
            }
            _ => {
                eprintln!("{} at {:?}", error.message, error.t);
            }
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
            TokenType::Identifier(_) => Ok(self.advance()),
            _ => Err(ParseError {
                t: self.peek().clone(),
                message: message.to_string(),
            }),
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

    pub fn parse(&mut self, global_symbols:&mut SymbolTable) -> Vec<Stmt> {
        let mut statements = Vec::new();				
        while !self.is_finished() {
            match self.declaration(global_symbols) {
                Ok(stmt) => statements.push(stmt),
                Err(parse_error) => self.error(parse_error),
            }
        }
        statements
    }

    fn declaration(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        if self.matches(&[TokenType::Var]) {
            return self.var_declaration(symbols);
        }

        let result = self.statement(symbols);
        if result.is_err() {			
            self.synchronize();
        }
        result
    }

    fn var_declaration(&mut self, symbols:&mut SymbolTable) -> Result<Stmt, ParseError> {
        let v: Token = self.consume_identifier("Expect variable name")?;
		let variable_name = match v.token_type {
			TokenType::Identifier(ref n) => n.clone(),
			_ => panic!("Tried to consume identifier!"),
		};
		if self.matches(&[TokenType::Colon]) {
			// Type may be a built-in type or an identifier for a user-defined type			
			let type_name = self.advance();			
			let valid_type_name = match DataType::from_token_type(&type_name.token_type){
				Some(valid_type) => valid_type,
				None => {											
					return Err(ParseError { 
						t:type_name, 
						message: "Types must be built-in or user defined.".to_string() 
					});
				}
			};
			
			if let DataType::User(ref u) = valid_type_name {
				let has_type = symbols.lookup(u);
				if has_type.is_err(){
					let message = format!("Type named {} not declared in this scope or an outer scope.",u);
					return Err(ParseError { 
						t:v, 
						message:message						
					});
				}				
			}			
			
			if self.matches(&[TokenType::Equal]) {
				let initializer = self.expression()?;				
				let inferred_type_result = initializer.expected_type();
				match inferred_type_result {
					Err(type_error) =>  {}, // do nothing for now
					Ok(ref inferred_type) => {
						if inferred_type != &valid_type_name {
							let message = format!("Can't initialize variable '{}' of type {} to an expression with value {}",
								&v.print(), &valid_type_name, &inferred_type);
							return Err(ParseError { 
								t: type_name,
								message });
						}
					}
				}
				
				self.consume(TokenType::SemiColon, "expect ';'")?;
				
				let entry = SymbolTableEntry::new_var(
					&v, &variable_name, &valid_type_name, &DataValue::Unresolved);
				if symbols.add(entry) {
					Ok(Stmt::var_stmt(v, valid_type_name, initializer))
				} else {
					// Already defined
					let message = format!("Variable already declared in this scope!");
					Err(ParseError{t:v, message})
				}
			} else {
				Err(ParseError {
					t: v,
					message: "Variable declaration requires initial value assignment with '='."
						.to_string(),
				})
			}
													
		} else { // infer data type
			
			if self.matches(&[TokenType::Equal]) {
				let initializer = self.expression()?;
				let inferred_type = match initializer.expected_type() {
					Err(type_error) => {
						self.error( ParseError { 
							t: self.previous(),
							message: type_error.message.clone()});
						
						let message = format!("Can't infer type for {}, initial value can't be resolved. Please put a specific type next to the variable name. ",&v.print());
						
						return  Err(ParseError { t: v, message });
						
						
					},
					Ok(data_type) => data_type,
				};
				
				self.consume(TokenType::SemiColon, "expect ';'")?;
				let entry = SymbolTableEntry::new_var(
					&v, &variable_name, &inferred_type, &DataValue::Unresolved);
				if symbols.add(entry) {
					Ok(Stmt::var_stmt(v, inferred_type, initializer))
				} else {
					// Already defined
					let message = format!("Variable already declared in this scope!");
					Err(ParseError{t:v, message})
				}
			} else {
				Err(ParseError {
					t: v,
					message: "Variable declaration requires initial value assignment with '='."
						.to_string(),
				})
			}
		}
	
    }

    fn statement(&mut self, symbols:&mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
		
		if self.matches(&[If]) {
			return self.if_statement(symbols);
		}
		
        if self.matches(&[Print]) {
            return self.print_statement(symbols);
        }

        if self.matches(&[LeftBrace]) {
            return self.block_statement(symbols);
        }

        self.expression_statement(symbols)
    }

    fn expression_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::SemiColon, "Expect ';' after expression.")?;
        Ok(Stmt::expression_stmt(expr))
    }
	
	fn if_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
		use TokenType::*;
		let condition = self.expression()?;
				
		self.consume(LeftBrace, "expect '{' following 'if' condition.")?;
		let then_branch = self.block_statement(symbols)?;
		if self.matches(&[Else]) {
			self.consume(LeftBrace, "expect '{' after 'else' in 'if' statement.")?;
			let else_branch = self.block_statement(symbols)?;
			Ok(Stmt::if_stmt(condition, then_branch, Some(else_branch)))
		} else {
			Ok(Stmt::if_stmt(condition, then_branch, None))
		}
	}

    fn print_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::SemiColon, "Expected ';'")?;
        Ok(Stmt::print_stmt(expr))
    }

    fn block_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let mut stmt_list: Vec<Stmt> = Vec::new();
		let mut local_symbols = symbols.extend();
        while !self.check(&RightBrace) && !self.is_finished() {
            let stmt = self.declaration(&mut local_symbols)?;
            stmt_list.push(stmt);
        }
        self.consume(RightBrace, "expect '}' after block.")?;
        Ok(Stmt::block_stmt(stmt_list, local_symbols))
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let assignee = self.equality()?;
        // Check for special assignment operator
        if self.matches(&[TokenType::ColonEqual]) {
            let change = self.previous();
            let new_value = self.assignment()?;
            return match assignee {
                Expr::Variable(ref node) => Ok(Expr::assignment(node.name.clone(), new_value)),
                _ => {
                    let message = format!("{} not a valid assignment target.", &assignee.print());
                    Err(ParseError { t: change, message })
                }
            };
        } // assignment operator
        Ok(assignee) // if we get here it's an r-value!
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut expr = self.comparison()?;
        while self.matches(&[LessGreater, Equal]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut expr = self.term()?;
        while self.matches(&[Less, LessEqual, Greater, GreaterEqual]) {
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
                Ok(Expr::variable(self.previous()))
            }
            LeftParen => {
                self.advance();
                let expr = self.expression()?;
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
                    message,
                })
            }
        }
    }
}
