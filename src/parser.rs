use crate::expression::*;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::statement::Stmt;
use crate::symbol_table::*;
use crate::types::*;

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
            &self.peek().token_type ==  token_type
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
                Class | Fun | Var | Val | For | If | While | Print | Break | Return => return,
                _ => {
                    self.advance();
                }
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

    pub fn parse(&mut self, global_symbols: &mut SymbolTable) -> Vec<Stmt> {
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
        if self.matches(&[TokenType::Fun]) {
            return self.function("function", symbols);
        }

        if self.matches(&[TokenType::Val, TokenType::Var]) {
            return self.var_declaration(symbols);
        }

        let result = self.statement(symbols);
        if result.is_err() {
            self.synchronize();
        }
        result
    }

    fn function(&mut self, kind: &str, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let name = self.consume_identifier(&format!("expect {} name.", kind))?;
        self.consume(LeftParen, &format!("expect '(' after {} name.", kind))?;
        let mut parameters: Vec<Box<SymbolTableEntry>> = Vec::new();

        if !self.check(&RightParen) {
            loop {
                if parameters.len() >= 255 {
                    let parse_error = ParseError {
                        t: self.peek(),
                        message: "More than 255 parameters not allowed".to_string(),
                    };
                    self.error(parse_error);
                }

                let mut declaration_type = DeclarationType::Val;
                if self.matches(&[Var]) {
                    declaration_type = DeclarationType::Var;
                } else if self.matches(&[Cpy]) {
                    declaration_type = DeclarationType::Cpy;
                } else if self.matches(&[Val]) {
                    // same as default
                    declaration_type = DeclarationType::Val;
                }

                let param_name = self.consume_identifier("expect parameter name.")?;
                self.consume(Colon, "expect ':' after parameter name.")?;

                let type_name = self.advance();
                let param_type = match DataType::from_token_type(&type_name.token_type) {
                    Some(valid_type) => valid_type,
                    None => {
                        return Err(ParseError {
                            t: type_name,
                            message: "Types must be built-in or user defined.".to_string(),
                        });
                    }
                };

                // Add param to local symbol table
                let entry = SymbolTableEntry::new_param(
                    parameters.len(),
                    declaration_type,
                    param_name,
                    param_type,
                );
                parameters.push(Box::new(entry));
                if !self.matches(&[Comma]) {
                    break;
                }
            } // loop
        } // right-paren
        self.consume(RightParen, "expect ')' after parameters.")?;
        let mut return_type = DataType::Empty;
        if self.matches(&[Colon]) {
            let return_type_name = self.advance();
            return_type = match DataType::from_token_type(&return_type_name.token_type) {
                Some(valid_type) => valid_type,
                None => {
                    return Err(ParseError {
                        t: return_type_name,
                        message: "Types must be built-in or user defined.".to_string(),
                    });
                }
            };
        }
        // Needed for the return_type symbol we'll put into the
        // local symbols for the function.
        let return_type_location = self.previous();

        let function_name = name.identifier_string();
        // 'name' is the token holding the location of the function name in the source,
        // 'function_name' has the actual str with the name.
        // The symbol table doesn't need the body of the function.
        let entry = SymbolTableEntry::new_fun(
            symbols.entries.len(),
            Some(name.clone()),
            &function_name,
            parameters.clone(),
            &return_type,
        );

        // Add to parent symbol table
        symbols.add(entry); // For recursion

        // We will add symbols for params, then pass this local symbol table
        // to the function_body() for  more eadditions and extensions.
        let mut local_symbols = symbols.extend();

        let return_type_entry = SymbolTableEntry::new_var(
            local_symbols.entries.len(),
            &return_type_location,
            "RETURN_TYPE",
            &return_type,
            &DataValue::Unresolved,
        );

        local_symbols.add(return_type_entry);

        for param in &parameters {
            let mut p = *param.clone();
            p.entry_number = local_symbols.entries.len();
            local_symbols.add(p);
        }

        // get body
        self.consume(LeftBrace, "expect '{'")?;
        let body = self.function_body(&mut local_symbols)?;

        Ok(Stmt::fun_stmt(
            name,
            parameters,
            return_type,
            body,
            local_symbols,
        ))
    }

    // Like a block_statement but without its own AST node or symbols -- those are owned
    // by the function.

    fn function_body(&mut self, local_symbols: &mut SymbolTable) -> Result<Vec<Stmt>, ParseError> {
        use TokenType::*;
        let mut stmt_list: Vec<Stmt> = Vec::new();
        while !self.check(&RightBrace) && !self.is_finished() {
            let stmt = self.declaration(local_symbols)?;
            stmt_list.push(stmt);
        }
        self.consume(RightBrace, "expect '}' at the end of a function body.")?;
        // Find any return statements and add the return type
        // If none are found it's a parse error.
        Ok(stmt_list)
    }

    // TODO: simplify this var_declaration() !
    fn var_declaration(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let mut decl_type = DeclarationType::Val;
        if matches!(self.previous().token_type, TokenType::Var) {
            decl_type = DeclarationType::Var;
        }

        let v: Token = self.consume_identifier("Expect variable name")?;
        let variable_name = v.identifier_string();

        if self.matches(&[TokenType::Colon]) {
            // Type may be a built-in type or an identifier for a user-defined type
            let type_name = self.advance();
            let valid_type_name = match DataType::from_token_type(&type_name.token_type) {
                Some(valid_type) => valid_type,
                None => {
                    return Err(ParseError {
                        t: type_name,
                        message: "Types must be built-in or user defined.".to_string(),
                    });
                }
            };

            if let DataType::User(ref u) = valid_type_name {
                let has_type = symbols.lookup(u);
                if has_type.is_err() {
                    let message = format!(
                        "Type named {} not declared in this scope or an outer scope.",
                        u
                    );
                    return Err(ParseError {
                        t: v,
                        message: message,
                    });
                }
            }

            if self.matches(&[TokenType::Equal]) {
                let initializer = self.expression(symbols)?;
                let inferred_type_result = initializer.expected_type();
                match inferred_type_result {
                    Err(type_error) => {} // do nothing for now
                    Ok(ref inferred_type) => {
                        if !matches!(inferred_type, &DataType::Unresolved)
                            && inferred_type != &valid_type_name
                        {
                            let message = format!("Can't initialize variable '{}' of type {} to an expression with value {}",
								&v.print(), &valid_type_name, &inferred_type);
                            return Err(ParseError {
                                t: type_name,
                                message,
                            });
                        }
                    }
                }

                self.consume(TokenType::SemiColon, "expect ';'")?;
                let entry_number = symbols.entries.len();
                let entry = if matches!(decl_type, DeclarationType::Var) {
                    SymbolTableEntry::new_var(
                        entry_number,
                        &v,
                        &variable_name,
                        &valid_type_name,
                        &DataValue::Unresolved,
                    )
                } else {
                    SymbolTableEntry::new_val(
                        entry_number,
                        &v,
                        &variable_name,
                        &valid_type_name,
                        &DataValue::Unresolved,
                    )
                };

                if symbols.add(entry) {
                    Ok(Stmt::var_stmt(v, valid_type_name, initializer))
                } else {
                    // Already defined
                    let message = format!("Variable already declared in this scope!");
                    Err(ParseError { t: v, message })
                }
            } else {
                Err(ParseError {
                    t: v,
                    message: "Variable declaration requires initial value assignment with '='."
                        .to_string(),
                })
            }
        } else {
            // infer data type
            if self.matches(&[TokenType::Equal]) {
                let initializer = self.expression(symbols)?;
                let inferred_type = match initializer.expected_type() {
                    Err(type_error) => {
                        self.error(ParseError {
                            t: self.previous(),
                            message: type_error.message.clone(),
                        });

                        let message = format!("Can't infer type for {}, initial value can't be resolved. Please put a specific type next to the variable name. ",&v.print());

                        return Err(ParseError { t: v, message });
                    }
                    Ok(data_type) => data_type,
                };

                self.consume(TokenType::SemiColon, "expect ';'")?;
                let entry_number = symbols.entries.len();
                let entry = if matches!(decl_type, DeclarationType::Var) {
                    SymbolTableEntry::new_var(
                        entry_number,
                        &v,
                        &variable_name,
                        &inferred_type,
                        &DataValue::Unresolved,
                    )
                } else {
                    SymbolTableEntry::new_val(
                        entry_number,
                        &v,
                        &variable_name,
                        &inferred_type,
                        &DataValue::Unresolved,
                    )
                };

                if symbols.add(entry) {
                    Ok(Stmt::var_stmt(v, inferred_type, initializer))
                } else {
                    // Already defined
                    let message = format!("Variable already declared in this scope!");
                    Err(ParseError { t: v, message })
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

    fn statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;

        if self.matches(&[If]) {
            return self.if_statement(symbols);
        }

        if self.matches(&[While]) {
            return self.while_statement(symbols);
        }

        if self.matches(&[Print]) {
            return self.print_statement(symbols);
        }

        if self.matches(&[LeftBrace]) {
            return self.block_statement(symbols);
        }

        if self.matches(&[Return]) {
            return self.return_statement(symbols);
        }

        if self.matches(&[Break]) {
            return self.break_statement(symbols);
        }

        self.expression_statement(symbols)
    }

    fn return_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let location = self.previous();

        // for now don't allow empty returns
        let return_expr = self.expression(symbols)?;
        self.consume(TokenType::SemiColon, "expect ';' after return statement.")?;

        if let Ok(ste) = symbols.lookup("RETURN_TYPE") {
            Ok(Stmt::return_stmt(
                location,
                return_expr,
                ste.data_type.clone(),
            ))
        } else {
            Err(ParseError {
                t: location,
                message: format!("Return statement not in a function!"),
            })
        }
    }

    fn break_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let location = self.previous();
        self.consume(TokenType::SemiColon, "expect ';' after 'break' statement.")?;

        if let Ok(ste) = symbols.lookup("IN_BLOCK") {
            Ok(Stmt::break_stmt(location))
        } else {
            Err(ParseError {
                t: location,
                message: format!("Break statement not in a block ( between '{{' and '}}')!"),
            })
        }
    }

    fn expression_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let expr = self.expression(symbols)?;
        self.consume(TokenType::SemiColon, "Expect ';' after expression.")?;
        Ok(Stmt::expression_stmt(expr))
    }

    fn if_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let condition = self.expression(symbols)?;

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

    fn while_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let condition = self.expression(symbols)?;
        self.consume(LeftBrace, "expect '{' following 'while' condition.")?;
        let body = self.block_statement(symbols)?;
        Ok(Stmt::while_stmt(condition, body))
    }

    fn print_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let mut exprs: Vec<Expr> = Vec::new();
        let expr = self.expression(symbols)?;
        exprs.push(expr);
        while self.matches(&[TokenType::Comma]) {
            let next_expr = self.expression(symbols)?;
            exprs.push(next_expr);
        }

        self.consume(TokenType::SemiColon, "Expected ';'")?;
        Ok(Stmt::print_stmt(exprs))
    }

    fn block_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let mut stmt_list: Vec<Stmt> = Vec::new();
        let block_location = self.previous();
        let mut local_symbols = symbols.extend();
        let entry_number = local_symbols.entries.len();
        let block_entry = SymbolTableEntry::new_val(
            entry_number,
            &block_location,
            "IN_BLOCK",
            &DataType::Empty,
            &DataValue::Unresolved,
        );

        local_symbols.add(block_entry);
        while !self.check(&RightBrace) && !self.is_finished() {
            let stmt = self.declaration(&mut local_symbols)?;
            stmt_list.push(stmt);
        }
        self.consume(RightBrace, "expect '}' after block.")?;
        Ok(Stmt::block_stmt(stmt_list, local_symbols))
    }

    fn expression(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        self.assignment(symbols)
    }

    fn assignment(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let assignee = self.or(symbols)?;
        // Check for special assignment operator
        if self.matches(&[TokenType::ColonEqual]) {
            let change = self.previous();
            let new_value = self.assignment(symbols)?;
            return match assignee {
                Expr::Variable(ref node) => {
                    let (distance, index) =
                        symbols.distance_and_index(&node.name.identifier_string(), 0);

                    Ok(Expr::assignment(
                        node.name.clone(),
                        new_value,
                        distance,
                        index,
                    ))
                }
                _ => {
                    let message = format!("{} not a valid assignment target.", &assignee.print());
                    Err(ParseError { t: change, message })
                }
            };
        } // assignment operator
        Ok(assignee) // if we get here it's an r-value!
    }

    fn or(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.and(symbols)?;
        while self.matches(&[TokenType::Or]) {
            let operator = self.previous();
            let right = self.and(symbols)?;
            expr = Expr::logical(expr, operator, right);
        }
        Ok(expr)
    }

    fn and(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.equality(symbols)?;
        while self.matches(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.equality(symbols)?;
            expr = Expr::logical(expr, operator, right);
        }
        Ok(expr)
    }

    fn equality(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut expr = self.comparison(symbols)?;

        while self.matches(&[LessGreater, Equal]) {
            let operator: Token = self.previous();
            let right: Expr = self.comparison(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn comparison(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut expr = self.term(symbols)?;
        while self.matches(&[Less, LessEqual, Greater, GreaterEqual]) {
            let operator = self.previous();
            let right = self.term(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn term(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.factor(symbols)?;

        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous(); // a + or  -
            let right = self.factor(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn factor(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.unary(symbols)?; // get any leading - or 'not'
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        if self.matches(&[TokenType::Not, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary(symbols)?;
            return Ok(Expr::unary(operator, right));
        }

        self.call(symbols)
    }

    fn call(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        use TokenType::*;

        // Possibly the start of a function call or just a bare
        // primary expression.
        let mut expr = self.primary(symbols)?;
        loop {
            if self.matches(&[LeftParen]) {
                // replace the expression with the full function call
                expr = self.finish_call(expr, symbols)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut args: Vec<Expr> = Vec::new();
        if !self.check(&RightParen) {
            loop {
                let next_arg = self.expression(symbols)?;
                args.push(next_arg);
                if args.len() >= 255 {
                    let parse_error = ParseError {
                        t: self.previous(),
                        message: "Exceeded maximum arguments to function.".to_string(),
                    };
                    self.error(parse_error);
                }
                if !self.matches(&[Comma]) {
                    break;
                }
            }
        }
        let paren = self.consume(RightParen, "expect ')' after arguments.")?;
        Ok(Expr::call(callee, paren, args))
    }

    fn primary(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
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
                let (distance, index) = symbols.distance_and_index(&name, 0);
                Ok(Expr::variable(self.previous(), distance, index))
            }
            LeftParen => {
                self.advance();
                let expr = self.expression(symbols)?;
                self.consume(RightParen, "Expect ')' after expression.")?;
                Ok(Expr::grouping(expr))
            }
            _ => {
                let l = self.peek().line;
                let c = self.peek().column;
                let type_name = self.peek().token_type.print();
                let message = format!("Error parsing primary expression at {}, {}. Found {} but expected a number, string, true, false, or nil.",
					l,c,&type_name);
                self.advance(); // move past the bad token
                Err(ParseError {
                    t: self.peek(),
                    message,
                })
            }
        }
    }
}
