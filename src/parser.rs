use crate::errors::parse_err;
use crate::errors::Error;
use crate::expression::*;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::statement::Stmt;
use crate::symbol_table::*;
use crate::types::*;

type ParseError = crate::errors::Error;

const TRACE:bool = false;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn error(&mut self, error: ParseError) {
        eprintln!("{}", &error.format());
        self.errors.push(error);
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
            &self.peek().token_type == token_type
        }
    }

    fn print_next(&self) {
        println!(" {} ", &self.peek().token_type.print())
    }

    fn advance(&mut self) -> Token {
        if TRACE { self.print_next();}
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
            Err(parse_err(&self.peek(), message))
        }
    }

    // Need specialized consume() functions because can't pass
    // enums with values like Identifier(String), Number(i64) etc.
    fn consume_identifier(&mut self, message: &str) -> Result<Token, ParseError> {
        match self.peek().token_type {
            TokenType::Identifier(_) => Ok(self.advance()),
            _ => Err(parse_err(&self.peek(), message)),
        }
    }

    fn match_terminator(&mut self) -> Result<bool, ParseError> {                        
        if self.check(&TokenType::SemiColon) {
            self.advance();
            self.skip_if_newline();
            Ok(true)
        } else if self.check(&TokenType::Eol) {
            self.advance();
            Ok(true)
        } else {
            let message = "Expected ';' or newline";
            Err(parse_err(&self.peek(), message))
        }
    }

    fn skip_if(&mut self, token_type: &TokenType) {
        if self.check(token_type) {
            self.advance();
        }
    }

    fn skip_if_comma(&mut self) {
        self.skip_if(&TokenType::Comma);
    }

    fn skip_if_newline(&mut self) {
        self.skip_if(&TokenType::Eol);
    }

    fn skip_all_newlines(&mut self) {
        while self.check(&TokenType::Eol) {
            self.advance();
        }
    }

    // The idea is to consume tokens until we reach the end of the next statement.
    fn synchronize(&mut self) {
        self.advance();
        use TokenType::*;
        while !self.is_finished() {
            if matches!(self.previous().token_type, SemiColon)
                || matches!(self.previous().token_type, Eol)
            {
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

    pub fn parse(&mut self, global_symbols: &mut SymbolTable) -> 
		Result< Vec<Stmt>, Vec<ParseError>> {
        let mut statements = Vec::new();
        while !self.is_finished() {
            // Here is where we'd put imports or module includes
            // of some sort, then read the main program.
            match self.program(global_symbols) {
                Ok(stmt) => statements.push(stmt),
                Err(parse_error) => self.error(parse_error),
            }
        }
        if self.errors.len() > 0 {
			Err(self.errors.clone())
		} else {
			Ok(statements)
		}
    }

    // A program is a statement. It is a list of declarations followed by a block
    // statement type with the entry point function calls or statements.
    fn program(&mut self, global_symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let mut decls = Vec::new();
        let mut imperatives = Stmt::NoOp;
        let mut found_main = false; // a 'program' may lack imperatives if it's a module

        while !self.is_finished() {
            if found_main {
                let message =
                    format!("Program entry point block already found,no more statements allowed.");
                return Err(parse_err(&self.previous(), &message));
            }
            let stmt = self.declaration(global_symbols)?;
            match stmt {
                Stmt::Var(_) | Stmt::Fun(_) => decls.push(stmt),
                Stmt::Block(_) => {
                    imperatives = stmt;
                    found_main = true;
                }
                _ => {
                    let message =
						format!("Programs can only have declarations (fun, var, val) and a block '{{','}}' at the end.");
                    return Err(parse_err(&self.previous(), &message));
                }
            }
        }
        // If we get to this point without finding {} (main) the
        // imperatives will be a NoOp which is fine for a module.
        Ok(Stmt::program(decls, Box::new(imperatives)))
    }

    fn declaration(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {        
        self.skip_all_newlines();
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
        self.skip_all_newlines();
        if !self.check(&RightParen) {
            loop {
                if parameters.len() >= 255 {
                    let parse_error =
                        parse_err(&self.peek(), "More than 255 parameters not allowed");
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
                let mut param_type = match DataType::from_token_type(&type_name.token_type) {
                    Some(valid_type) => valid_type,
                    None => {
                        return Err(parse_err(
                            &type_name,
                            "Types must be built-in or user defined.",
                        ));
                    }
                };

                if let DataType::Lookup(_) = param_type {
                    self.consume(Less, "expect '<' after 'Array, DirectMap, HashMap' to complete type signature.")?;
                    let lookup_type_name = self.advance();
                    if matches!(array_type_name.token_type, TokenType::ArrayType) {
                        return Err(parse_err(
                            &array_type_name,
                            "Can't nest arrays in function parameters.",
                        ));
                    }

                    let array_contains_type = match DataType::from_token_type(&array_type_name.token_type) {
                        Some(valid_type) => valid_type,
                        None => {
                            return Err(parse_err(
								&array_type_name,
								"Can't make an array of this type. Types must be built-in or user defined."
							));
                        }
                    };

                    self.consume(Greater, "expect '>' after array member type.")?;
                    let array_type = LookupType::Array { size: None, DataType::Number, contains_type: array_contains_type};
                    param_type = DataType::Lookup(Box::new(array_type))
                }

                // Add param to local symbol table
                let entry = SymbolTableEntry::new_param(
                    parameters.len(),
                    declaration_type,
                    param_name,
                    param_type,
                );
                parameters.push(Box::new(entry));
                if !self.matches(&[Comma]) {
                    self.skip_if_newline();
                    break;
                }
                self.skip_if_newline();
            } // loop
        } // right-paren
        self.skip_if_comma();
        self.consume(RightParen, "expect ')' after parameters.")?;
        self.skip_if_newline();
        let mut return_type = DataType::Empty;
        if self.matches(&[Colon]) {
            let return_type_name = self.advance();
            return_type = match DataType::from_token_type(&return_type_name.token_type) {
                Some(valid_type) => valid_type,
                None => {
                    return Err(parse_err(
                        &return_type_name,
                        "Types must be built-in or user defined.",
                    ));
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
        self.skip_if_newline();
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
        self.skip_all_newlines();
        while !self.check(&RightBrace) && !self.is_finished() {
            let stmt = self.declaration(local_symbols)?;
            stmt_list.push(stmt);
            self.skip_all_newlines();
        }
        self.consume(RightBrace, "expect '}' at the end of a function body.")?;
        self.skip_all_newlines();
        // TODO: Find any return statements and add the return type
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
                // For the most part complex types should be declared elsewhere, but
                // we allow Map or Array.
                Some(valid_type) => match valid_type {
                    DataType::Lookup(_) => {
                        self.consume(
                            TokenType::Less,
                            "expect '<' after 'array' to complete type signature.",
                        )?;
                        let array_type_name = self.advance();
                        if matches!(array_type_name.token_type, TokenType::ArrayType) {
                            return Err(parse_err(
                                &array_type_name,
                                "Can't nest arrays in variable type declarations.",
                            ));
                        }

                        let array_type =
                            match DataType::from_token_type(&array_type_name.token_type) {
                                Some(simple_type) => simple_type,
                                None => {
                                    return Err(parse_err(
									&array_type_name,
									"Can't make an array of this type. Types must be built-in or user defined."
								));
                                }
                            };

                        self.consume(TokenType::Greater, "expect '>' after array member type.")?;
                        DataType::Array(Box::new(array_type))
                    }

                    // TODO Map and Set go here
                    _ => valid_type,
                },
                None => {
                    return Err(parse_err(
                        &type_name,
                        "Types must be built-in or user defined.",
                    ));
                }
            };

            if let DataType::User(ref u) = valid_type_name {
                let has_type = symbols.lookup(u);
                if has_type.is_err() {
                    let message = format!(
                        "Type named {} not declared in this scope or an outer scope.",
                        u
                    );
                    return Err(parse_err(&v, &message));
                }
            }

            if self.matches(&[TokenType::Equal]) {
                self.skip_if_newline();
                let initializer = self.expression(symbols)?;
                //self.consume(TokenType::SemiColon, "expect ';'")?;
                self.match_terminator()?;
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
                    Err(parse_err(&v, &message))
                }
            } else {
                Err(parse_err(
                    &v,
                    "Variable declaration requires initial value assignment with '='.",
                ))
            }
        } else {
            // infer data type
            if self.matches(&[TokenType::Equal]) {
                self.skip_if_newline();
                let initializer = self.expression(symbols)?;
                let inferred_type = match initializer.determine_type(symbols) {
                    Err(type_error) => {
                        self.error(parse_err(&self.previous(), &type_error.message));

                        let message = format!("Can't infer type for {}, initial value can't be resolved. Please put a specific type next to the variable name. ",&v.print());
                        return Err(parse_err(&v, &message));
                    }
                    Ok(data_type) => data_type,
                };

                //self.consume(TokenType::SemiColon, "expect ';'")?;
                self.match_terminator()?;
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
                    Err(parse_err(&v, &message))
                }
            } else {
                Err(parse_err(
                    &v,
                    "Variable declaration requires initial value assignment with '='.",
                ))
            }
        }
    }

    fn statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        self.skip_all_newlines();

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
        //self.consume(TokenType::SemiColon, "expect ';' after return statement.")?;
        self.match_terminator()?;

        if let Ok(ste) = symbols.lookup("RETURN_TYPE") {
            Ok(Stmt::return_stmt(
                location,
                return_expr,
                ste.data_type.clone(),
            ))
        } else {
            let message = format!("Return statement not in a function!");
            Err(parse_err(&location, &message))
        }
    }

    fn break_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let location = self.previous();
        //self.consume(TokenType::SemiColon, "expect ';' after 'break' statement.")?;
        self.match_terminator()?;

        if let Ok(ste) = symbols.lookup("IN_BLOCK") {
            Ok(Stmt::break_stmt(location))
        } else {
            let message = format!("Break statement not in a block ( between '{{' and '}}')!");
            Err(parse_err(&location, &message))
        }
    }

    fn expression_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let expr = self.expression(symbols)?;
        //self.consume(TokenType::SemiColon, "Expect ';' after expression.")?;
        self.match_terminator()?;
        Ok(Stmt::expression_stmt(expr))
    }

    fn if_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let location = self.previous();
        let condition = self.expression(symbols)?;
        self.skip_all_newlines();

        self.consume(LeftBrace, "expect '{' following 'if' condition.")?;
        let then_branch = self.block_statement(symbols)?;
        self.skip_if_newline();
        if self.matches(&[Else]) {
            self.skip_all_newlines();
            self.consume(LeftBrace, "expect '{' after 'else' in 'if' statement.")?;
            let else_branch = self.block_statement(symbols)?;
            Ok(Stmt::if_stmt(
                location,
                condition,
                then_branch,
                Some(else_branch),
            ))
        } else {
            Ok(Stmt::if_stmt(location, condition, then_branch, None))
        }
    }

    fn while_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        use TokenType::*;
        let location = self.previous();
        let condition = self.expression(symbols)?;
        self.skip_if_newline();
        self.consume(LeftBrace, "expect '{' following 'while' condition.")?;
        self.skip_all_newlines();
        let body = self.block_statement(symbols)?;
        Ok(Stmt::while_stmt(location, condition, body))
    }

    fn print_statement(&mut self, symbols: &mut SymbolTable) -> Result<Stmt, ParseError> {
        let mut exprs: Vec<Expr> = Vec::new();
        let expr = self.expression(symbols)?;
        exprs.push(expr);
        while self.matches(&[TokenType::Comma]) {
            self.skip_if_newline();
            let next_expr = self.expression(symbols)?;
            exprs.push(next_expr);
        }

        //self.consume(TokenType::SemiColon, "Expected ';'")?;
        self.match_terminator()?;
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
            self.skip_all_newlines();
            let stmt = self.declaration(&mut local_symbols)?;
            stmt_list.push(stmt);
        }
        self.skip_all_newlines();
        self.consume(RightBrace, "expect '}' after block.")?;
        self.skip_all_newlines();
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
                    Err(parse_err(&change, &message))
                }
            };
        } // assignment operator
        Ok(assignee) // if we get here it's an r-value!
    }

    fn or(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.and(symbols)?;
        while self.matches(&[TokenType::Or]) {
            let operator = self.previous();
            self.skip_all_newlines();
            let right = self.and(symbols)?;
            expr = Expr::logical(expr, operator, right);
        }
        Ok(expr)
    }

    fn and(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.equality(symbols)?;
        while self.matches(&[TokenType::And]) {
            let operator = self.previous();
            self.skip_all_newlines();
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
            self.skip_all_newlines();
            let right = self.term(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn term(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.factor(symbols)?;

        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous(); // a + or  -
            self.skip_all_newlines();
            let right = self.factor(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn factor(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        let mut expr = self.unary(symbols)?; // get any leading - or 'not'
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            self.skip_all_newlines();
            let right = self.unary(symbols)?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        if self.matches(&[TokenType::Not, TokenType::Minus]) {
            let operator = self.previous();
            self.skip_all_newlines();
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
            } else if self.matches(&[LeftBracket]) {
                // An array or hash lookup
                expr = self.finish_lookup(expr, symbols)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_lookup(&mut self, callee: Expr, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        use TokenType::*;
        if !self.check(&RightBracket) {
            let index = self.expression(symbols)?;
            let right_bracket =
                self.consume(RightBracket, "expect ']' to complete lookup expression.")?;
            Ok(Expr::lookup(callee, right_bracket, index))
        } else {
            let parse_error = parse_err(
                &self.previous(),
                "Lookup expression needs an index but was empty.",
            );
            self.error(parse_error.clone());
            Err(parse_error)
        }
    }

    fn finish_call(&mut self, callee: Expr, symbols: &SymbolTable) -> Result<Expr, ParseError> {
        use TokenType::*;
        let mut args: Vec<Expr> = Vec::new();
        self.skip_if_newline();
        if !self.check(&RightParen) {
            loop {
                let next_arg = self.expression(symbols)?;
                args.push(next_arg);
                if args.len() >= 255 {
                    let parse_error =
                        parse_err(&self.previous(), "Exceeded maximum arguments to function.");
                    self.error(parse_error.clone());
                    return Err(parse_error);
                }
                if !self.matches(&[Comma]) {
                    self.skip_if_newline();
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
                self.skip_all_newlines();
                let expr = self.expression(symbols)?;
                self.skip_all_newlines();
                self.consume(RightParen, "Expect ')' after expression.")?;
                Ok(Expr::grouping(expr))
            }
            LeftBracket => {
                self.advance();
                let mut elements = Vec::new();
                self.consume(LeftBracket, "expect '['")?;
                let mut expr = self.expression(symbols)?;
                elements.push(expr);
                while self.matches(&[Comma]) {
                    self.skip_if_newline();
                    expr = self.expression(symbols)?;
                    elements.push(expr);
                }
                self.skip_if_newline();
                let right_bracket =
                    self.consume(RightBracket, "expect ']' to close an array literal.")?;
                Ok(Expr::array(right_bracket, elements))
            }
            _ => {
                let l = self.peek().line;
                let c = self.peek().column;
                let type_name = self.peek().token_type.print();
                let message = format!("Error parsing primary expression at {}, {}. Found {} but expected a number, string, true, false, or nil.",
					l,c,&type_name);
                self.advance(); // move past the bad token
                Err(parse_err(&self.peek(), &message))
            }
        }
    }
}
