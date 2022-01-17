use core::str::Chars;

use std::collections::HashMap;
#[derive(PartialEq,Clone, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
	LeftBracket,
	RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,
    Colon,
    // One or two character tokens.
    LessGreater, // not equal
    ColonEqual,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Number(f64),
    Identifier(String),
    Str(String),
    Bool,

    Comment(String),
	
    NumberType,
    StringType,
    BooleanType,
    SetType,
	ArrayType,

    // keywords
    Set,
    Enum,
    In,
    Intersects,
    Intersection,
    Union,
    Difference,
    Complement,
    Break,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Not,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    Val,
    Cpy,
    While,

    ScanError(String),
    Eof,
}

impl TokenType {
    // For the token types carrying data, print their values.
    pub fn print_value(&self) -> String {
        use TokenType::*;
        match self {
            Identifier(name) => format!("{}", &name),
            Number(value) => format!("{}", value),
            Str(value) => format!("{}", &value),
            Comment(value) => format!("{}", &value),
            ScanError(msg) => format!("{}", &msg),
            _ => "".to_string(),
        }
    }

    // Prints only the name of the token type.     
    pub fn print(&self) -> String {
        use TokenType::*;
        let name = match self {
            LeftParen => "(",
            RightParen => ")",
            LeftBrace => "{",
            RightBrace => "}",
			LeftBracket => "[",
			RightBracket => "]",
            Comma => ",",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Dot => ".",
            SemiColon => ";",
            Slash => "/",
            Colon => ":",

            Less => "<",
            LessEqual => "<=",
            Equal => "=",
            ColonEqual => ":=",
            Greater => ">",
            GreaterEqual => ">=",

            Set => "set",
            Enum => "Enum",
            In => "in",
            Intersects => "intersects",
            Intersection => "intersection",
            Union => "union",
            Difference => "difference",
            Complement => "complement",
            Break => "break",
            And => "and",
            Or => "or",
            Class => "Class",
            Else => "else",
            False => "false",
            Fun => "fun",
            For => "for",
            If => "if",
            Nil => "nil",
            Not => "not",
            LessGreater => "<>",
            Print => "print",
            Return => "return",
            Super => "super",
            This => "this",
            True => "true",
            Var => "var",
            Val => "val",
            Cpy => "cpy",
            While => "while",

            // Type names
            NumberType => "number",
            StringType => "string",
            BooleanType => "boolean",
            SetType => "set",
			ArrayType => "array",

            // literals
            Number(_) => "Number",
            Str(_) => "String",
            Identifier(_) => "identifier",
            Bool => "bool",
            Comment(_) => "comment",
            _ => panic!("Unhandled {:?}", self),
        };
        name.to_string()
    }

    pub fn is_comparison_operator(&self) -> bool {
        use TokenType::*;
        match self {
            LessGreater | Equal | Greater | GreaterEqual | Less | LessEqual => true,
            _ => false,
        }
    }

    pub fn is_arithmetic_operator(&self) -> bool {
        use TokenType::*;
        match self {
            Plus | Minus | Slash | Star => true,
            _ => false,
        }
    }

    // Store this in the scanner struct for quick lookup
    pub fn reserved_words() -> HashMap<String, TokenType> {
        use TokenType::*;
        let words = vec![
            StringType,
            NumberType,
            BooleanType,
            SetType,
			ArrayType,
            Enum,
            In,
            Intersects,
            Intersection,
            Union,
            Difference,
            Complement,
            And,
            Break,
            Class,
            Else,
            False,
            Fun,
            For,
            If,
            Nil,
            Not,
            Or,
            Print,
            Return,
            Super,
            This,
            True,
            Var,
            Val,
            Cpy,
            While,
        ];

        let mut types_by_name: HashMap<String, TokenType> = HashMap::new();
        for w in words {
            types_by_name.insert(w.print(), w);
        }

        types_by_name
    }

    pub fn data_type(&self) -> TokenType {
        use TokenType::*;
        match self {
            Number(_) => NumberType,
            Str(_) => StringType,
            True => BooleanType,
            False => BooleanType,
            _ => panic!("Not a supported value type."),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub current: usize,
    pub line: usize,
    pub column: usize,
}

impl Token {
    // The point is to dump all the info including the line and column
    pub fn print(&self) -> String {
        format!("'{:?}' at {}, {}", &self.token_type, self.line, self.column)
    }

    pub fn pos(&self) -> String {
        format!("{}, {}", self.line, self.column)
    }

    pub fn is_comparison_operator(&self) -> bool {
        self.token_type.is_comparison_operator()
    }

    pub fn is_arithmetic_operator(&self) -> bool {
        self.token_type.is_arithmetic_operator()
    }

    pub fn identifier_string(&self) -> String {
        match self.token_type {
            TokenType::Identifier(ref i) => i.clone(),
            _ => {
                panic!("Should only call identifier_string() on a token you know is an identifier!")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scanner {
    text: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    reserved_words: HashMap<String, TokenType>,
}

impl Scanner {
    pub fn new(script: String) -> Self {
        Self {
            text: script.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            reserved_words: TokenType::reserved_words(),
        }
    }

    // place-holder
    fn error(&self, msg: String) {
        eprintln!("{}", &msg)
    }

    fn is_finished(&self) -> bool {
        self.current >= self.text.len()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        // The chars() iterator borrows the string data, but string is mutable
        // and so is 'self' here. So we can't do self.text.chars().__rust_force_expr!
        // Also, you simply cannot make an iterator like std::str::chars and the string
        // it iterates over owned by the same struct
        //
        // We want to hang on to the original text we're parsing because we want
        // to retrieve lines at random to present in error messages.
        //
        // See  https://stackoverflow.com/questions/47193584/is-there-an-owned-version-of-stringchars for
        // some more involved alternative solutions.
        //
        // The clone() here is fine except we double the memory.
        //
        // Another solution would be to only store a Vec<char> in the struct and index through it
        // or iterate as desired. That also takes extra memory compared to a single string.

        //
        //let text_to_iterate = self.text.clone();
        //let mut source = text_to_iterate.chars();

        while !self.is_finished() {
            self.start = self.current;
            match self.scan_token() {
                Err(msg) => self.error(msg),
                Ok(token) => tokens.push(token),
            }
        }

        tokens.push(self.make_token(TokenType::Eof).unwrap());

        tokens
            .into_iter()
            .filter(|t| match t.token_type {
                TokenType::Comment(_) => false,
                _ => true,
            })
            .collect()
    }

    fn make_token(&self, token_type: TokenType) -> Result<Token, String> {
        match token_type {
            TokenType::ScanError(msg) => Err(msg),
            _ => Ok(Token {
                token_type,
                start: self.start,
                current: self.current,
                line: self.line,
                column: self.column,
            }),
        }
    }

    fn scan_token(&mut self) -> Result<Token, String> {
        // Since with my design the match expression must return a token type
        // we can't use the match to match on whitespace characters;
        // it can't be used to advance the scanner past non-token characters.
        //
        // I include comments in the match because they are returned as tokens
        // for use in preprocessing, like annotations or for pretty-printing.
        self.skip_whitespace();
        let c = self.advance();
        let token_type = match c {
            '\0' => TokenType::Eof,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
			'[' => TokenType::LeftBracket,
			']' => TokenType::RightBracket,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '*' => TokenType::Star,
            '+' => TokenType::Plus,
            ';' => TokenType::SemiColon,
            '=' => TokenType::Equal,
            ':' => {
                if self.match_char('=') {
                    TokenType::ColonEqual
                } else {
                    TokenType::Colon
                }
            }
            '>' => {
                if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            '<' => {
                if self.match_char('=') {
                    TokenType::LessEqual
                } else if self.match_char('>') {
                    TokenType::LessGreater
                } else {
                    TokenType::Less
                }
            }
            '/' => {
                if self.match_char('/') {
                    let mut content = "".to_string();
                    while self.this_char() != '\n' && !self.is_finished() {
                        content.push(self.this_char());
                        self.advance();
                    }
                    TokenType::Comment(content)
                } else {
                    TokenType::Slash
                }
            }
            '"' => self.string_literal(),

            _ => {
                if self.is_digit(c) {
                    self.number_literal()
                } else if self.is_alpha(c) {
                    self.identifier()
                } else {
                    TokenType::ScanError(format!(
                        "Unrecognized character {} at {}, {}",
                        c, self.line, self.column
                    ))
                }
            }
        };
        self.make_token(token_type)
    }

    // This will be called after advance() so 'current' will be the char following
    // the one we got from advance() -- a look-ahead.
    fn match_char(&mut self, expected: char) -> bool {
        let not_matched = self.is_finished() || expected != self.text[self.current];
        if not_matched {
            return false;
        }
        self.current += 1;
        true
    }

    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn is_whitespace(&self, c: char) -> bool {
        c == ' ' || c == '\t' || c == '\n' || c == '\r'
    }

    fn skip_whitespace(&mut self) {
        while !self.is_finished() && self.is_whitespace(self.this_char()) {
            self.advance();
        }
        self.start = self.current;
    }

    fn this_char(&self) -> char {
        if self.is_finished() {
            return '\0';
        }
        self.text[self.current]
    }

    fn next_char(&self) -> char {
        if self.current + 1 >= self.text.len() {
            return '\0';
        }
        self.text[self.current + 1]
    }

    fn number_literal(&mut self) -> TokenType {
        let mut integer_literal = true;
        while self.is_digit(self.this_char()) {
            self.advance();
        }
        if self.this_char() == '.' && self.is_digit(self.next_char()) {
            integer_literal = false;
            self.advance(); // eat the "."
            while self.is_digit(self.this_char()) {
                self.advance();
            }
        }
        let content: String = self.text[self.start..self.current].into_iter().collect();
        let value: f64 = match content.parse() {
            Ok(valid) => valid,
            Err(msg) => {
                return TokenType::ScanError(format!("{} when parsing '{}'", msg, &content));
            }
        };

        TokenType::Number(value)
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '?' || c == '!'
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) -> TokenType {
        while self.is_alpha_numeric(self.this_char()) {
            self.advance();
        }
        let content: String = self.text[self.start..self.current].into_iter().collect();

        match self.reserved_words.get(&content) {
            Some(word) => word.clone(),
            None => TokenType::Identifier(content),
        }
    }

    // Consume content for string literal and return it
    // in the TokenType::Str varient.
    fn string_literal(&mut self) -> TokenType {
        // For better reporting on unterminated strings
        let starting_line = self.line;
        let starting_column = self.column;

        let mut content = "".to_string();
        while self.this_char() != '"' && !self.is_finished() {
            content.push(self.this_char());
            self.advance();
        }

        if self.is_finished() {
            let msg: String = format!(
                "Unterminated string starting at {}, {}",
                starting_line, starting_column
            );
            TokenType::ScanError(msg)
        } else {
            // Eat the second "
            self.advance();
            TokenType::Str(content)
        }
    }

    fn advance(&mut self) -> char {
        if self.is_finished() {
            return '\0';
        }
        let c = self.text[self.current];
        self.current += 1;
        self.column += 1;
        if '\n' == c {
            self.line += 1;
            self.column = 0;
        }
        c
    }
}
