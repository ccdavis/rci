use crate::lex::Token;
use crate::lex::TokenType;
use crate::types::DataType;
use crate::types::DataValue;
use crate::types::DeclarationType;
use crate::types::UserType;
use std::collections::HashMap;

const TRACE: bool = false;
#[derive(Clone, Debug)]
pub struct SymbolTableEntry {
    pub entry_number: usize, // The entry in the current table when this entry was added
    pub name: String,
    pub module: Vec<String>,
    pub alias_for: Option<String>,
    pub is_arg: bool,

    // Where the symbol was declared in the source
    pub location: Option<Token>, // use the col and line
    pub data_type: DataType,
    pub data_value: DataValue, // some will be DataValue::None
    pub entry_type: DeclarationType,

    pub size: usize,             // compile-time number of items if a collection type
    pub contains_type: DataType, // type inside of collection if any
    pub indexed_by_type: DataType,

    // For record / class members
    pub fields: Vec<SymbolTableEntry>,
}

impl SymbolTableEntry {
    // TODO: Change these init methods to take the Declaration / Statement Node struct types

    pub fn fully_qualified_name(&self) -> String {
        let sep = TokenType::At.print();
        self.module.join(&sep) + &sep + &self.name
    }

    pub fn new_type(
        entry_number: usize,
        location: &Token,
        name: &str,
        data_type: &DataType,
        data_value: &DataValue, // default value if any
        module: Vec<String>,
    ) -> Self {
        Self {
            entry_number,
            location: Some(location.clone()),
            name: name.to_owned(),
            module,
            alias_for: None,
            is_arg: false,
            entry_type: DeclarationType::Type,
            data_type: data_type.clone(),
            data_value: data_value.clone(),
            size: 1,
            contains_type: DataType::Empty,
            indexed_by_type: DataType::Empty,
            fields: Vec::new(),
        }
    }

    pub fn new_var(
        entry_number: usize,
        location: &Token,
        name: &str,
        data_type: &DataType,
        data_value: &DataValue,
        module: Vec<String>,
    ) -> Self {
        Self {
            entry_number,
            location: Some(location.clone()),
            name: name.to_owned(),
            module,
            alias_for: None,
            is_arg: false,
            entry_type: DeclarationType::Var,
            data_type: data_type.clone(),
            data_value: data_value.clone(),
            size: 1,
            contains_type: DataType::Empty,
            indexed_by_type: DataType::Empty,
            fields: Vec::new(),
        }
    }

    pub fn new_val(
        entry_number: usize,
        location: &Token,
        name: &str,
        data_type: &DataType,
        data_value: &DataValue,
        module: Vec<String>,
    ) -> Self {
        Self {
            entry_number,
            location: Some(location.clone()),
            name: name.to_owned(),
            module,
            alias_for: None,
            is_arg: false,
            entry_type: DeclarationType::Val,
            data_type: data_type.clone(),
            data_value: data_value.clone(),
            size: 1,
            contains_type: DataType::Empty,
            indexed_by_type: DataType::Empty,
            fields: Vec::new(),
        }
    }

    pub fn new_copy(
        entry_number: usize,
        location: &Token,
        name: &str,
        data_type: &DataType,
        data_value: &DataValue,
        module: Vec<String>,
    ) -> Self {
        Self {
            entry_number,
            location: Some(location.clone()),
            name: name.to_owned(),
            module,
            alias_for: None,
            is_arg: false,
            entry_type: DeclarationType::Cpy,
            data_type: data_type.clone(),
            data_value: data_value.clone(),
            size: 1,
            contains_type: DataType::Empty,
            indexed_by_type: DataType::Empty,
            fields: Vec::new(),
        }
    }

    pub fn new_param(
        entry_number: usize,
        decl_type: DeclarationType,
        name: Token,
        data_type: DataType,
        module: Vec<String>,
    ) -> SymbolTableEntry {
        let location = name.clone();
        let param_name = name.identifier_string();

        let mut entry = match decl_type {
            DeclarationType::Var => SymbolTableEntry::new_var(
                entry_number,
                &location,
                &param_name,
                &data_type,
                &DataValue::Unresolved,
                module,
            ),
            DeclarationType::Val => SymbolTableEntry::new_val(
                entry_number,
                &location,
                &param_name,
                &data_type,
                &DataValue::Unresolved,
                module,
            ),
            DeclarationType::Cpy => SymbolTableEntry::new_copy(
                entry_number,
                &location,
                &param_name,
                &data_type,
                &DataValue::Unresolved,
                module,
            ),
            _ => panic!(
                "Can't use decl type '{:?}' in a parameter definition at {}!",
                decl_type,
                &name.print()
            ),
        };
        entry.is_arg = true;
        entry
    }

    pub fn new_fun(
        entry_number: usize,
        location: Option<Token>,
        name: &str,
        alias_for: Option<String>,
        params: Vec<SymbolTableEntry>,
        data_type: &DataType,
        module: Vec<String>,
    ) -> Self {
        Self {
            entry_number,
            location,
            name: name.to_owned(),
            module,
            alias_for,
            is_arg: false,
            entry_type: DeclarationType::Fun,
            data_type: data_type.clone(),
            data_value: DataValue::Unresolved,
            size: 1,
            contains_type: DataType::Empty,
            indexed_by_type: DataType::Empty,
            fields: params,
        }
    }

    pub fn format_debug(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    pub entries: HashMap<String, SymbolTableEntry>,
}

#[derive(Clone, Debug)]
pub struct NotDeclaredError {
    pub message: String,
}

impl SymbolTable {
    pub fn global() -> Self {
        Self {
            entries: HashMap::new(),
            outer: None,
        }
    }

    pub fn extend(&mut self) -> SymbolTable {
        SymbolTable {
            entries: HashMap::new(),
            outer: Some(Box::new(self.clone())),
        }
    }

    pub fn add(&mut self, st: SymbolTableEntry) -> bool {
        if self.entries.contains_key(&st.name) {
            false
        } else {
            self.entries.insert(st.name.to_owned(), st);
            true
        }
    }

    // Distance in scopes from the current scope, or None if it doesn't exist.
    // For use resolving variable scopes for assignment and variable expressions.
    // Also returns the index of the variable at that scope.
    //
    // It's possible to return None if the variable was in global scope but the parser
    // hasn't reached it yet.
    pub fn distance_and_index(&self, name: &str, hops: usize) -> (Option<usize>, Option<usize>) {
        match self.entries.get(name) {
            Some(ste) => (Some(hops), Some(ste.entry_number)),
            None => {
                if let Some(outer_scope) = &self.outer {
                    outer_scope.distance_and_index(name, hops + 1)
                } else {
                    (None, None)
                }
            }
        }
    }

    pub fn lookup(&self, name: &str) -> Result<&SymbolTableEntry, NotDeclaredError> {
        match self.entries.get(name) {
            Some(ste) => Ok(ste),
            None => {
                if let Some(outer_scope) = &self.outer {
                    outer_scope.lookup(name)
                } else {
                    let message = format!("{} not declared.", name);
                    Err(NotDeclaredError { message })
                }
            }
        }
    } // fn

    pub fn print_debug(&self) {
        for (k, v) in &self.entries {
            println!("{}  ->  {}", &k, &v.format_debug());
            if let Some(outer_scope) = &self.outer {
                println!("Outer scope: -------->");
                outer_scope.print_debug();
            }
        }
    }
}

pub fn data_type_for_symbol(
    symbols: &SymbolTable,
    name: &str,
) -> Result<DataType, NotDeclaredError> {
    let symbol_table_entry = symbols.lookup(name)?;
    let symbol_type = symbol_table_entry.data_type.clone();

    if TRACE {
        println!("Found symbol {}", &name);
    }
    if TRACE {
        println!("Type of variable is '{:?}'", symbol_table_entry.data_type);
    }

    if let DataType::User(user_type) = symbol_type {
        resolve_user_type(symbols, user_type.as_ref())
    } else {
        Ok(symbol_type)
    }
}

pub fn resolve_user_type(
    symbols: &SymbolTable,
    u: &UserType,
) -> Result<DataType, NotDeclaredError> {
    if let DataType::Unresolved = *u.definition {
        let ste = symbols.lookup(&u.name)?;
        match ste.data_type {
            DataType::User(ref actual) => Ok(*actual.definition.clone()),
            _ => Ok(ste.data_type.clone()),
        }
    } else {
        Ok(*u.definition.clone())
    }
}
