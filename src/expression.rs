use crate::environment::Environment;
use crate::lex::Token;
use crate::lex::TokenType;
use crate::operations;
use crate::types::ReturnValue;
use crate::types::DataValue;
use crate::types::DataType;
use std::rc::Rc;

pub struct EvaluationError {
    pub message: String,
}

pub trait Node {
    // return the name  of the operation if any and
    // associated expressions.
    fn print(&self) -> String;
    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError>;
}

#[derive(Clone, Debug)]
struct BinaryNode {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

impl Node for BinaryNode {
    fn print(&self) -> String {
        format!(
            "{} {} {}",
            &self.operator.print(),
            self.left.print(),
            self.right.print()
        )
    }

    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {
        use TokenType::*;
        let left = self.left.evaluate(envr)?;
        let right = self.right.evaluate(envr)?;
        let left_value = left.get();
        let right_value = right.get();
        let result = match self.operator.token_type {
            Plus => operations::add(left_value, right_value),
            Minus => operations::subtract(left_value, right_value),
            Slash => operations::divide(left_value, right_value),
            Star => operations::multiply(left_value, right_value),
            Greater => operations::compare_gt(left_value, right_value),
            Less => operations::compare_lt(left_value, right_value),
            GreaterEqual => operations::compare_gte(left_value, right_value),
            LessEqual => operations::compare_lte(left_value, right_value),
            LessGreater => operations::not_equal(left_value, right_value),
            Equal => operations::equal(left_value, right_value),

            _ => Err(format!(
                "Operation {} not supported yet!",
                self.operator.token_type.print()
            )),
        }; // match

        // If there's an error, add the source location from the token
        match result {
            Ok(r) => Ok(r),
            Err(message) => {
                let eval_err = format!(
                    "Error performing '{}' operation at {}, {}: {}",
                    &self.operator.token_type.print(),
                    self.operator.line,
                    self.operator.column,
                    &message
                );
                Err(EvaluationError { message: eval_err })
            }
        }
    }
} // impl

#[derive(Clone, Debug)]
pub struct GroupingNode {
    expr: Box<Expr>,
}

impl Node for GroupingNode {
    fn print(&self) -> String {
        format!("Grouping: {}", &self.expr.print())
    }

    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {
        self.expr.evaluate(envr)
    }
}

#[derive(Clone, Debug)]
pub struct LiteralNode {
    value: Rc<DataValue>,
}

impl Node for LiteralNode {
    fn print(&self) -> String {
        self.value.print()
    }

    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {        
		Ok(ReturnValue::Reference(Rc::clone(&self.value)))
    }
}

#[derive(Clone, Debug)]
pub struct UnaryNode {
    operator: Token,
    expr: Box<Expr>,
}

impl Node for UnaryNode {
    fn print(&self) -> String {
        format!("{} {}", &self.operator.print(), &self.expr.print())
    }

    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {
        let right = &self.expr.evaluate(envr)?;

        match self.operator.token_type {
            TokenType::Minus => match right.get() {
                DataValue::Number(n) => {
                    let new_value = ReturnValue::Value(DataValue::Number(-n));
                    Ok(new_value)
                }
                _ => {
                    let message = format!("Not a number value at {:?}", self.operator);
                    Err(EvaluationError { message })
                }
            },
            TokenType::Not => {
                if !matches!(TokenType::False, right) && !matches!(TokenType::Nil, right) {
                    Ok(ReturnValue::Value(DataValue::Bool(true)))
                } else {
                    Ok(ReturnValue::Value(DataValue::Bool(false)))
                }
            }
            _ => {
                let message = format!("Invalid unary expression at {:?}", &self.operator);
                Err(EvaluationError { message })
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableNode {
    pub name: Token,
    pub index: usize,
}

impl Node for VariableNode {
    fn print(&self) -> String {
        format!("var: {}", &self.name.print())
    }

    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {
        match self.name.token_type {
            TokenType::Identifier(ref name) => envr.get(&name),
            _ => Err(EvaluationError {
                message: "Can't look up non-identifiers".to_string(),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AssignmentNode {
    name: Token,
    value: Box<Expr>,
}

impl Node for AssignmentNode {
    fn print(&self) -> String {
        format!(
            "assign var: {} to  {}",
            &self.name.print(),
            &self.value.print()
        )
    }

    fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {
        let value_to_store = self.value.evaluate(envr)?;
        let var_name = match self.name.token_type {
            TokenType::Identifier(ref variable_name) => Ok(variable_name),
            _ => {
                let message = format!("Only assignment to simple identifiers permitted currently. Assignee token was {:?}", &self.name);
                Err(EvaluationError { message })
            }
        };

        let assignable_var = var_name?;
        envr.assign(assignable_var, value_to_store)?;
        Ok(ReturnValue::None)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinaryNode),
    Unary(UnaryNode),
    Grouping(GroupingNode),
    Literal(ReturnValue),
    Variable(VariableNode),
    Assignment(AssignmentNode),
}

impl Expr {
    pub fn evaluate(&self, envr: &mut Environment) -> Result<ReturnValue, EvaluationError> {
        match self {
            Expr::Binary(n) => n.evaluate(envr),
            Expr::Unary(n) => n.evaluate(envr),
            Expr::Grouping(n) => n.evaluate(envr),
            Expr::Variable(n) => n.evaluate(envr),
            Expr::Assignment(n) => n.evaluate(envr),
            Expr::Literal(ref value) => Ok(value.clone_or_increment_count()),
        }
    }

    pub fn binary(l: Expr, op: Token, r: Expr) -> Expr {
        let node = BinaryNode {
            left: Box::new(l),
            operator: op,
            right: Box::new(r),
        };
        Expr::Binary(node)
    }

    pub fn unary(op: Token, e: Expr) -> Expr {
        let node = UnaryNode {
            operator: op,
            expr: Box::new(e),
        };
        Expr::Unary(node)
    }

    pub fn literal(value: Token) -> Expr {
		let data_value = DataValue::from_token_type(&value.token_type);
        Expr::Literal(ReturnValue::new_ref(data_value))
    }

    pub fn grouping(e: Expr) -> Expr {
        Expr::Grouping(GroupingNode { expr: Box::new(e) })
    }

    pub fn variable(name: Token) -> Expr {
        Expr::Variable(VariableNode { name, index: 0 })
    }

    pub fn assignment(name: Token, new_value: Expr) -> Expr {
        Expr::Assignment(AssignmentNode {
            name,
            value: Box::new(new_value),
        })
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Expr::Literal(_))
    }

    pub fn data_type(&self) -> DataType{
        match self {
            Expr::Literal(ReturnValue::Reference(data_value)) => 
				DataType::from_data_value(&data_value),
            Expr::Literal(ReturnValue::Value(data_value)) => 
				DataType::from_data_value(&data_value),
            _ => panic!("Not a literal expression!"),
        }
    }

    // Type check conveniences

    pub fn is_number(&self) -> bool {
        matches!(self.data_type(), DataType::Number)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self.data_type(), DataType::Bool)
    }

    pub fn is_string(&self) -> bool {
        matches!(self.data_type(), DataType::Str)
    }

    
    fn parenthesize(inside: String) -> String {
        "(".to_string() + &inside + ")"
    }
	

    pub fn print(&self) -> String {
        use Expr::*;
        let inside = match self {
            Binary(n) => n.print(),
            Unary(n) => n.print(),
            Literal(ref n) => n.print(),
            Grouping(n) => n.print(),
            Variable(n) => n.print(),
            Assignment(n) => n.print(),

            _ => panic!("Not implemented"),
        };
        Expr::parenthesize(inside)
    }
}
