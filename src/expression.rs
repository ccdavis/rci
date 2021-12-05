use crate::lex::Token;
use crate::lex::TokenType;
use crate::operations;
use std::rc::Rc;

pub struct EvaluationError {
    pub message: String,
}

pub trait Node {
    // return the name  of the operation if any and
    // associated expressions.
    fn print(&self) -> String;
    fn evaluate(&self) -> Result<TokenType, EvaluationError>;
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

    fn evaluate(&self) -> Result<TokenType, EvaluationError> {
        use TokenType::*;
        let left_value = self.left.evaluate()?;
        let right_value = self.right.evaluate()?;
        let result = match self.operator.token_type {			
            Minus => operations::subtract(left_value, right_value),
            Slash => operations::divide(left_value, right_value),
            Star => operations::multiply(left_value, right_value),
            Plus => operations::add(&left_value, &right_value),
            Greater => operations::compare_gt(left_value, right_value),
            Less => operations::compare_lt(left_value, right_value),
            GreaterEqual => operations::compare_gte(left_value, right_value),
            LessEqual => operations::compare_lte(left_value, right_value),
            LessGreater => operations::not_equal(left_value, right_value),
            Equal => operations::equal(left_value, right_value),
            _ => Err(format!(
                    "Operation {} not supported yet!",
                    self.operator.token_type.print())),
                            
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

    fn evaluate(&self) -> Result<TokenType, EvaluationError> {
        self.expr.evaluate()
    }
}

#[derive(Clone, Debug)]
pub struct LiteralNode {
    value: Rc<TokenType>
}

impl Node for LiteralNode {
    fn print(&self) -> String {
        format!("{}", &self.value.print())
    }

    fn evaluate(&self) -> Result<TokenType, EvaluationError> {
        Ok(self.value)
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

    fn evaluate(&self) -> Result<TokenType, EvaluationError> {
        let right = self.expr.evaluate()?;
        match self.operator.token_type {
            TokenType::Minus => match right {
                TokenType::Number(n) => {
                    let new_value = TokenType::Number(-n);
                    Ok(new_value)
                }
                _ => {
                    let message = format!("Not a number value at {:?}", self.operator);
                    Err(EvaluationError { message })
                }
            },
            TokenType::Not => {
                if !matches!(TokenType::False,right) &&
					!matches!(TokenType::Nil, right) {
						Ok(TokenType::True)
                } else {
                    Ok(TokenType::False)
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
pub enum Expr {
    Binary(BinaryNode),
    Unary(UnaryNode),
    Grouping(GroupingNode),
    Literal(LiteralNode),
}

impl Expr {

	pub fn evaluate(&self) -> Result<TokenType,EvaluationError> {
		match self {
			Expr::Binary(n) => n.evaluate(),
			Expr::Unary(n) => n.evaluate(),
			Expr::Grouping(n) => n.evaluate(),
			Expr::Literal(n) => n.evaluate(),			
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

    pub fn literal(value: TokenType) -> Expr {
        Expr::Literal(LiteralNode {value: Rc::new(value) })
    }

    pub fn grouping(e: Expr) -> Expr {
        Expr::Grouping(GroupingNode { expr: Box::new(e) })
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Expr::Literal(_) => true,
            _ => false,
        }
    }

    pub fn literal_value(&self) -> TokenType {
        match self {
            Expr::Literal(l) => *l.value,
            _ => panic!("Not a literal value: {:?}", self),
        }
    }

    pub fn data_type(&self) -> TokenType {
		self.literal_value().data_type()                 
    }

    // Type check conveniences

    pub fn is_number(&self) -> bool {
        matches!(self.data_type(), TokenType::NumberType)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self.data_type(), TokenType::BooleanType)
    }

    pub fn is_string(&self) -> bool {
        matches!(self.data_type(), TokenType::StringType)
    }

    //  True or not Nil
    pub fn is_truthy(&self) -> bool {
        match self.data_type() {
            TokenType::True => true,
            TokenType::False => false,
            TokenType::Nil => false,
            _ => true,
        }
    }

    fn parenthesize(inside: String) -> String {
        "(".to_string() + &inside + ")"
    }

    pub fn print(&self) -> String {
        use Expr::*;
        let inside = match self {
            Binary(n) => n.print(),
            Unary(n) => n.print(),
            Literal(n) => n.print(),
            Grouping(n) => n.print(),

            _ => panic!("Not implemented"),
        };
        Expr::parenthesize(inside)
    }
}
