#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Push(Value),
    Pop,
    Load(usize),
    Store(usize),
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Not,
    Jump(usize),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Call(usize),
    Return,
    FunctionStart(usize),
    FunctionEnd,
    NoOp,
}

impl Opcode {
    pub fn is_jump(&self) -> bool {
        match self {
            Opcode::Jump(_) | Opcode::JumpIfTrue(_) | Opcode::JumpIfFalse(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug)]
pub enum Register {
    R00(Option<Value>),
    R01(Option<Value>),
    R02(Option<Value>),
    R03(Option<Value>),
    R04(Option<Value>),
    R05(Option<Value>),
    R06(Option<Value>),
    R07(Option<Value>),
    R08(Option<Value>),
    R09(Option<Value>),
    R10(Option<Value>),
    R11(Option<Value>),
    R12(Option<Value>),
}
