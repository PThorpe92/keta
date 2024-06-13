use crate::ast;
use crate::opcode::{Opcode, Value};

pub struct VM {
    pub registers: [u8; 32],
    pub pc: usize,
    pub program: Vec<Opcode>,
    pub stack_pointer: usize,
    pub stack: Vec<Value>,
    pub heap: Vec<u8>,
}

impl VM {
    pub fn new(prog: &[Opcode]) -> Self {
        VM {
            registers: [0; 32],
            pc: 0,
            stack_pointer: 0,
            program: prog.to_vec(),
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }

    fn fetch(&mut self) -> Opcode {
        let opcode = self.program[self.pc];
        self.pc += 1;
        opcode
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack underflow")
    }

    pub fn run(&mut self) {
        while self.pc < self.program.len() {
            self.execute(self.fetch());
        }
    }

    fn execute(&mut self, opcode: Opcode) {
        match opcode {
            Opcode::Push(val) => {
                self.push(val);
            }
            Opcode::Load(pos) => {
                let val = self.registers[pos];
                self.push(Value::Int(val as i64));
            }
            Opcode::Pop => {
                self.pop();
            }
            Opcode::Jump(pos) => {
                self.pc = pos;
            }
            Opcode::And => {
                let a = self.pop();
                let b = self.pop();
                match (a, b) {
                    (Value::Bool(a), Value::Bool(b)) => {
                        self.push(Value::Bool(a && b));
                    }
                    (Value::Int(a), Value::Int(b)) => {
                        self.push(Value::Int(a & b));
                    }
                    _ => panic!("Invalid operands for AND"),
                }
            }
            Opcode::Or => {
                let a = self.pop();
                let b = self.pop();
                match (a, b) {
                    (Value::Bool(a), Value::Bool(b)) => {
                        self.push(Value::Bool(a || b));
                    }
                    (Value::Int(a), Value::Int(b)) => {
                        self.push(Value::Int(a | b));
                    }
                    _ => panic!("Invalid operands for OR"),
                }
            }
            Opcode::Not => {
                let a = self.pop();
                match a {
                    Value::Bool(a) => {
                        self.push(Value::Bool(!a));
                    }
                    Value::Int(a) => {
                        self.push(Value::Int(!a));
                    }
                    _ => panic!("Invalid operand for NOT"),
                }
            }
            Opcode::Jump(pos) => {
                self.pc = pos;
            }
            Opcode::JumpIfTrue(pos) => {
                let a = self.pop();
                match a {
                    Value::Bool(true) => {
                        self.pc = pos;
                    }
                    _ => {}
                }
            }
        }
    }
}
