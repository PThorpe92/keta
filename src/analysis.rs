use nu_ansi_term::{Color, Style};

use crate::ast::{self, EvaluationContext};
use crate::parser::Parser;
use crate::token::{self, DataType};

impl<'a> Parser<'a> {
    pub fn analyze_node(
        &self,
        node: &ast::AstNode,
        ctx: &mut EvaluationContext,
    ) -> Result<(), String> {
        match &node.node_type {
            ast::NodeType::Expression(expr) => self.analyze_expression(expr, ctx),
            ast::NodeType::Statement(stmt) => self.analyze_statement(stmt, ctx),
            ast::NodeType::Definition(def) => self.analyze_definition(def, ctx),
        }
    }

    fn analyze_expression(
        &self,
        expr: &ast::Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<(), String> {
        match expr {
            ast::Expression::Literal(_) => Ok(()),
            ast::Expression::Identifier(ident) => {
                if !ctx.variables.contains_key(ident) {
                    panic!(
                        "Undeclared variable: {}",
                        Style::new().fg(Color::Red).bold().paint(ident.0.clone()),
                    )
                } else {
                    Ok(())
                }
            }
            ast::Expression::BinaryOp(op) => {
                self.analyze_expression(&op.left, ctx)?;
                self.analyze_expression(&op.right, ctx)?;
                Ok(())
            }
            ast::Expression::UnaryOp(op) => self.analyze_expression(&op.expression, ctx),
            ast::Expression::FnCall(call) => {
                if ctx.functions.contains_key(&call.name) {
                    Err(format!("Undeclared function: {}", call.name.0))
                } else {
                    for arg in &call.arguments {
                        self.analyze_expression(arg, ctx)?;
                    }
                    Ok(())
                }
            }
            ast::Expression::IfStatement(ifstmt) => {
                self.analyze_expression(&ifstmt.condition, ctx)?;
                self.analyze_block(&ifstmt.consequence, ctx)?;
                if let Some(alternative) = &ifstmt.alternative {
                    self.analyze_block(alternative, ctx)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn analyze_statement(
        &self,
        stmt: &ast::Statement,
        ctx: &mut EvaluationContext,
    ) -> Result<(), String> {
        match stmt {
            ast::Statement::Assignment(assign) => {
                if !ctx.variables.contains_key(&assign.identifier) {
                    Err(format!("Undeclared variable: {}", assign.identifier.0))
                } else {
                    self.analyze_expression(&assign.expression, ctx)
                }
            }
            ast::Statement::If(if_stmt) => {
                self.analyze_expression(&if_stmt.condition, ctx)?;
                self.analyze_block(&if_stmt.consequence, ctx)?;
                if let Some(alternative) = &if_stmt.alternative {
                    self.analyze_block(alternative, ctx)?;
                }
                Ok(())
            }
            ast::Statement::ForLoop(for_loop) => {
                self.analyze_expression(&for_loop.iterable, ctx)?;
                self.analyze_block(&for_loop.body, ctx)?;
                Ok(())
            }
            ast::Statement::Return(ret) => self.analyze_expression(&ret.expression, ctx),
            ast::Statement::Expression(expr) => self.analyze_expression(expr, ctx),
            ast::Statement::Block(block) => self.analyze_block(block, ctx),
            ast::Statement::ImportStmt(_) => Ok(()),
            ast::Statement::FunctionDef(func) => {
                self.analyze_function_def(func, ctx)?;
                Ok(())
            }
        }
    }

    fn analyze_definition(
        &self,
        def: &ast::Definition,
        ctx: &mut EvaluationContext,
    ) -> Result<(), String> {
        match def {
            ast::Definition::Variable(var_def) => {
                self.analyze_expression(&var_def.value, ctx)?;
                let data = match var_def.value {
                    ast::Expression::Literal(ref lit) => lit.clone(),
                    ast::Expression::Identifier(ref ident) => {
                        ctx.variables
                            .get(ident)
                            .unwrap_or(&None)
                            .clone()
                            .unwrap()
                            .value
                    }
                    _ => return Err(format!("Invalid value for variable: {}", var_def.name.0)),
                };
                ctx.variables.insert(
                    var_def.name.clone(),
                    Some(ast::ScopedVar {
                        scope: self.current_scope,
                        value: data,
                    }),
                );
                Ok(())
            }
            ast::Definition::Function(func_def) => {
                if ctx.functions.contains_key(&func_def.name) {
                    Err(format!("Function already defined: {}", func_def.name.0))
                } else {
                    ctx.functions
                        .insert(func_def.name.clone(), self.current_scope);
                    self.analyze_function_def(func_def, ctx)
                }
            }
            ast::Definition::Struct(struct_def) => {
                for field in &struct_def.fields {
                    self.analyze_data_type(&field.data_type)?;
                }
                Ok(())
            }
        }
    }

    fn analyze_function_def(
        &self,
        func_def: &ast::FunctionDef,
        ctx: &mut EvaluationContext,
    ) -> Result<(), String> {
        for param in &func_def.parameters {
            self.analyze_data_type(&param.data_type)?;
        }
        self.analyze_block(&func_def.body, ctx)
    }

    fn analyze_function_call(
        &self,
        func_call: &ast::FnCall,
        ctx: &mut EvaluationContext,
    ) -> Result<(), String> {
        if !ctx.functions.contains_key(&func_call.name) {
            Err(format!("Undeclared function: {}", func_call.name.0))
        } else {
            if let Some(fn_def) = self.program.find_function(&func_call.name.0) {
                fn_def
                    .parameters
                    .iter()
                    .zip(func_call.arguments.iter())
                    .for_each(|(param, arg)| {
                        if param.data_type != arg.data_type {
                            panic!(
                                "Type mismatch: Expected {} but got {}",
                                param.data_type, arg.data_type
                            )
                        }
                    });
            }
            Ok(())
        }
    }

    fn evaluate_expression(
        &self,
        expr: &ast::Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<ast::Literal, String> {
        match expr {
            ast::Expression::Literal(lit) => Ok(lit.clone()),
            ast::Expression::Identifier(ident) => {
                if let Some(var) = ctx.variables.get(ident) {
                    if let Some(var) = var {
                        Ok(var.value.clone())
                    } else {
                        Err(format!("Variable {} is not initialized", ident.0))
                    }
                } else {
                    Err(format!("Undeclared variable: {}", ident.0))
                }
            }
            ast::Expression::BinaryOp(op) => {
                let left = self.evaluate_expression(&op.left, ctx)?;
                let right = self.evaluate_expression(&op.right, ctx)?;
                match op.operator {
                    ast::BinaryOperator::Add => {
                       if let DataType::Integer(lhs) = left.0 {
                       if let DataType::Integer(rhs) = right.0 {
                         Ok(ast::Literal(DataType::Integer(lhs + rhs)))
                        }
                      } else if let DataType::Float(lhs) = left.0 {
                        if let DataType::Float(rhs) = right.0 {
                        Ok(ast::Literal(DataType::Float(lhs.parse::<f64>()? + rhs.parse::<f64>()?)))
                        }
                    }
                     }
                    ast::BinaryOperator::Subtract => {
                       if let DataType::Integer(lhs) = left.0 {
                       if let DataType::Integer(rhs) = right.0 {
                         Ok(ast::Literal(DataType::Integer(lhs - rhs)))
                        }
                      } else if let DataType::Float(lhs) = left.0 {
                        if let DataType::Float(rhs) = right.0 {
                        Ok(ast::Literal(DataType::Float(lhs.parse::<f64>()? - rhs.parse::<f64>()?)))
                        }
                    }
                     }
                    token::Operator::Mul => {
                       if let DataType::Integer(lhs) = left.0 {
                       if let DataType::Integer(rhs) = right.0 {
                         Ok(ast::Literal(DataType::Integer(lhs - rhs)))
                        }
                      } else if let DataType::Float(lhs) = left.0 {
                        if let DataType::Float(rhs) = right.0 {
                        Ok(ast::Literal(DataType::Float(lhs.parse::<f64>()? - rhs.parse::<f64>()?)))
                        }
                    }
                     }
                    token::Operator::Divide => {
                       if let DataType::Integer(lhs) = left.0 {
                       if let DataType::Integer(rhs) = right.0 {
                         Ok(ast::Literal(DataType::Integer(lhs / rhs)))
                        }
                      }
                    }
                    token::Operator::Modulus => {
                        Ok(ast::Literal(DataType::Integer(left.number % right.number)))
                    }
                    token::Operator::Equal => {
                        Ok(ast::Literal(DataType::Boolean(left.number == right.number)))
                    }
                    token::Operator::NotEqual => {
                        Ok(ast::Literal(DataType::Boolean(left.number != right.number)))
                    }
                    token::Operator::GreaterThan => {
                        Ok(ast::Literal(DataType::Boolean(left.number > right)))
                    }
                    token::Operator::LessThan => {
                        Ok(ast::Literal(DataType::Boolean(left.number < right.number)))
                    }
                    token::Operator::GreaterThanOrEqual => {
                        Ok(ast::Literal(DataType::Boolean(left.number >= right.number)))
                    }
                    token::Operator::LessThanOrEqual => {
                        Ok(ast::Literal(DataType::Boolean(left.number <= right.number)))
                    }
                    _ => Err("Invalid binary operator".to_string()),
                }
            }
            ast::Expression::UnaryOp(op) => {
                let expr = self.evaluate_expression(&op.expression, ctx)?;
                match op.operator {
                    token::Operator::NotEqual => Ok(ast::Literal(DataType::Boolean(!expr))),
                    ast::UnaryOp
    fn analyze_block(&self, block: &ast::Block, ctx: &mut EvaluationContext) -> Result<(), String> {
        for node in &block.body {
            self.analyze_node(node, ctx)?;
        }
        Ok(())
    }

    fn analyze_data_type(&self, data_type: &crate::token::DataType) -> Result<(), String> {
        //TODO: Implement type checking logic
        Ok(())
    }
}
