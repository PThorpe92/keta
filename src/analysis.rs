use crate::ast;
use crate::parser::Parser;

impl<'a> Parser<'a> {
    pub fn semantic_analysis(&mut self, program: &ast::Program) -> Result<(), String> {
        for node in &program.body {
            self.analyze_node(node)?;
        }
        Ok(())
    }

    fn analyze_node(&mut self, node: &ast::AstNode) -> Result<(), String> {
        match &node.node_type {
            ast::NodeType::Expression(expr) => self.analyze_expression(expr),
            ast::NodeType::Statement(stmt) => self.analyze_statement(stmt),
            ast::NodeType::Definition(def) => self.analyze_definition(def),
        }
    }

    fn analyze_expression(&mut self, expr: &ast::Expression) -> Result<(), String> {
        match expr {
            ast::Expression::Literal(_) => Ok(()),
            ast::Expression::Identifier(ident) => {
                if self.identifiers.variables.get(ident).is_none() {
                    Err(format!("Undeclared variable: {}", ident.0))
                } else {
                    Ok(())
                }
            }
            ast::Expression::BinaryOp(op) => {
                self.analyze_expression(&op.left)?;
                self.analyze_expression(&op.right)?;
                Ok(())
            }
            ast::Expression::UnaryOp(op) => self.analyze_expression(&op.expression),
            ast::Expression::FnCall(call) => {
                if self.identifiers.functions.get(&call.name).is_none() {
                    Err(format!("Undeclared function: {}", call.name.0))
                } else {
                    for arg in &call.arguments {
                        self.analyze_expression(arg)?;
                    }
                    Ok(())
                }
            }
            // Add other expression types as needed
            _ => Ok(()),
        }
    }

    fn analyze_statement(&mut self, stmt: &ast::Statement) -> Result<(), String> {
        match stmt {
            ast::Statement::Assignment(assign) => {
                if self.identifiers.variables.get(&assign.identifier).is_none() {
                    Err(format!("Undeclared variable: {}", assign.identifier.0))
                } else {
                    self.analyze_expression(&assign.expression)
                }
            }
            ast::Statement::If(if_stmt) => {
                self.analyze_expression(&if_stmt.condition)?;
                self.analyze_block(&if_stmt.consequence)?;
                if let Some(alternative) = &if_stmt.alternative {
                    self.analyze_block(alternative)?;
                }
                Ok(())
            }
            ast::Statement::ForLoop(for_loop) => {
                self.analyze_expression(&for_loop.iterable)?;
                self.analyze_block(&for_loop.body)?;
                Ok(())
            }
            ast::Statement::Return(ret) => self.analyze_expression(&ret.expression),
            ast::Statement::Expression(expr) => self.analyze_expression(expr),
            ast::Statement::Block(block) => self.analyze_block(block),
            ast::Statement::ImportStmt(_) => Ok(()),
            ast::Statement::FunctionDef(func) => {
                self.analyze_function_def(func)?;
                Ok(())
            }
        }
    }

    fn analyze_definition(&mut self, def: &ast::Definition) -> Result<(), String> {
        match def {
            ast::Definition::Variable(var_def) => {
                self.analyze_expression(&var_def.value)?;
                if self.identifiers.variables.contains_key(&var_def.name) {
                    Err(format!("Variable already defined: {}", var_def.name.0))
                } else {
                    let data = match var_def.value {
                        ast::Expression::Literal(ref lit) => lit.clone(),
                        ast::Expression::Identifier(ref ident) => {
                            self.identifiers
                                .variables
                                .get(ident)
                                .unwrap()
                                .clone()
                                .unwrap()
                                .value
                        }
                        _ => return Err(format!("Invalid value for variable: {}", var_def.name.0)),
                    };
                    self.identifiers.variables.insert(
                        var_def.name.clone(),
                        Some(ast::ScopedVar {
                            scope: self.current_scope,
                            value: data,
                        }),
                    );
                    Ok(())
                }
            }
            ast::Definition::Function(func_def) => {
                if self.identifiers.functions.contains_key(&func_def.name) {
                    Err(format!("Function already defined: {}", func_def.name.0))
                } else {
                    self.identifiers
                        .functions
                        .insert(func_def.name.clone(), self.current_scope);
                    self.analyze_function_def(func_def)
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

    fn analyze_function_def(&mut self, func_def: &ast::FunctionDef) -> Result<(), String> {
        for param in &func_def.parameters {
            self.analyze_data_type(&param.data_type)?;
        }
        self.analyze_block(&func_def.body)
    }

    fn analyze_block(&mut self, block: &ast::Block) -> Result<(), String> {
        for node in &block.body {
            self.analyze_node(node)?;
        }
        Ok(())
    }

    fn analyze_data_type(&self, data_type: &crate::token::DataType) -> Result<(), String> {
        //TODO: Implement type checking logic
        Ok(())
    }
}
