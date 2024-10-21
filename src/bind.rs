use crate::diag::{Message, Reporter};
use crate::list::{Id, List};
use crate::parse::ParseResult;
use crate::syntax::*;

pub struct BindResult {
    /// All symbols created by the bind step
    pub symbols: List<Symbol>,
    /// The symbol added for given syntax.
    /// This is `Node.symbol` from `centi-typescript`
    pub syntax_symbols: std::collections::HashMap<Id<Syntax>, Id<Symbol>>,
    /// All symbol tables, either locals or object (type) members.
    pub tables: List<Table>,
    /// The locals table for given syntax (Function or Signature)
    pub syntax_locals: std::collections::HashMap<Id<Syntax>, Id<Table>>,
    /// The root table id for the module
    pub module_table: Id<Table>,
}

pub fn bind(parsed: &ParseResult, reporter: &mut Reporter<'_>) -> BindResult {
    let mut context = BindContext {
        symbols: Default::default(),
        syntax_symbols: Default::default(),
        tables: Default::default(),
        syntax_locals: Default::default(),
        syntax: &parsed.syntax,
        reporter,
    };

    let mut module_binder = context.binder(None);

    for statement in parsed.syntax[parsed.module].statements.iter().copied() {
        module_binder.statement(statement);
    }

    let module_table = module_binder.table;

    BindResult {
        symbols: context.symbols,
        syntax_symbols: context.syntax_symbols,
        tables: context.tables,
        syntax_locals: context.syntax_locals,
        module_table,
    }
}

#[derive(Default)]
pub struct Symbol {
    pub declarations: Vec<Declaration>,
    pub object_members: Option<Id<Table>>,
}

impl Symbol {
    pub fn value_declaration(&self) -> Option<&Declaration> {
        self.declarations
            .iter()
            .find(|d| d.meaning == Meaning::Value)
    }

    pub fn type_declarations(&self) -> impl Iterator<Item = &Declaration> {
        self.declarations
            .iter()
            .filter(|d| d.meaning == Meaning::Type)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Declaration {
    pub syntax: Id<Syntax>,
    pub meaning: Meaning,
}

impl Declaration {
    pub fn ty(syntax: Id<Syntax>) -> Self {
        Self {
            syntax,
            meaning: Meaning::Type,
        }
    }

    pub fn value(syntax: Id<Syntax>) -> Self {
        Self {
            syntax,
            meaning: Meaning::Value,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Meaning {
    Type,
    Value,
}

pub struct Table {
    pub parent: Option<Id<Table>>,
    pub symbols: std::collections::HashMap<String, Id<Symbol>>,
}

impl Table {
    pub fn new(parent: Option<Id<Table>>) -> Self {
        Self {
            parent,
            symbols: Default::default(),
        }
    }
}

struct BindContext<'a, 'reports> {
    /// See [`BindResult::symbols`]
    symbols: List<Symbol>,
    /// See [`BindResult::syntax_symbols`]
    syntax_symbols: std::collections::HashMap<Id<Syntax>, Id<Symbol>>,
    /// See [`BindResult::tables`]
    tables: List<Table>,
    /// See [`BindResult::syntax_locals`]
    syntax_locals: std::collections::HashMap<Id<Syntax>, Id<Table>>,
    syntax: &'a SyntaxList,
    reporter: &'a mut Reporter<'reports>,
}

impl<'a, 'reports> BindContext<'a, 'reports> {
    fn add_decl(&mut self, syntax: Id<Syntax>, symbol: Id<Symbol>) {
        match self.syntax_symbols.entry(syntax) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(symbol);
            }
            std::collections::hash_map::Entry::Occupied(entry) => {
                panic!(
                    "syntax {syntax:?} already has declaration: {:?}",
                    entry.get()
                );
            }
        }
    }

    fn add_symbol(&mut self, declaration: Declaration) -> Id<Symbol> {
        let id = self.symbols.add_default();
        self.symbols[id].declarations.push(declaration);
        self.add_decl(declaration.syntax, id);
        id
    }

    fn binder(&mut self, parent_table: Option<Id<Table>>) -> Binder<'_, 'a, 'reports> {
        let table = self.tables.add(Table::new(parent_table));
        Binder {
            context: self,
            table,
        }
    }
}

struct Binder<'context, 'a, 'reports> {
    context: &'context mut BindContext<'a, 'reports>,
    table: Id<Table>,
}

impl<'context, 'a, 'reports> Binder<'context, 'a, 'reports> {
    fn add_symbol_for(&mut self, syntax: Id<Syntax>, meaning: Meaning) -> Id<Symbol> {
        self.context.add_symbol(Declaration { syntax, meaning })
    }

    fn declare_symbol(&mut self, name: Identifier, declaration: Declaration) {
        let Some(text) = &self.context.syntax[name] else {
            // failed to parse
            return;
        };
        match self.context.tables[self.table].symbols.get(text) {
            None => {
                let id = self.context.add_symbol(declaration);
                self.context.tables[self.table]
                    .symbols
                    .insert(text.clone(), id);
            }
            Some(&id) => {
                let symbol = &mut self.context.symbols[id];
                match symbol
                    .declarations
                    .iter()
                    .find(|d| d.meaning == declaration.meaning)
                {
                    Some(&other) => {
                        let span = self.context.syntax.span(declaration.syntax);
                        let other_span = self.context.syntax.span(other.syntax);
                        self.context
                            .reporter
                            .error(span, Message::Redeclared { name: text.clone() })
                            .note(other_span, Message::FirstDeclared);
                    }
                    None => {
                        symbol.declarations.push(declaration);
                        self.context.add_decl(declaration.syntax, id);
                    }
                }
            }
        };
    }

    fn statement(&mut self, syntax: Statement) {
        match syntax {
            Statement::Var(id) => {
                let data = &self.context.syntax[id];
                self.declare_symbol(data.name, Declaration::value(id.0));
                self.ty_opt(data.typename);
                self.expression(data.initializer);
            }
            Statement::TypeAlias(id) => {
                let decl = Declaration::ty(id.0);
                let data = &self.context.syntax[id];
                self.declare_symbol(data.name, decl);
                self.ty(data.typename);
            }
            Statement::ExpressionStatement(id) => {
                let data = &self.context.syntax[id];
                self.expression(data.expression);
            }
            Statement::Return(id) => {
                let data = &self.context.syntax[id];
                self.expression(data.expression);
            }
            Statement::Error => {}
        }
    }

    fn expression(&mut self, syntax: Expression) {
        match syntax {
            Expression::Identifier(_) => {}
            Expression::NumericLiteral(_) => {}
            Expression::StringLiteral(_) => {}
            Expression::Assignment(id) => {
                let data = &self.context.syntax[id];
                self.expression(data.value);
            }
            Expression::Object(id) => {
                let symbol = self.add_symbol_for(id.0, Meaning::Value);
                let data = &self.context.syntax[id];
                let mut binder = self.context.binder(Some(self.table));
                for property in data.properties.iter().copied() {
                    let decl = Declaration::value(property.0);
                    let data = &binder.context.syntax[property];
                    binder.declare_symbol(data.name, decl);
                    binder.expression(data.initializer);
                }
                self.context.symbols[symbol].object_members = Some(binder.table);
            }
            Expression::Function(id) => {
                self.add_symbol_for(id.0, Meaning::Value);
                let data = &self.context.syntax[id];
                let mut binder = self.context.binder(Some(self.table));
                binder.context.syntax_locals.insert(id.0, binder.table);
                binder.ty_opt(data.typename);
                if let Some(ps) = &data.type_parameters {
                    for p in ps.iter().copied() {
                        let decl = Declaration::ty(p.0);
                        let data = &binder.context.syntax[p];
                        binder.declare_symbol(data.name, decl);
                    }
                }
                for p in data.parameters.iter().copied() {
                    let decl = Declaration::value(p.0);
                    let data = &binder.context.syntax[p];
                    binder.declare_symbol(data.name, decl);
                    binder.ty_opt(data.typename);
                }
                for statement in data.body.iter().copied() {
                    binder.statement(statement);
                }
            }
            Expression::Call(id) => {
                let data = &self.context.syntax[id];
                self.expression(data.expression);
                if let Some(args) = &data.type_arguments {
                    for arg in args.iter().copied() {
                        self.ty(arg);
                    }
                }
                for arg in data.arguments.iter().copied() {
                    self.expression(arg);
                }
            }
            Expression::Error => {}
        }
    }

    fn ty_opt(&mut self, syntax: Option<Ty>) {
        if let Some(ty) = syntax {
            self.ty(ty);
        }
    }

    fn ty(&mut self, syntax: Ty) {
        match syntax {
            Ty::ObjectLiteralType(id) => {
                let symbol = self.add_symbol_for(id.0, Meaning::Type);
                let mut binder = self.context.binder(Some(self.table));
                let data = &binder.context.syntax[id];
                for prop in data.properties.iter().copied() {
                    let decl = Declaration::value(prop.0);
                    let data = &binder.context.syntax[prop];
                    binder.declare_symbol(data.name, decl);
                    binder.ty_opt(data.typename);
                }
                self.context.symbols[symbol].object_members = Some(binder.table);
            }
            Ty::Identifier(_) => {}
            Ty::SignatureDeclaration(id) => {
                self.add_symbol_for(id.0, Meaning::Type);
                let data = &self.context.syntax[id];
                let mut binder = self.context.binder(Some(self.table));
                binder.context.syntax_locals.insert(id.0, binder.table);
                if let Some(ps) = &data.type_parameters {
                    for p in ps.iter().copied() {
                        let decl = Declaration::ty(p.0);
                        let data = &binder.context.syntax[p];
                        binder.declare_symbol(data.name, decl);
                    }
                }
                for p in data.parameters.iter().copied() {
                    let decl = Declaration::value(p.0);
                    let data = &binder.context.syntax[p];
                    binder.declare_symbol(data.name, decl);
                    binder.ty_opt(data.typename);
                }
                if let Some(ty) = data.typename {
                    binder.ty(ty);
                }
            }
            Ty::Error => {}
        }
    }
}
