use crate::diag::{Message, Reporter};
use crate::list::{Id, List};
use crate::parse::ParseResult;
use crate::syntax::*;

pub struct BindResult {
    /// All symbols created by the bind step
    pub symbols: List<Symbol>,
    /// The symbol added for given syntax.
    /// This is `Node.symbol` from `centi-typescript`
    pub syntax_symbols: Table<Id<Syntax>, Id<Symbol>>,
    /// Symbol lookup tables
    pub scopes: List<Scope>,
    /// The locals scope table for given syntax item (Function or Signature),
    /// (not including the Module)
    pub syntax_locals: Table<Id<Syntax>, Id<Scope>>,
    /// The members for the given symbol for an object value or type
    pub symbol_members: Table<Id<Symbol>, Members>,
    /// The root table id for the Module
    pub module_locals: Id<Scope>,
}

pub fn bind(parsed: &ParseResult, reporter: &mut Reporter<'_>) -> BindResult {
    let mut context = BindContext {
        symbols: Default::default(),
        syntax_symbols: Default::default(),
        scopes: Default::default(),
        syntax_locals: Default::default(),
        symbol_members: Default::default(),
        syntax: &parsed.syntax,
        reporter,
    };

    let mut module_binder = context.binder(None);

    for statement in parsed.syntax[parsed.module].statements.iter().copied() {
        module_binder.statement(statement);
    }

    let module_locals = module_binder.scope;

    BindResult {
        symbols: context.symbols,
        syntax_symbols: context.syntax_symbols,
        scopes: context.scopes,
        syntax_locals: context.syntax_locals,
        symbol_members: context.symbol_members,
        module_locals,
    }
}

#[derive(Clone, Default)]
pub struct Symbol {
    pub declarations: Vec<Declaration>,
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

#[derive(Clone)]
pub struct Table<K, V>(pub std::collections::HashMap<K, V>);

impl<K, V> std::ops::Index<K> for Table<K, V>
where
    K: Eq + std::hash::Hash,
{
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.0[&index]
    }
}

impl<K, V> std::ops::IndexMut<K> for Table<K, V>
where
    K: Eq + std::hash::Hash,
    V: Default,
{
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        self.0.entry(index).or_default()
    }
}

impl<K, V> Default for Table<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Clone, Default)]
pub struct Scope {
    pub parent: Option<Id<Scope>>,
    pub symbols: Table<String, Id<Symbol>>,
}

impl Scope {
    pub fn new(parent: Option<Id<Scope>>) -> Self {
        Self {
            parent,
            symbols: Default::default(),
        }
    }
}

#[derive(Clone, Default)]
pub struct Members {
    pub items: Vec<(String, Id<Symbol>)>,
}

struct BindContext<'a, 'reports> {
    /// See [`BindResult::symbols`]
    symbols: List<Symbol>,
    /// See [`BindResult::syntax_symbols`]
    syntax_symbols: Table<Id<Syntax>, Id<Symbol>>,
    /// See [`BindResult::locals`]
    scopes: List<Scope>,
    /// See [`BindResult::syntax_locals`]
    syntax_locals: Table<Id<Syntax>, Id<Scope>>,
    /// See [`BindResult::symbol_members`]
    symbol_members: Table<Id<Symbol>, Members>,
    /// Parsed syntax
    syntax: &'a SyntaxList,
    /// Error reporting
    reporter: &'a mut Reporter<'reports>,
}

impl<'a, 'reports> BindContext<'a, 'reports> {
    fn add_decl(&mut self, syntax: Id<Syntax>, symbol: Id<Symbol>) {
        match self.syntax_symbols.0.entry(syntax) {
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

    fn binder(&mut self, parent_scope: Option<Id<Scope>>) -> Binder<'_, 'a, 'reports> {
        let table = self.scopes.add(Scope::new(parent_scope));
        Binder {
            context: self,
            scope: table,
        }
    }
}

struct Binder<'context, 'a, 'reports> {
    context: &'context mut BindContext<'a, 'reports>,
    scope: Id<Scope>,
}

impl<'context, 'a, 'reports> Binder<'context, 'a, 'reports> {
    fn declare_symbol(&mut self, name: Identifier, declaration: Declaration) {
        let Some(text) = &self.context.syntax[name] else {
            // failed to parse
            return;
        };
        match self.context.scopes[self.scope].symbols.0.get(text) {
            None => {
                let id = self.context.add_symbol(declaration);
                self.context.scopes[self.scope]
                    .symbols
                    .0
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
                let symbol = self.context.add_symbol(Declaration::value(id.0));
                let mut members = Members::default();
                let data = &self.context.syntax[id];
                for property in data.properties.iter().copied() {
                    let member = self.context.add_symbol(Declaration::value(property.0));
                    let data = &self.context.syntax[property];
                    if let Some(name) = &self.context.syntax[data.name] {
                        members.items.push((name.to_string(), member));
                    }
                    self.expression(data.initializer);
                }
                self.context.symbol_members.0.insert(symbol, members);
            }
            Expression::Function(id) => {
                self.context.add_symbol(Declaration::value(id.0));
                let data = &self.context.syntax[id];
                let mut binder = self.context.binder(Some(self.scope));
                binder.context.syntax_locals.0.insert(id.0, binder.scope);
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
                let symbol = self.context.add_symbol(Declaration::ty(id.0));
                let mut members = Members::default();
                let data = &self.context.syntax[id];
                for prop in data.properties.iter().copied() {
                    let member = self.context.add_symbol(Declaration::value(prop.0));
                    let data = &self.context.syntax[prop];
                    if let Some(name) = &self.context.syntax[data.name] {
                        members.items.push((name.to_string(), member));
                    }
                    self.ty_opt(data.typename);
                }
                self.context.symbol_members.0.insert(symbol, members);
            }
            Ty::Identifier(_) => {}
            Ty::SignatureDeclaration(id) => {
                self.context.add_symbol(Declaration::ty(id.0));
                let data = &self.context.syntax[id];
                let mut binder = self.context.binder(Some(self.scope));
                binder.context.syntax_locals.0.insert(id.0, binder.scope);
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
