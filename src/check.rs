use crate::bind::{BindResult, Meaning, Members, Scope, Symbol, Table};
use crate::diag::{Message, Reporter};
use crate::list::{Id, List};
use crate::parse::ParseResult;
use crate::source::SourceSpan;
use crate::syntax::*;

#[derive(Default)]
pub struct CheckResult {
    pub type_defs: List<TypeDef>,
    pub symbol_value_types: Table<Id<Symbol>, TypeId>,
    pub symbol_type_types: Table<Id<Symbol>, TypeId>,
}

#[derive(Clone)]
pub enum TypeDef {
    Object(ObjectTypeDef),
    TypeVariable { name: Identifier },
    Function(Signature),
}

#[derive(Clone)]
pub struct ObjectTypeDef {
    pub members: Table<String, MemberTypeDef>,
}

#[derive(Clone)]
pub struct MemberTypeDef {
    pub symbol: Id<Symbol>,
    pub ty: TypeId,
}

#[derive(Clone)]
pub struct Signature {
    pub type_parameters: Option<Vec<Id<Symbol>>>,
    pub parameters: Vec<Id<Symbol>>,
    pub return_type: TypeId,
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum TypeId {
    Void,
    Error,
    Any,
    Number,
    String,
    Def(Id<TypeDef>),
}

pub fn check(
    parsed: &ParseResult,
    bindings: &BindResult,
    reporter: &mut Reporter<'_>,
) -> CheckResult {
    let mut context = CheckContext {
        syntax: &parsed.syntax,
        symbols: bindings.symbols.clone(),
        syntax_symbols: &bindings.syntax_symbols,
        scopes: bindings.scopes.clone(),
        syntax_locals: &bindings.syntax_locals,
        symbol_members: &bindings.symbol_members,
        type_defs: Default::default(),

        symbol_value_types: Default::default(),
        symbol_type_types: Default::default(),

        reporter,
    };

    let mut checker = context.checker(bindings.module_locals);

    for statement in parsed.syntax[parsed.module].statements.iter().copied() {
        checker.statement(statement);
    }

    CheckResult {
        type_defs: context.type_defs,
        symbol_value_types: context.symbol_value_types,
        symbol_type_types: context.symbol_type_types,
    }
}

struct TypeMapper {
    sources: Vec<TypeId>,
    targets: Vec<TypeId>,
}

struct CheckContext<'a, 'reports> {
    // inputs:
    // ParseResult
    syntax: &'a SyntaxList,
    // BindResult
    symbols: List<Symbol>,
    syntax_symbols: &'a Table<Id<Syntax>, Id<Symbol>>,
    scopes: List<Scope>,
    syntax_locals: &'a Table<Id<Syntax>, Id<Scope>>,
    symbol_members: &'a Table<Id<Symbol>, Members>,
    // output:
    // CheckResult
    type_defs: List<TypeDef>,
    symbol_value_types: Table<Id<Symbol>, TypeId>,
    symbol_type_types: Table<Id<Symbol>, TypeId>,

    // errors
    reporter: &'a mut Reporter<'reports>,
}

impl<'a, 'reports> CheckContext<'a, 'reports> {
    fn checker(&mut self, scope: Id<Scope>) -> Checker<'_, 'a, 'reports> {
        Checker {
            context: self,
            scope,
        }
    }

    fn error(&mut self, span: SourceSpan, message: Message) -> TypeId {
        self.reporter.error(span, message);
        TypeId::Error
    }

    fn error_for(&mut self, syntax: Id<Syntax>, message: Message) -> TypeId {
        let span = self.syntax.span(syntax);
        self.error(span, message)
    }

    fn define_type(&mut self, type_def: TypeDef) -> TypeId {
        TypeId::Def(self.type_defs.add(type_def))
    }

    fn syntax_symbol(&self, syntax: Id<Syntax>) -> &Symbol {
        let id = self.syntax_symbols[syntax];
        &self.symbols[id]
    }
}

struct Checker<'context, 'a, 'reports> {
    context: &'context mut CheckContext<'a, 'reports>,
    scope: Id<Scope>,
}

impl Checker<'_, '_, '_> {
    fn statement(&mut self, syntax: Statement) -> TypeId {
        match syntax {
            Statement::Var(syntax) => {
                let data = &self.context.syntax[syntax];
                self.expression(data.initializer)
            }
            Statement::TypeAlias(syntax) => {
                let data = &self.context.syntax[syntax];
                self.ty(data.typename)
            }
            Statement::ExpressionStatement(syntax) => {
                let data = &self.context.syntax[syntax];
                self.expression(data.expression)
            }
            Statement::Return(syntax) => {
                let data = &self.context.syntax[syntax];
                self.expression(data.expression)
            }
            Statement::Error => TypeId::Error,
        }
    }

    fn expression(&mut self, syntax: Expression) -> TypeId {
        match syntax {
            Expression::Identifier(syntax) => self.identifier(syntax),
            Expression::NumericLiteral(_) => TypeId::Number,
            Expression::StringLiteral(_) => TypeId::String,
            Expression::Assignment(syntax) => {
                let data = &self.context.syntax[syntax];
                let left = self.identifier(data.name);
                let right = self.expression(data.value);
                if !self.is_assignable_to(right, left) {
                    self.context
                        .error_for(syntax.0, Message::Assign { left, right });
                }
                left
            }
            Expression::Object(syntax) => self.object(syntax),
            Expression::Function(syntax) => self.function(syntax),
            Expression::Call(syntax) => self.call(syntax),
            Expression::Error => TypeId::Error,
        }
    }

    fn identifier(&mut self, syntax: Identifier) -> TypeId {
        self.resolve(syntax, Meaning::Value)
    }

    fn object(&mut self, syntax: Object) -> TypeId {
        let data = &self.context.syntax[syntax];
        for p in data.properties.iter().copied() {
            self.property_initializer(p);
        }

        let symbol = self.context.syntax_symbols[syntax.0];
        let members = Table(
            self.context.symbol_members[symbol]
                .items
                .iter()
                .cloned()
                .map(|(name, symbol)| {
                    let ty = self.value_type_of_symbol(symbol);
                    (name, MemberTypeDef { symbol, ty })
                })
                .collect(),
        );
        self.context
            .define_type(TypeDef::Object(ObjectTypeDef { members }))
    }

    fn property_initializer(&mut self, syntax: PropertyInitializer) -> TypeId {
        let data = &self.context.syntax[syntax];
        self.expression(data.initializer)
    }

    fn function(&mut self, syntax: Function) -> TypeId {
        let locals = self.context.syntax_locals[syntax.0];
        let symbol = self.context.syntax_symbols[syntax.0];
        self.context.checker(locals).value_type_of_symbol(symbol)
    }

    fn call(&mut self, syntax: Call) -> TypeId {
        let data = &self.context.syntax[syntax];
        let ty = self.expression(data.expression);
        if data.type_arguments.is_some() {
            // todo
            return TypeId::Error;
        }

        let mut arg_types = vec![];
        for arg in data.arguments.iter().copied() {
            if let Some(id) = arg.id() {
                arg_types.push((id, self.expression(arg)));
            }
        }
        let TypeId::Def(function) = ty else {
            return self.context.error_for(syntax.0, Message::Call { ty });
        };
        let TypeDef::Function(def) = &self.context.type_defs[function] else {
            return self.context.error_for(syntax.0, Message::Call { ty });
        };
        // fixme: to avoid lifetime errors. Should be cheap as it's only ids, but i'm sure theres
        //        a better option here.
        let def = def.clone();

        if def.parameters.len() != arg_types.len() {
            self.context.error_for(
                syntax.0,
                Message::ArgumentCount {
                    parameter_count: def.parameters.len(),
                    argument_count: arg_types.len(),
                },
            );
        }
        for ((arg, ty), param) in arg_types
            .iter()
            .copied()
            .zip(def.parameters.iter().copied())
        {
            let parameter_type = self.value_type_of_symbol(param);
            if !self.is_assignable_to(ty, parameter_type) {
                let span = self.context.syntax.span(arg);
                self.context.reporter.error(
                    span,
                    Message::Argument {
                        parameter_type,
                        argument_type: ty,
                    },
                );
            }
        }

        def.return_type
    }

    fn instantiate_signature(&mut self, signature: &Signature, mapper: &TypeMapper) -> Signature {
        Signature {
            type_parameters: None,
            parameters: signature
                .parameters
                .iter()
                .copied()
                .map(|id| self.instantiate_symbol(id, mapper))
                .collect(),
            return_type: self.instantiate_type(signature.return_type, mapper),
        }
    }

    fn instantiate_type(&mut self, ty: TypeId, mapper: &TypeMapper) -> TypeId {
        match ty {
            TypeId::Def(id) => match &self.context.type_defs[id] {
                TypeDef::Function(def) => {
                    let def = def.clone();
                    let signature = self.instantiate_signature(&def, mapper);
                    self.context.define_type(TypeDef::Function(signature))
                }
                TypeDef::Object(def) => {
                    let def = ObjectTypeDef {
                        members: Table(
                            def.members
                                .0
                                .clone()
                                .into_iter()
                                .map(|(name, def)| {
                                    let symbol = self.instantiate_symbol(def.symbol, mapper);
                                    let ty = self.value_type_of_symbol(symbol);
                                    (name, MemberTypeDef { symbol, ty })
                                })
                                .collect(),
                        ),
                    };
                    self.context.define_type(TypeDef::Object(def))
                }
                TypeDef::TypeVariable { .. } => {
                    match mapper.sources.iter().position(|item| *item == ty) {
                        Some(index) => mapper.targets[index],
                        None => ty,
                    }
                }
            },
            ty => ty,
        }
    }

    fn instantiate_symbol(&mut self, id: Id<Symbol>, mapper: &TypeMapper) -> Id<Symbol> {
        let value_type = self.context.symbol_value_types.0.get(&id).copied();
        let type_type = self.context.symbol_type_types.0.get(&id).copied();
        let new_value_type = value_type.map(|ty| self.instantiate_type(ty, mapper));
        let new_type_type = type_type.map(|ty| self.instantiate_type(ty, mapper));
        if value_type != new_value_type || type_type != new_type_type {
            // create a duplicate symbol so we can attach the mapped types
            let symbol = self.context.symbols[id].clone();
            let id = self.context.symbols.add(symbol);
            if let Some(ty) = new_value_type {
                self.context.symbol_value_types.0.insert(id, ty);
            }
            if let Some(ty) = new_type_type {
                self.context.symbol_type_types.0.insert(id, ty);
            }
            id
        } else {
            id
        }
    }

    //
    // fn infer_type_arguments(
    //     type_parameters: Id<TypeDef>, // should all reference the TypeVariable variant?
    //     signature: SignatureDeclaration,
    //     arg_types: Vec<TypeId>,
    // ) -> Vec<TypeId> {
    //     todo!()
    // }

    // inlined
    // fn parameter(syntax: Parameter) -> TypeId;
    // fn type_parameter(syntax: TypeParameter) -> TypeId
    // fn body(body: Statement[], declared_type: Option<TypeId>) -> TypeId
    // fn for_each_return_statement(body: Statement: [], callback: Fn(syntax: Return))

    fn ty_opt(&mut self, syntax: Option<Ty>) -> TypeId {
        if let Some(syntax) = syntax {
            self.ty(syntax)
        } else {
            TypeId::Any
        }
    }

    fn ty(&mut self, syntax: Ty) -> TypeId {
        match syntax {
            Ty::Identifier(syntax) => match self.context.syntax[syntax].as_deref() {
                Some("string") => TypeId::String,
                Some("number") => TypeId::Number,
                _ => self.resolve(syntax, Meaning::Type),
            },
            Ty::ObjectLiteralType(syntax) => self.object_literal_type(syntax),
            Ty::SignatureDeclaration(syntax) => {
                let symbol = self.context.syntax_symbols[syntax.0];
                self.type_type_of_symbol(symbol)
            }
            Ty::Error => TypeId::Error,
        }
    }

    fn object_literal_type(&mut self, syntax: ObjectLiteralType) -> TypeId {
        let id = self.context.syntax_symbols[syntax.0];
        let def = ObjectTypeDef {
            members: Table(
                self.context.symbol_members[id]
                    .items
                    .iter()
                    .cloned()
                    .map(|(name, symbol)| {
                        let ty = self.value_type_of_symbol(symbol);
                        (name, MemberTypeDef { symbol, ty })
                    })
                    .collect(),
            ),
        };
        let ty = self.context.define_type(TypeDef::Object(def));
        self.context.symbol_type_types.0.insert(id, ty);
        ty
    }

    // inlined
    // fn property_declaration(syntax: PropertyDeclaration) -> TypeId

    fn value_type_of_symbol(&mut self, id: Id<Symbol>) -> TypeId {
        let symbol = &self.context.symbols[id];
        let decl = symbol
            .value_declaration()
            .expect("value type of symbol without value decl");
        if let Some(&ty) = self.context.symbol_value_types.0.get(&id) {
            return ty;
        }
        // todo: instantiated symbol? ("target" in symbol)
        let syntax = &self.context.syntax[decl.syntax];
        match &syntax.data {
            SyntaxData::Var { .. } => self.statement(Var(decl.syntax).into()),
            SyntaxData::TypeAlias { .. } => self.statement(TypeAlias(decl.syntax).into()),
            SyntaxData::Object { .. } => self.expression(Object(decl.syntax).into()),
            SyntaxData::PropertyInitializer { .. } => {
                self.property_initializer(PropertyInitializer(decl.syntax))
            }
            SyntaxData::PropertyDeclaration(data) => self.ty_opt(data.typename),
            SyntaxData::Parameter(ParameterData { typename, .. }) => self.ty_opt(*typename),
            SyntaxData::Function { .. } => self.type_of_function(Function(decl.syntax)),

            kind => {
                todo!("value declaration kind: {kind:?}")
            }
        }
    }

    // not to be confused with function(), used by expression() etc.
    // to resolve the cached symbol type, which calls this.
    fn type_of_function(&mut self, syntax: Function) -> TypeId {
        let id = self.context.syntax_symbols[syntax.0];
        let data = &self.context.syntax[syntax];
        let type_parameters = data.type_parameters.as_ref().map(|tp| {
            tp.iter()
                .copied()
                .map(|p| {
                    // inlined type_parameter()
                    let symbol = self.context.syntax_symbols[p.0];
                    self.type_type_of_symbol(symbol);
                    symbol
                })
                .collect()
        });
        let parameters = data
            .parameters
            .iter()
            .copied()
            .map(|p| {
                // inlined check_parameter()
                let data = &self.context.syntax[p];
                self.ty_opt(data.typename);
                self.context.syntax_symbols[p.0]
            })
            .collect();
        let declared_type = data.typename.map(|ty| self.ty(ty));

        // inlined body()
        let mut return_types = vec![];

        for statement in data.body.iter().copied() {
            let statement_type = self.statement(statement);
            // todo: recursive
            // todo: dedupe
            if let Statement::Return(syntax) = statement {
                if let Some(declared_type) = declared_type {
                    if !self.is_assignable_to(statement_type, declared_type) {
                        self.context.error_for(
                            syntax.0,
                            Message::ReturnType {
                                return_type: statement_type,
                                declared_type,
                            },
                        );
                    }
                }
                return_types.push(statement_type);
            }
        }

        let return_type = if let Some(ty) = declared_type {
            ty
        } else if let Some(&ty) = return_types.first() {
            ty
        } else {
            TypeId::Void
        };

        let ty = self.context.define_type(TypeDef::Function(Signature {
            type_parameters,
            parameters,
            return_type,
        }));
        self.context.symbol_value_types.0.insert(id, ty);
        ty
    }

    fn type_of_signature(&mut self, syntax: SignatureDeclaration) -> TypeId {
        let locals = self.context.syntax_locals[syntax.0];
        let mut checker = self.context.checker(locals);
        let data = &checker.context.syntax[syntax];
        for p in data.type_parameters.iter().flatten().copied() {
            let symbol = checker.context.syntax_symbols[p.0];
            checker.type_type_of_symbol(symbol);
        }
        for p in data.parameters.iter().copied() {
            let data = &checker.context.syntax[p];
            checker.ty_opt(data.typename);
        }
        let type_parameters = data.type_parameters.as_ref().map(|tp| {
            tp.iter()
                .map(|p| checker.context.syntax_symbols[p.0])
                .collect()
        });
        let parameters = data
            .parameters
            .iter()
            .map(|p| checker.context.syntax_symbols[p.0])
            .collect();
        let return_type = checker.ty_opt(data.typename);
        let ty = checker.context.define_type(TypeDef::Function(Signature {
            type_parameters,
            parameters,
            return_type,
        }));
        let symbol = checker.context.syntax_symbols[syntax.0];
        checker.context.symbol_type_types.0.insert(symbol, ty);
        ty
    }

    fn type_type_of_symbol(&mut self, id: Id<Symbol>) -> TypeId {
        if let Some(&ty) = self.context.symbol_type_types.0.get(&id) {
            return ty;
        }
        // todo instantiated symbols ("target" in symbol)
        let symbol = &self.context.symbols[id];
        let decl = symbol.type_declarations().copied().next();
        if let Some(decl) = decl {
            let syntax = &self.context.syntax[decl.syntax];
            match &syntax.data {
                SyntaxData::TypeAlias(data) => {
                    return self.ty(data.typename);
                }
                SyntaxData::TypeParameter(data) => {
                    let ret = self
                        .context
                        .define_type(TypeDef::TypeVariable { name: data.name });
                    self.context.symbol_type_types.0.insert(id, ret);
                    return ret;
                }
                SyntaxData::SignatureDeclaration(_) => {
                    return self.type_of_signature(SignatureDeclaration(decl.syntax))
                }
                _ => {}
            }
        };
        panic!("symbol has no type declarations")
    }

    // implemented in crate::diag::WithContext: diag::Message contains ids
    // fn type_to_string(ty: TypeId) -> String

    // In centi-typescript, this is called from (the equivalents of):
    // - identifier() (via expression() and ty())
    // - object_literal_type() - should just return from symbol.members
    // - object_literal() - should just return from symbol.members
    //
    // Table-providing locations:
    // - Module, Function, Signature: provides locals
    // - Object, ObjectLiteralType: provides symbol.members
    //
    // This implies that Object and ObjectLiteralType shouldn't need to resolve at all, and simply
    // return from their members in [`CheckContext::`]
    //
    // Then this is only called by `identifier()` (for both values and types), so chasing the table
    // chain is correct, but only makes sense if we update [`Self::table`]
    fn resolve(&mut self, name: Identifier, meaning: Meaning) -> TypeId {
        let Some(text) = &self.context.syntax[name] else {
            // Parse error
            return TypeId::Error;
        };

        let mut next_scope = Some(self.scope);

        while let Some(scope) = next_scope {
            let table = &self.context.scopes[scope];
            next_scope = table.parent;

            if let Some(&symbol) = table.symbols.0.get(text) {
                let declarations = &self.context.symbols[symbol].declarations;
                if declarations.iter().any(|d| d.meaning == meaning) {
                    return match meaning {
                        Meaning::Value => self.value_type_of_symbol(symbol),
                        Meaning::Type => self.type_type_of_symbol(symbol),
                    };
                }
            }
        }

        self.context.error_for(
            name.0,
            Message::Resolve {
                name: text.to_string(),
                meaning,
            },
        )
    }

    // inlined
    // fn get_symbol(locals: Id<Table>, name: &str, meaning: Meaning) -> Option<Id<Symbol>>

    fn is_assignable_to(&mut self, source: TypeId, target: TypeId) -> bool {
        if source == target {
            return true;
        }
        match (source, target) {
            (TypeId::Any | TypeId::Error, _) | (_, TypeId::Any | TypeId::Error) => true,
            (TypeId::Number | TypeId::String | TypeId::Void, _)
            | (_, TypeId::Number | TypeId::String | TypeId::Void) => false,
            (TypeId::Def(source), TypeId::Def(target)) => {
                let source = &self.context.type_defs[source];
                let target = &self.context.type_defs[target];
                match (source, target) {
                    (TypeDef::Object(source), TypeDef::Object(target)) => {
                        // fixme: workaround lifetime error
                        let source = source.clone();
                        let target = target.clone();
                        for (key, target_member) in &target.members.0 {
                            let assignable = match source.members.0.get(key) {
                                None => false,
                                Some(source_member) => {
                                    let source_ty = source_member.ty;
                                    let target_ty = target_member.ty;
                                    self.is_assignable_to(source_ty, target_ty)
                                }
                            };
                            if !assignable {
                                return false;
                            }
                        }
                        true
                    }
                    (TypeDef::Function(source), TypeDef::Function(target)) => {
                        // fixme: workaround lifetime error
                        let source = source.clone();
                        let mut target = target.clone();
                        if let (Some(source_tp), Some(target_tp)) =
                            (&source.type_parameters, &target.type_parameters)
                        {
                            let sources = target_tp
                                .iter()
                                .copied()
                                .map(|tp| self.type_type_of_symbol(tp))
                                .collect();
                            let targets = source_tp
                                .iter()
                                .copied()
                                .map(|tp| self.type_type_of_symbol(tp))
                                .collect();
                            let mapper = TypeMapper { sources, targets };
                            target = self.instantiate_signature(&target, &mapper);
                        }

                        self.is_assignable_to(source.return_type, target.return_type)
                            && source.parameters.len() <= target.parameters.len()
                            && source.parameters.iter().zip(target.parameters.iter()).all(
                                |(sp, tp)| {
                                    let sp = self.value_type_of_symbol(*sp);
                                    let tp = self.value_type_of_symbol(*tp);
                                    self.is_assignable_to(tp, sp)
                                },
                            )
                    }
                    _ => false,
                }
            }
        }
    }
}
