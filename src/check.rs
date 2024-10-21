use crate::bind::{BindResult, Meaning, Symbol, Table};
use crate::diag::{Message, Reporter};
use crate::list::{Id, List};
use crate::parse::ParseResult;
use crate::source::SourceSpan;
use crate::syntax::*;

#[derive(Default)]
pub struct CheckResult {
    pub type_defs: List<TypeDef>,
}

#[derive(Clone, Debug)]
pub enum TypeDef {
    Object { members: Id<Table> },
    TypeVariable { name: Identifier },
    Function(FunctionTypeDef),
}

#[derive(Clone, Debug)]
pub struct FunctionTypeDef {
    pub type_parameters: Option<Vec<Id<Symbol>>>,
    pub parameters: Vec<Id<Symbol>>,
    pub return_type: TypeId,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
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
        symbols: &bindings.symbols,
        syntax_symbols: &bindings.syntax_symbols,
        tables: &bindings.tables,
        syntax_locals: &bindings.syntax_locals,

        symbol_value_types: std::collections::HashMap::new(),
        symbol_type_types: std::collections::HashMap::new(),
        type_defs: Default::default(),

        reporter,
    };

    let mut checker = context.checker(bindings.module_table);

    for statement in parsed.syntax[parsed.module].statements.iter().copied() {
        checker.statement(statement);
    }

    CheckResult {
        type_defs: context.type_defs,
    }
}

struct TypeMapper {
    sources: Vec<TypeId>,
    targets: Vec<TypeId>,
}

struct CheckContext<'a, 'reports> {
    // ParseResult
    syntax: &'a SyntaxList,
    // BindResult
    symbols: &'a List<Symbol>,
    syntax_symbols: &'a std::collections::HashMap<Id<Syntax>, Id<Symbol>>,
    tables: &'a List<Table>,
    syntax_locals: &'a std::collections::HashMap<Id<Syntax>, Id<Table>>,
    // CheckResult
    type_defs: List<TypeDef>,

    symbol_value_types: std::collections::HashMap<Id<Symbol>, TypeId>,
    symbol_type_types: std::collections::HashMap<Id<Symbol>, TypeId>,
    reporter: &'a mut Reporter<'reports>,
}

impl<'a, 'reports> CheckContext<'a, 'reports> {
    fn checker(&mut self, table: Id<Table>) -> Checker<'_, 'a, 'reports> {
        Checker {
            context: self,
            table,
        }
    }
}

struct Checker<'context, 'a, 'reports> {
    context: &'context mut CheckContext<'a, 'reports>,
    table: Id<Table>,
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
                    self.error_for(syntax.0, Message::Assign { left, right });
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
            self.property(p);
        }

        let symbol = self.syntax_symbol(syntax.0);
        let members = symbol.object_members.expect("expected object symbol");
        self.define_type(TypeDef::Object { members })
    }

    fn property(&mut self, syntax: PropertyInitializer) -> TypeId {
        let data = &self.context.syntax[syntax];
        self.expression(data.initializer)
    }

    fn function(&mut self, syntax: Function) -> TypeId {
        let locals = self.context.syntax_locals[&syntax.0];
        let symbol = self.context.syntax_symbols[&syntax.0];
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
            return self.error_for(syntax.0, Message::Call { ty });
        };
        let TypeDef::Function(def) = &self.context.type_defs[function] else {
            return self.error_for(syntax.0, Message::Call { ty });
        };
        // fixme: to avoid lifetime errors. Should be cheap as it's only ids, but i'm sure theres
        //        a better option here.
        let def = def.clone();

        if def.parameters.len() != arg_types.len() {
            self.error_for(
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

    // fn instantiate_signature(
    //     generic_signature: SignatureDeclaration,
    //     mapper: TypeMapper,
    // ) -> InstantiatedSignatureDeclaration {
    //     todo!()
    // }
    //
    // fn instantiate_type(ty: TypeId, mapper: TypeMapper) -> TypeId {
    //     todo!()
    // }
    //
    // fn instantiate_symbol(
    //     symbol: Symbol,
    //     mapper: TypeMapper,
    // ) -> InstantiatedSymbol {
    //     todo!()
    // }
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
                let symbol = self.context.syntax_symbols[&syntax.0];
                self.type_type_of_symbol(symbol)
            }
            Ty::Error => TypeId::Error,
        }
    }

    fn object_literal_type(&mut self, syntax: ObjectLiteralType) -> TypeId {
        let id = self.context.syntax_symbols[&syntax.0];
        let symbol = &self.context.symbols[id];
        let members = symbol
            .object_members
            .expect("ObjectLiteralType should have members");
        let ty = self.define_type(TypeDef::Object { members });
        self.context.symbol_type_types.insert(id, ty);
        ty
    }

    // inlined
    // fn property_declaration(syntax: PropertyDeclaration) -> TypeId

    fn value_type_of_symbol(&mut self, id: Id<Symbol>) -> TypeId {
        let symbol = &self.context.symbols[id];
        let decl = symbol
            .value_declaration()
            .expect("value type of symbol without value decl");
        if let Some(&ty) = self.context.symbol_value_types.get(&id) {
            return ty;
        }
        // todo: instantiated symbol? ("target" in symbol)
        let syntax = &self.context.syntax[decl.syntax];
        match &syntax.data {
            SyntaxData::Var { .. } => self.statement(Var(decl.syntax).into()),
            SyntaxData::TypeAlias { .. } => self.statement(TypeAlias(decl.syntax).into()),
            SyntaxData::Object { .. } => self.expression(Object(decl.syntax).into()),
            SyntaxData::PropertyInitializer { .. } => {
                self.property(PropertyInitializer(decl.syntax))
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
        let id = self.context.syntax_symbols[&syntax.0];
        let data = &self.context.syntax[syntax];
        let type_parameters = data.type_parameters.as_ref().map(|tp| {
            tp.iter()
                .copied()
                .map(|p| {
                    // inlined type_parameter()
                    let symbol = self.context.syntax_symbols[&p.0];
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
                self.context.syntax_symbols[&p.0]
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
                        self.error_for(
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

        let ty = self.define_type(TypeDef::Function(FunctionTypeDef {
            type_parameters,
            parameters,
            return_type,
        }));
        self.context.symbol_value_types.insert(id, ty);
        ty
    }

    fn type_of_signature(&mut self, syntax: SignatureDeclaration) -> TypeId {
        let locals = self.context.syntax_locals[&syntax.0];
        let mut checker = self.context.checker(locals);
        let data = &checker.context.syntax[syntax];
        for p in data.type_parameters.iter().flatten().copied() {
            let symbol = checker.context.syntax_symbols[&p.0];
            checker.type_type_of_symbol(symbol);
        }
        for p in data.parameters.iter().copied() {
            let data = &checker.context.syntax[p];
            checker.ty_opt(data.typename);
        }
        let type_parameters = data.type_parameters.as_ref().map(|tp| {
            tp.iter()
                .map(|p| checker.context.syntax_symbols[&p.0])
                .collect()
        });
        let parameters = data
            .parameters
            .iter()
            .map(|p| checker.context.syntax_symbols[&p.0])
            .collect();
        let return_type = checker.ty_opt(data.typename);
        let ty = checker.define_type(TypeDef::Function(FunctionTypeDef {
            type_parameters,
            parameters,
            return_type,
        }));
        let symbol = checker.context.syntax_symbols[&syntax.0];
        checker.context.symbol_type_types.insert(symbol, ty);
        ty
    }

    fn type_type_of_symbol(&mut self, id: Id<Symbol>) -> TypeId {
        if let Some(&ty) = self.context.symbol_type_types.get(&id) {
            return ty;
        }
        // todo instantiated symbols ("target" in symbol)
        let symbol = &self.context.symbols[id];
        if let Some(decl) = symbol.type_declarations().next() {
            let syntax = &self.context.syntax[decl.syntax];
            match &syntax.data {
                SyntaxData::TypeAlias(data) => {
                    return self.ty(data.typename);
                }
                SyntaxData::TypeParameter(data) => {
                    let ret = self.define_type(TypeDef::TypeVariable { name: data.name });
                    self.context.symbol_type_types.insert(id, ret);
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

    // fixme: Unlike centi-typescript, resolves in the current Checker::table context, instead
    //        of chasing syntax parents. This means recursing into locals-providing syntax should
    //        use a child Checker.
    //
    // Called from:
    // - identifier() (via expression() and ty())
    // - object_literal_type() - should just return from symbol.members
    // - object_literal() - should just return from symbol.members
    //
    // Table-providing locations:
    // - Module, Function, Signature: provides locals
    // - Object, ObjectLiteralType: provides symbol.members
    //
    // This implies that Object and ObjectLiteralType shouldn't need to resolve at all, and simply
    // return from their table in (the resolution of) [`Symbol::object_members`]?
    //
    // Then this is only called by `identifier()` (for both values and types), so chasing the table
    // chain is correct, but only makes sense if we update [`Self::table`]
    fn resolve(&mut self, name: Identifier, meaning: Meaning) -> TypeId {
        let Some(text) = &self.context.syntax[name] else {
            // Parse error
            return TypeId::Error;
        };

        let mut next_table = Some(self.table);

        while let Some(table) = next_table {
            let table = &self.context.tables[table];
            next_table = table.parent;

            if let Some(&symbol) = table.symbols.get(text) {
                let declarations = &self.context.symbols[symbol].declarations;
                if declarations.iter().any(|d| d.meaning == meaning) {
                    return match meaning {
                        Meaning::Value => self.value_type_of_symbol(symbol),
                        Meaning::Type => self.type_type_of_symbol(symbol),
                    };
                }
            }
        }

        self.error_for(
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
                    (TypeDef::Object { members: source }, TypeDef::Object { members: target }) => {
                        let source = &self.context.tables[*source];
                        let target = &self.context.tables[*target];

                        for (key, &target_symbol) in &target.symbols {
                            let assignable = match source.symbols.get(key) {
                                None => false,
                                Some(&source_symbol) => {
                                    let source_ty = self.value_type_of_symbol(source_symbol);
                                    let target_ty = self.value_type_of_symbol(target_symbol);
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
                        let target = target.clone();
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
                            todo!("instantiate signatures")
                        }
                        if !self.is_assignable_to(source.return_type, target.return_type) {
                            return false;
                        }
                        if source.parameters.len() <= target.parameters.len() {
                            return false;
                        }
                        source
                            .parameters
                            .iter()
                            .zip(target.parameters.iter())
                            .all(|(sp, tp)| {
                                let sp = self.value_type_of_symbol(*sp);
                                let tp = self.value_type_of_symbol(*tp);
                                self.is_assignable_to(tp, sp)
                            })
                    }
                    _ => false,
                }
            }
        }
    }

    //----------
    // added helpers (that should be moved to Context)

    fn error(&mut self, span: SourceSpan, message: Message) -> TypeId {
        self.context.reporter.error(span, message);
        TypeId::Error
    }

    fn error_for(&mut self, syntax: Id<Syntax>, message: Message) -> TypeId {
        let span = self.context.syntax.span(syntax);
        self.error(span, message)
    }

    fn define_type(&mut self, type_def: TypeDef) -> TypeId {
        TypeId::Def(self.context.type_defs.add(type_def))
    }

    fn syntax_symbol(&self, syntax: Id<Syntax>) -> &Symbol {
        let id = self.context.syntax_symbols[&syntax];
        &self.context.symbols[id]
    }
}
