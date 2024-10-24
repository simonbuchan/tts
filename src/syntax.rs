use crate::list::{Id, List};
use crate::source::{Source, SourcePos, SourceSpan};

#[derive(Clone, Debug)]
pub struct Syntax {
    pub span: SourceSpan,
    pub data: SyntaxData,
}

#[derive(Default)]
pub struct SyntaxList(List<Syntax>);

impl SyntaxList {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pos(&self, id: Id<Syntax>) -> SourcePos {
        self.0[id].span.start
    }

    pub fn span(&self, id: Id<Syntax>) -> SourceSpan {
        self.0[id].span
    }
}

impl std::ops::Index<Id<Syntax>> for SyntaxList {
    type Output = Syntax;

    fn index(&self, index: Id<Syntax>) -> &Self::Output {
        &self.0[index]
    }
}

pub trait SyntaxId {
    type Data;

    fn add(list: &mut SyntaxList, span: SourceSpan, data: Self::Data) -> Self;
}

impl SyntaxList {
    pub fn add<S: SyntaxId>(&mut self, span: SourceSpan, data: S::Data) -> S {
        S::add(self, span, data)
    }
}

macro_rules! syntax {
    {
        $( $Kind: ident : $Data: ty , )*
        ==
        $( $Group: ident = $( $Alias: ident )|+, )*
    } => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum SyntaxKind {
            $( $Kind, )*
        }

        #[derive(Clone, Debug)]
        pub enum SyntaxData {
            $( $Kind ( $Data ), )*
        }

        impl SyntaxData {
            pub fn kind(&self) -> SyntaxKind {
                match self {
                    $(Self::$Kind { .. } => SyntaxKind::$Kind,)*
                }
            }
        }

        $(
        #[derive(Copy, Clone, Debug, Hash, Ord, PartialOrd, Eq, PartialEq)]
        pub struct $Kind(pub Id<Syntax>);

        impl SyntaxId for $Kind {
            type Data = $Data;

            fn add(table: &mut SyntaxList, span: SourceSpan, data: $Data) -> Self {
                let data = SyntaxData::$Kind ( data );
                let id = table.0.add(Syntax { span, data });
                Self(id)
            }
        }

        impl std::ops::Index<$Kind> for SyntaxList {
            type Output = $Data;

            fn index(&self, index: $Kind) -> &Self::Output {
                let syntax = &self.0[index.0];
                match &syntax.data {
                    SyntaxData::$Kind ( data ) => data,
                    data => panic!(
                        "syntax {:?} expected {:?}, found {:?}",
                        index.0,
                        SyntaxKind::$Kind,
                        data.kind()
                    ),
                }
            }
        }
        )*

        $(

        #[derive(Copy, Clone)]
        pub enum $Group {
            $( $Alias ( $Alias ) ,)*
            Error,
        }

        impl $Group {
            pub fn id(self) -> Option<Id<Syntax>> {
                match self {
                    $(Self::$Alias(syntax) => Some(syntax.0),)*
                    Self::Error => None,
                }
            }

            pub fn from_kind(id: Id<Syntax>, kind: SyntaxKind) -> Self {
                match kind {
                    $(SyntaxKind::$Alias => Self::$Alias($Alias(id)),)*
                    _ => Self::Error,
                }
            }
        }

        $(
        impl From<$Alias> for $Group {
            fn from(value: $Alias) -> Self {
                Self::$Alias(value)
            }
        }
        )*

        impl std::fmt::Debug for $Group {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$Alias(inner) => inner.fmt(f),)*
                    Self::Error => f.write_str("Error"),
                }
            }
        }

        )*

        // todo
        #[allow(dead_code, unused_variables, non_snake_case)]
        trait Visitor {
            $(
            fn $Kind(&mut self, id: Id<Syntax>, data: &$Data) {

            }
            )*
        }
    };
}

syntax! {
    NumericLiteral: u64,
    StringLiteral: String,
    Identifier: Option<String>,
    Module: ModuleData,
    Assignment: AssignmentData,
    ExpressionStatement: ExpressionStatementData,
    Var: VarData,
    TypeAlias: TypeAliasData,
    Object: ObjectData,
    PropertyInitializer: PropertyInitializerData,
    ObjectLiteralType: ObjectLiteralTypeData,
    PropertyDeclaration: PropertyDeclarationData,
    Function: FunctionData,
    SignatureDeclaration: SignatureDeclarationData,
    Parameter: ParameterData,
    TypeParameter: TypeParameterData,
    Return: ReturnData,
    Call: CallData,
    ==
    Expression = Identifier | NumericLiteral | StringLiteral | Assignment
               | Object | Function | Call,
    Statement = Var | TypeAlias | ExpressionStatement | Return,
    Ty = ObjectLiteralType | Identifier | SignatureDeclaration,
}

// todo
trait SyntaxTree {
    fn properties<V: PropertyVisitor>(&self, visitor: V);
}

trait PropertyVisitor {
    fn none(&self, name: &'static str);
    fn some(&self, name: &'static str, id: Id<Syntax>);
    fn list(&self, name: &'static str) -> impl ListAccess;
}

trait ListAccess {
    fn item(&mut self, id: Id<Syntax>);
}

#[derive(Clone, Debug)]
pub struct ObjectLiteralTypeData {
    pub properties: Vec<PropertyDeclaration>,
}

impl SyntaxTree for ObjectLiteralTypeData {
    fn properties<V: PropertyVisitor>(&self, visitor: V) {
        let mut access = visitor.list("properties");
        for p in self.properties.iter().copied() {
            access.item(p.0);
        }
    }
}

#[derive(Clone, Debug)]
pub struct ObjectData {
    pub properties: Vec<PropertyInitializer>,
}

#[derive(Clone, Debug)]
pub struct PropertyInitializerData {
    pub name: Identifier,
    pub initializer: Expression,
}

#[derive(Clone, Debug)]
pub struct PropertyDeclarationData {
    pub name: Identifier,
    pub typename: Option<Ty>,
}

#[derive(Clone, Debug)]
pub struct AssignmentData {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Clone, Debug)]
pub struct FunctionData {
    pub name: Option<Identifier>,
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<Parameter>,
    pub typename: Option<Ty>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct SignatureDeclarationData {
    pub type_parameters: Option<Vec<TypeParameter>>,
    pub parameters: Vec<Parameter>,
    pub typename: Option<Ty>,
}

#[derive(Clone, Debug)]
pub struct TypeParameterData {
    pub name: Identifier,
}

#[derive(Clone, Debug)]
pub struct ParameterData {
    pub name: Identifier,
    pub typename: Option<Ty>,
}

#[derive(Clone, Debug)]
pub struct ExpressionStatementData {
    pub expression: Expression,
}

#[derive(Clone, Debug)]
pub struct VarData {
    pub name: Identifier,
    pub typename: Option<Ty>,
    pub initializer: Expression,
}

#[derive(Clone, Debug)]
pub struct TypeAliasData {
    pub name: Identifier,
    pub typename: Ty,
}

#[derive(Clone, Debug)]
pub struct ReturnData {
    pub expression: Expression,
}

#[derive(Clone, Debug)]
pub struct CallData {
    pub expression: Expression,
    pub type_arguments: Option<Vec<Ty>>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct ModuleData {
    pub statements: Vec<Statement>,
}

// Should use a visitor or something?
// Or could simply dump the flat syntax list, though it would be ugly!
pub struct TreeWriter<'source, 'syntax> {
    source: &'source Source,
    list: &'syntax SyntaxList,
    id: Id<Syntax>,
    indent: usize,
}

impl<'source, 'syntax> TreeWriter<'source, 'syntax> {
    pub fn new(source: &'source Source, list: &'syntax SyntaxList, id: Id<Syntax>) -> Self {
        Self {
            source,
            list,
            id,
            indent: 0,
        }
    }
}

struct FmtData<'b, 'a> {
    f: &'b mut std::fmt::Formatter<'a>,
    indent: usize,
}

impl FmtData<'_, '_> {
    fn write_indent(&mut self) -> std::fmt::Result {
        indent(self.indent, self.f)
    }

    fn open(&mut self) -> std::fmt::Result {
        writeln!(self.f, " {{")?;
        self.indent += 2;
        Ok(())
    }

    fn close(mut self) -> std::fmt::Result {
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.f, "}}")?;
        Ok(())
    }

    fn field_name(&mut self, name: &str) -> std::fmt::Result {
        self.write_indent()?;
        write!(self.f, "{name}: ")?;
        Ok(())
    }

    fn field_nested(
        &mut self,
        name: &str,
        parent: &TreeWriter<'_, '_>,
        id: Id<Syntax>,
    ) -> std::fmt::Result {
        self.field_name(name)?;
        self.value_nested(parent, id)
    }

    fn field_try_nested(
        &mut self,
        name: &str,
        parent: &TreeWriter<'_, '_>,
        id: Option<Id<Syntax>>,
    ) -> std::fmt::Result {
        self.field_name(name)?;
        if let Some(id) = id {
            self.value_nested(parent, id)
        } else {
            self.value_error()
        }
    }

    fn field_opt_try_nested(
        &mut self,
        name: &str,
        parent: &TreeWriter<'_, '_>,
        id: Option<Option<Id<Syntax>>>,
    ) -> std::fmt::Result {
        self.field_name(name)?;
        if let Some(id) = id {
            if let Some(id) = id {
                self.value_nested(parent, id)
            } else {
                self.value_error()
            }
        } else {
            self.value_none()
        }
    }

    fn value_none(&mut self) -> std::fmt::Result {
        writeln!(self.f, "<none>")
    }

    fn value_error(&mut self) -> std::fmt::Result {
        writeln!(self.f, "<error>")
    }

    fn field_array_start(&mut self, name: &str) -> std::fmt::Result {
        self.field_name(name)?;
        self.value_array_start()?;
        Ok(())
    }

    fn value_array_start(&mut self) -> std::fmt::Result {
        self.indent += 2;
        writeln!(self.f, "[")
    }

    fn array_item(&mut self) -> std::fmt::Result {
        self.write_indent()
    }

    fn array_item_try_nested(
        &mut self,
        parent: &TreeWriter<'_, '_>,
        id: Option<Id<Syntax>>,
    ) -> std::fmt::Result {
        self.array_item()?;
        self.value_try_nested(parent, id)
    }

    fn array_item_nested(
        &mut self,
        parent: &TreeWriter<'_, '_>,
        id: Id<Syntax>,
    ) -> std::fmt::Result {
        self.array_item()?;
        self.value_nested(parent, id)
    }

    fn value_array_end(&mut self) -> std::fmt::Result {
        self.indent -= 2;
        self.write_indent()?;
        writeln!(self.f, "]")
    }

    fn value_try_nested(
        &mut self,
        parent: &TreeWriter<'_, '_>,
        id: Option<Id<Syntax>>,
    ) -> std::fmt::Result {
        if let Some(id) = id {
            self.value_nested(parent, id)
        } else {
            self.value_error()
        }
    }

    fn value_nested(&mut self, parent: &TreeWriter<'_, '_>, id: Id<Syntax>) -> std::fmt::Result {
        let child = TreeWriter {
            source: parent.source,
            list: parent.list,
            id,
            indent: self.indent,
        };
        std::fmt::Display::fmt(&child, self.f)
    }
}

fn indent(count: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use std::fmt::Write as _;
    for _ in 0..count {
        f.write_char(' ')?
    }
    Ok(())
}

impl std::fmt::Display for TreeWriter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let syntax = &self.list[self.id];

        let start_loc = self.source.line_col(syntax.span.start);
        let end_loc = self.source.line_col(syntax.span.end);
        write!(
            f,
            "{:?} @ {}-{} ({}-{})",
            syntax.data.kind(),
            start_loc,
            end_loc,
            syntax.span.start.0,
            syntax.span.end.0,
        )?;

        let mut fmt = FmtData {
            f,
            indent: self.indent,
        };

        // might be able to simplify this with a generic visitor? (probably not)
        match &syntax.data {
            SyntaxData::Module(data) => {
                fmt.open()?;
                fmt.field_array_start("statements")?;
                for statement in &data.statements {
                    fmt.array_item_try_nested(self, statement.id())?;
                }
                fmt.value_array_end()?;
                fmt.close()?;
            }
            SyntaxData::NumericLiteral(data) => {
                writeln!(f, " = {data}")?;
            }
            SyntaxData::StringLiteral(data) => {
                writeln!(f, " = {data}")?;
            }
            SyntaxData::Identifier(data) => {
                if let Some(name) = data {
                    writeln!(f, " = {name}")?;
                } else {
                    writeln!(f, " = <error>")?;
                }
            }
            SyntaxData::Assignment(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.field_try_nested("value", self, data.value.id())?;
                fmt.close()?;
            }
            SyntaxData::ExpressionStatement(data) => {
                fmt.open()?;
                fmt.field_try_nested("expression", self, data.expression.id())?;
                fmt.close()?;
            }
            SyntaxData::Var(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.field_opt_try_nested("typename", self, data.typename.map(|t| t.id()))?;
                fmt.field_try_nested("initializer", self, data.initializer.id())?;
                fmt.close()?;
            }
            SyntaxData::TypeAlias(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.field_try_nested("typename", self, data.typename.id())?;
                fmt.close()?;
            }
            SyntaxData::Object(data) => {
                fmt.open()?;
                fmt.field_array_start("properties")?;
                for p in data.properties.iter().copied() {
                    fmt.array_item_nested(self, p.0)?;
                }
                fmt.value_array_end()?;
                fmt.close()?;
            }
            SyntaxData::PropertyInitializer(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.field_try_nested("initializer", self, data.initializer.id())?;
                fmt.close()?;
            }
            SyntaxData::ObjectLiteralType(data) => {
                fmt.open()?;
                fmt.field_array_start("properties")?;
                for p in data.properties.iter().copied() {
                    fmt.array_item_nested(self, p.0)?;
                }
                fmt.value_array_end()?;
                fmt.close()?;
            }
            SyntaxData::PropertyDeclaration(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.field_opt_try_nested("typename", self, data.typename.map(|t| t.id()))?;
                fmt.close()?;
            }
            SyntaxData::Function(data) => {
                fmt.open()?;
                fmt.field_name("name")?;
                if let Some(id) = data.name {
                    fmt.value_nested(self, id.0)?;
                } else {
                    fmt.value_none()?;
                }
                fmt.field_name("type_parameters")?;
                if let Some(ps) = &data.type_parameters {
                    fmt.value_array_start()?;
                    for p in ps.iter().copied() {
                        fmt.array_item_nested(self, p.0)?
                    }
                    fmt.value_array_end()?;
                } else {
                    fmt.value_none()?;
                }
                fmt.field_array_start("parameters")?;
                for p in data.parameters.iter().copied() {
                    fmt.array_item_nested(self, p.0)?
                }
                fmt.value_array_end()?;
                fmt.field_opt_try_nested("typename", self, data.typename.map(|t| t.id()))?;
                fmt.field_array_start("body")?;
                for s in data.body.iter().cloned() {
                    fmt.array_item_try_nested(self, s.id())?
                }
                fmt.value_array_end()?;
                fmt.close()?;
            }
            SyntaxData::SignatureDeclaration(data) => {
                fmt.open()?;
                fmt.field_name("type_parameters")?;
                if let Some(ps) = &data.type_parameters {
                    fmt.value_array_start()?;
                    for p in ps.iter().copied() {
                        fmt.array_item_nested(self, p.0)?
                    }
                    fmt.value_array_end()?;
                } else {
                    fmt.value_none()?;
                }
                fmt.field_array_start("parameters")?;
                for p in data.parameters.iter().copied() {
                    fmt.array_item_nested(self, p.0)?
                }
                fmt.value_array_end()?;
                fmt.field_opt_try_nested("typename", self, data.typename.map(|t| t.id()))?;
                fmt.close()?;
            }
            SyntaxData::Parameter(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.field_opt_try_nested("typename", self, data.typename.map(|t| t.id()))?;
                fmt.close()?;
            }
            SyntaxData::TypeParameter(data) => {
                fmt.open()?;
                fmt.field_nested("name", self, data.name.0)?;
                fmt.close()?;
            }
            SyntaxData::Return(data) => {
                fmt.open()?;
                fmt.field_try_nested("expression", self, data.expression.id())?;
                fmt.close()?;
            }
            SyntaxData::Call(data) => {
                fmt.open()?;
                fmt.field_try_nested("expression", self, data.expression.id())?;
                fmt.field_name("type_arguments")?;
                if let Some(tas) = &data.type_arguments {
                    fmt.value_array_start()?;
                    for ta in tas.iter().copied() {
                        fmt.array_item_try_nested(self, ta.id())?
                    }
                    fmt.value_array_end()?;
                } else {
                    fmt.value_none()?;
                }
                fmt.field_array_start("arguments")?;
                for a in data.arguments.iter().copied() {
                    fmt.array_item_try_nested(self, a.id())?;
                }
                fmt.value_array_end()?;
                fmt.close()?;
            }
        }
        Ok(())
    }
}
