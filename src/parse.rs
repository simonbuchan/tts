use crate::diag::{Message, Reporter};
use crate::lex::{Lexer, TokenKind};
use crate::source::{SourcePos, SourceSpan};
use crate::syntax::*;

pub struct ParseResult {
    pub syntax: SyntaxList,
    pub module: Module,
}

pub fn parse(lexer: Lexer<'_>, reporter: &mut Reporter<'_>) -> ParseResult {
    let mut parser = Parser {
        lexer,
        reporter,
        syntax: SyntaxList::new(),
    };
    let module = parser.module();
    ParseResult {
        syntax: parser.syntax,
        module,
    }
}

struct Parser<'source, 'reporter, 'reports> {
    lexer: Lexer<'source>,
    reporter: &'reporter mut Reporter<'reports>,
    syntax: SyntaxList,
}

impl Parser<'_, '_, '_> {
    fn syntax<S: SyntaxId>(&mut self, start: SourcePos, data: S::Data) -> S {
        let end = self.lexer.previous_end;
        let span = SourceSpan { start, end };
        self.syntax.add(span, data)
    }

    fn module(&mut self) -> Module {
        let start = self.lexer.span.start;
        let mut statements = vec![];
        // unwrap terminated body so it doesn't have to special case trying to
        // scan on End
        while self.lexer.kind != TokenKind::End {
            statements.push(self.statement());
            self.try_kind(TokenKind::Semicolon);
        }
        self.syntax(start, ModuleData { statements })
    }

    fn block(&mut self) -> Vec<Statement> {
        self.expect(TokenKind::OpenBrace, "block");
        self.terminated(Self::statement, TokenKind::Semicolon, TokenKind::CloseBrace)
    }

    fn expression(&mut self) -> Expression {
        let start = self.lexer.span.start;
        let expression = self.expression_below_call();
        if self.try_kind(TokenKind::OpenParen) {
            let arguments =
                self.terminated(Self::expression, TokenKind::Comma, TokenKind::CloseParen);
            let data = CallData {
                expression,
                type_arguments: None,
                arguments,
            };
            Expression::Call(self.syntax(start, data))
        } else if self.try_kind(TokenKind::LessThan) {
            let type_arguments =
                Some(self.terminated(Self::type_node, TokenKind::Comma, TokenKind::GreaterThan));
            self.expect(TokenKind::OpenParen, "parameter list");
            let arguments =
                self.terminated(Self::expression, TokenKind::Comma, TokenKind::CloseParen);
            let data = CallData {
                expression,
                type_arguments,
                arguments,
            };
            Expression::Call(self.syntax(start, data))
        } else {
            expression
        }
    }

    fn expression_below_call(&mut self) -> Expression {
        let span = self.lexer.span;
        match self.lexer.kind {
            TokenKind::OpenBrace => {
                self.lexer.scan();
                let properties = self.terminated(
                    Self::property_initializer,
                    TokenKind::Comma,
                    TokenKind::CloseBrace,
                );
                Expression::Object(self.syntax(span.start, ObjectData { properties }))
            }
            TokenKind::KeywordFunction => {
                self.lexer.scan();
                let name = match self.lexer.kind {
                    TokenKind::Identifier => Some(self.identifier()),
                    _ => None,
                };
                let type_parameters = if self.try_kind(TokenKind::LessThan) {
                    Some(self.terminated(
                        Self::type_parameter,
                        TokenKind::Comma,
                        TokenKind::GreaterThan,
                    ))
                } else {
                    None
                };
                self.expect(TokenKind::OpenParen, "parameter list");
                let parameters =
                    self.terminated(Self::parameter, TokenKind::Comma, TokenKind::CloseParen);
                let typename = self.try_type_annotation();
                let body = self.block();

                let data = FunctionData {
                    name,
                    type_parameters,
                    parameters,
                    typename,
                    body,
                };
                Expression::Function(self.syntax(span.start, data))
            }
            TokenKind::StringLiteral => {
                self.lexer.scan();
                let value = self.lexer.source[span].to_string();
                Expression::StringLiteral(self.syntax(span.start, value))
            }
            TokenKind::NumericLiteral => {
                self.lexer.scan();
                let value = self.lexer.source[span]
                    .parse()
                    .expect("invalid NumericLiteral token");
                Expression::NumericLiteral(self.syntax(span.start, value))
            }
            TokenKind::Identifier => {
                let name = self.identifier();
                if !self.try_kind(TokenKind::Equals) {
                    Expression::Identifier(name)
                } else {
                    let value = self.expression();
                    let data = AssignmentData { name, value };
                    Expression::Assignment(self.syntax(span.start, data))
                }
            }
            _ => {
                self.error(
                    span,
                    Message::Expected {
                        expected: "expression",
                    },
                );
                Expression::Error
            }
        }
    }

    fn parameter(&mut self) -> Parameter {
        let start = self.lexer.span.start;
        let name = self.identifier();
        let typename = self.try_type_annotation();
        let data = ParameterData { name, typename };
        self.syntax(start, data)
    }

    fn type_parameter(&mut self) -> TypeParameter {
        let start = self.lexer.span.start;
        let name = self.identifier();
        let data = TypeParameterData { name };
        self.syntax(start, data)
    }

    fn try_type_annotation(&mut self) -> Option<Ty> {
        if self.try_kind(TokenKind::Colon) {
            Some(self.type_node())
        } else {
            None
        }
    }

    fn type_node(&mut self) -> Ty {
        let kind = self.lexer.kind;
        let span = self.lexer.span;
        match kind {
            TokenKind::OpenBrace => {
                self.lexer.scan();
                let properties = self.terminated(
                    Self::property_declaration,
                    TokenKind::Comma,
                    TokenKind::CloseBrace,
                );
                let data = ObjectLiteralTypeData { properties };
                Ty::ObjectLiteralType(self.syntax(span.start, data))
            }
            TokenKind::LessThan => {
                self.lexer.scan();
                let type_parameters = Some(self.terminated(
                    Self::type_parameter,
                    TokenKind::Comma,
                    TokenKind::GreaterThan,
                ));
                self.expect(TokenKind::OpenParen, "parameter list");
                let parameters =
                    self.terminated(Self::parameter, TokenKind::Comma, TokenKind::CloseParen);
                self.expect(TokenKind::Arrow, "return type annotation");
                let typename = Some(self.type_node());
                let data = SignatureDeclarationData {
                    type_parameters,
                    parameters,
                    typename,
                };
                Ty::SignatureDeclaration(self.syntax(span.start, data))
            }
            TokenKind::OpenParen => {
                self.lexer.scan();
                let type_parameters = None;
                let parameters =
                    self.terminated(Self::parameter, TokenKind::Comma, TokenKind::CloseParen);
                self.expect(TokenKind::Arrow, "return type annotation");
                let typename = Some(self.type_node());
                let data = SignatureDeclarationData {
                    type_parameters,
                    parameters,
                    typename,
                };
                Ty::SignatureDeclaration(self.syntax(span.start, data))
            }
            TokenKind::Identifier => Ty::Identifier(self.identifier()),
            _ => {
                self.lexer.scan();
                self.error(span, Message::Expected { expected: "type" });
                Ty::Error
            }
        }
    }

    fn property_initializer(&mut self) -> PropertyInitializer {
        let start = self.lexer.span.start;
        let name = self.identifier();
        self.expect(TokenKind::Colon, "colon");
        let initializer = self.expression();
        let data = PropertyInitializerData { name, initializer };
        self.syntax(start, data)
    }

    fn property_declaration(&mut self) -> PropertyDeclaration {
        let start = self.lexer.span.start;
        let name = self.identifier();
        let typename = self.try_type_annotation();
        let data = PropertyDeclarationData { name, typename };
        self.syntax(start, data)
    }

    fn identifier(&mut self) -> Identifier {
        let kind = self.lexer.kind;
        let span = self.lexer.span;
        self.lexer.scan();
        let text = match kind {
            TokenKind::Identifier => Some(self.lexer.source[span].to_string()),
            _ => {
                self.error(
                    span,
                    Message::Expected {
                        expected: "identifier",
                    },
                );
                None
            }
        };
        self.syntax(span.start, text)
    }

    fn statement(&mut self) -> Statement {
        let start = self.lexer.span.start;
        match self.lexer.kind {
            TokenKind::KeywordVar => {
                self.lexer.scan();
                let name = self.identifier();
                let typename = self.try_type_annotation();
                self.expect(TokenKind::Equals, "'=' before initializer");
                let initializer = self.expression();
                let data = VarData {
                    name,
                    typename,
                    initializer,
                };
                Statement::Var(self.syntax(start, data))
            }
            TokenKind::KeywordType => {
                self.lexer.scan();
                let name = self.identifier();
                self.expect(TokenKind::Equals, "'=' before type");
                let typename = self.type_node();
                let data = TypeAliasData { name, typename };
                Statement::TypeAlias(self.syntax(start, data))
            }
            TokenKind::KeywordReturn => {
                self.lexer.scan();
                let expression = self.expression();
                let data = ReturnData { expression };
                Statement::Return(self.syntax(start, data))
            }
            _ => {
                let expression = self.expression();
                let data = ExpressionStatementData { expression };
                Statement::ExpressionStatement(self.syntax(start, data))
            }
        }
    }

    fn expect(&mut self, kind: TokenKind, expected: &'static str) -> bool {
        let ok = self.try_kind(kind);
        if !ok {
            self.expected_here(expected);
        }
        ok
    }

    fn expected_here(&mut self, expected: &'static str) {
        self.error(self.lexer.span, Message::Expected { expected })
    }

    fn error(&mut self, span: SourceSpan, message: Message) {
        self.reporter.error(span, message);
    }

    fn try_kind(&mut self, kind: TokenKind) -> bool {
        let ok = self.lexer.kind == kind;
        if ok {
            self.lexer.scan();
        }
        ok
    }

    fn terminated<Item>(
        &mut self,
        parse_item: fn(&mut Self) -> Item,
        separator: TokenKind,
        terminator: TokenKind,
    ) -> Vec<Item> {
        let first_span = self.lexer.span; // should be *previous* token?
        let mut list = vec![];
        while !self.try_kind(terminator) {
            if self.lexer.kind == TokenKind::End {
                self.error(first_span, Message::Unclosed);
                break;
            }
            list.push(parse_item(self));
            self.try_kind(separator);
        }
        list
    }
}
