use yuri_lexer::TokenKind;

use crate::ParseState;
use crate::error::{ParseError, ParseTry};
use yuri_ast::item::{Attribute, OuterDeclaration};
use yuri_ast::{Ident, Keyword};

impl<'src> ParseState<'src, '_> {
    pub fn outer_declaration(&mut self) -> Result<OuterDeclaration, ParseError> {
        let tok = self.peek().eof()?;

        let attributes = if tok.kind == TokenKind::At {
            self.attributes()?
        } else {
            Vec::new()
        };

        let tok = self.peek().eof()?;
        let has_export = if tok.kind == TokenKind::Ident {
            let matches = matches!(self.token_to_ident(tok), Ident::Keyword(Keyword::Export));
            if matches {
                self.skip();
            }
            matches
        } else {
            false
        };

        match tok.kind {
            TokenKind::Ident => {
                let ident = self.token_to_ident(tok);
                match ident {
                    Ident::Keyword(Keyword::Fn) => self.function(attributes),
                    Ident::Keyword(Keyword::Type) => todo!(),
                    Ident::Keyword(Keyword::Module) => todo!(),
                    Ident::Keyword(Keyword::Import) => todo!(),
                    _ => Err(ParseError::UnexpectedToken {
                        token: tok.kind,
                        at: self.pos(),
                    }),
                }
            }
            // attribute before something else
            token => Err(ParseError::UnexpectedToken {
                token,
                at: self.pos(),
            }),
        }
    }

    pub fn function(&mut self, attributes: Vec<Attribute>) -> Result<OuterDeclaration, ParseError> {
        todo!()
    }

    pub fn alias(&mut self) -> Result<OuterDeclaration, ParseError> {
        todo!()
    }

    pub fn attributes(&mut self) -> Result<Vec<Attribute>, ParseError> {
        todo!()
    }
}
