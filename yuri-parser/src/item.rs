use yuri_ast::types::{MatrixTy, WrittenTy};
use yuri_common::ScalarTy;
use yuri_lexer::TokenKind;

use crate::ParseState;
use crate::error::{ParseError, ParseTry};
use yuri_ast::item::{Attribute, FunctionItem, OuterDeclaration};
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
                    Ident::Keyword(Keyword::Fn) => {
                        self.function(has_export, attributes).map(Into::into)
                    }
                    Ident::Keyword(Keyword::Type) => todo!(),
                    Ident::Keyword(Keyword::Module) => todo!(),
                    Ident::Keyword(Keyword::Import) => todo!(),
                    _ => panic!("{ident:?}"),
                    // _ => Err(ParseError::UnexpectedToken {
                    //     token: tok.kind,
                    //     at: self.pos(),
                    // }),
                }
            }
            // attribute before something else
            token => panic!("{token:?}"),
            // Err(ParseError::UnexpectedToken {
            //     token,
            //     at: self.pos(),
            // }),
        }
    }

    pub fn function(
        &mut self,
        export: bool,
        attributes: Vec<Attribute>,
    ) -> Result<Box<FunctionItem>, ParseError> {
        {
            let tok = self.take().eof()?;
            debug_assert_eq!(self.token_to_ident(tok), Ident::Keyword(Keyword::Fn));
        }
        self.take_whitespace(true);
        let name = self.expect(TokenKind::Ident)?;
        let name = self.token_to_ident(name);

        self.expect(TokenKind::OpenParen)?;
        // TODO: take arguments
        self.take_whitespace(true);
        let mut parameters = Vec::new();
        self.expect(TokenKind::CloseParen)?;
        self.take_whitespace(true);
        // type annotation
        self.expect(TokenKind::Colon)?;
        self.take_whitespace(true);
        // TODO: optionally, return attributes
        let return_type = self.written_ty()?;
        self.take_whitespace(true);

        // function body
        let body = self.expr_block()?;

        Ok(FunctionItem {
            attributes,
            export,
            name,
            parameters,
            return_type,
            body,
        }
        .into())
    }

    pub fn written_ty(&mut self) -> Result<WrittenTy, ParseError> {
        let tok = self.peek().eof()?;
        match tok.kind {
            TokenKind::Ident => match self.token_to_ident(tok) {
                Ident::Id(_) => Ok(WrittenTy::Alias(self.qpath()?)),
                Ident::Keyword(Keyword::Mat(ty)) => {
                    self.skip();
                    Ok(WrittenTy::Matrix(ty))
                }
                Ident::Keyword(Keyword::Vec(ty)) => {
                    self.skip();
                    Ok(WrittenTy::Vector(ty))
                }
                Ident::Keyword(Keyword::Float(bits)) => {
                    self.skip();
                    Ok(WrittenTy::Scalar(ScalarTy::Float(bits)))
                }
                Ident::Keyword(Keyword::Unsigned(bits)) => {
                    self.skip();
                    Ok(WrittenTy::Scalar(ScalarTy::Unsigned(bits)))
                }
                Ident::Keyword(Keyword::Signed(bits)) => {
                    self.skip();
                    Ok(WrittenTy::Scalar(ScalarTy::Unsigned(bits)))
                }
                _ => Err(self.unexpected()),
            },
            TokenKind::OpenDoubleBrace => todo!(),
            TokenKind::OpenBracket => todo!(),
            _ => Err(self.unexpected()),
        }
    }

    pub fn alias(&mut self) -> Result<OuterDeclaration, ParseError> {
        todo!()
    }

    pub fn attributes(&mut self) -> Result<Vec<Attribute>, ParseError> {
        todo!()
    }
}
