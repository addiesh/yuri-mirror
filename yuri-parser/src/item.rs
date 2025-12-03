use yuri_ast::types::{MatrixTy, WrittenTy};
use yuri_common::ScalarTy;
use yuri_lexer::TokenKind;

use crate::ParseState;
use crate::error::{ParseError, ParseTry};
use yuri_ast::item::{Attribute, FunctionItem, OuterDeclaration, ParameterItem, VariableItem};
use yuri_ast::{Ident, Keyword};

impl<'src> ParseState<'src, '_> {
    pub fn outer_declaration(&mut self) -> Result<OuterDeclaration, ParseError> {
        let attributes = self.attributes()?;

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
                    _ => panic!("{ident:?} @ {}", self.pos()),
                    // _ => Err(ParseError::UnexpectedToken {
                    //     token: tok.kind,
                    //     at: self.pos(),
                    // }),
                }
            }
            // attribute before something else
            token => panic!("{token:?} @ {}", self.pos()),
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
            assert_eq!(self.token_to_ident(tok), Ident::Keyword(Keyword::Fn));
        }
        self.take_whitespace(true);
        let name = self.expect(TokenKind::Ident)?;
        let name = self.token_to_ident(name);

        let parameters = self.function_params()?;

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

    pub fn function_params(&mut self) -> Result<Vec<ParameterItem>, ParseError> {
        self.expect(TokenKind::OpenParen)?;
        let mut params = Vec::<ParameterItem>::new();
        self.take_whitespace(true);

        loop {
            // check if there's a close-paren at the beginning.
            let next = self.peek().eof()?;
            if next.kind == TokenKind::CloseParen {
                self.skip();
                break;
            }

            let attributes = self.attributes()?;
            let name = self.expect(TokenKind::Ident)?;
            let name = self.token_to_ident(name);
            self.take_whitespace(true);
            self.expect(TokenKind::Colon)?;
            self.take_whitespace(true);
            let explicit_type = self.written_ty()?;
            params.push(ParameterItem {
                attributes,
                name,
                explicit_type,
            });
            self.take_whitespace(true);
            let next = self.peek().eof()?;
            match next.kind {
                TokenKind::Comma => {
                    self.skip();
                    self.take_whitespace(true);
                    let next = self.peek().eof()?;
                    if next.kind == TokenKind::CloseParen {
                        self.skip();
                        break;
                    }
                    continue;
                }
                TokenKind::CloseParen => {
                    self.skip();
                    break;
                }
                _ => todo!("recover from unexpected before close paren in call expr"),
            }
        }
        Ok(params)
    }

    pub fn variable(&mut self) -> Result<VariableItem, ParseError> {
        {
            let tok = self.take().eof()?;
            assert_eq!(self.token_to_ident(tok), Ident::Keyword(Keyword::Let));
        }
        self.take_whitespace(true);
        let name = self.expect(TokenKind::Ident)?;
        let name = self.token_to_ident(name);

        self.take_whitespace(true);
        let next = self.peek().eof()?;
        let written_ty = if next.kind == TokenKind::Colon {
            Some(self.written_ty()?)
        } else {
            None
        };

        self.take_whitespace(true);
        self.expect(TokenKind::Eq)?;
        self.take_whitespace(true);

        let value = Box::new(self.expression()?);

        self.take_whitespace(false);
        self.expect(TokenKind::Newline)?;

        Ok(VariableItem {
            name,
            written_ty,
            value,
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
        let mut attributes = Vec::new();
        loop {
            let tok = self.peek().eof()?;
            if tok.kind == TokenKind::At {
                attributes.push(self.attribute()?);
            } else {
                break;
            }
            self.take_whitespace(true);
        }
        Ok(attributes)
    }

    pub fn attribute(&mut self) -> Result<Attribute, ParseError> {
        self.expect(TokenKind::At)?;
        self.take_whitespace(true);
        let path = self.qpath()?;
        let tok = self.peek().eof()?;
        if tok.kind == TokenKind::OpenParen {
            let args = Some(self.call()?);
            Ok(Attribute { path, args })
        } else {
            Ok(Attribute { path, args: None })
        }
    }
}
