module Parser.LOLAFormula
    (parseFormula)
where

import Control.Applicative ((*>),(<$>))
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (LanguageDef, emptyDef)
import qualified Text.Parsec.Token as Token

import Parser
import Property

languageDef :: LanguageDef ()
languageDef =
        emptyDef {
                 Token.commentStart    = "{",
                 Token.commentEnd      = "}",
                 Token.commentLine     = "",
                 Token.identStart      = noneOf ",;:(){}\t \n\r0123456789-",
                 Token.identLetter     = noneOf ",;:(){}\t \n\r",
                 Token.reservedNames   = ["FORMULA", "TRUE", "FALSE",
                                          "NOT", "AND", "OR"],
                 Token.reservedOpNames = ["<", "<=", "=", "!=", ">=", ">",
                                          "+", "-", "*"]
                 }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer -- parses an identifier
reserved :: String -> Parser ()
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator
parens :: Parser a -> Parser a
parens     = Token.parens     lexer -- parses p surrounded by parenthesis
integer :: Parser Integer
integer    = Token.integer    lexer -- parses an integer
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace


binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name fun = Infix  ( reservedOp name *> return fun )
prefix :: String -> (a -> a) -> Operator String () Identity a
prefix name fun = Prefix ( reservedOp name *> return fun )

termOperatorTable :: [[Operator String () Identity (Term String)]]
termOperatorTable =
        [ [ prefix "-" Minus ]
        , [ binary "*" (:*:) AssocLeft ]
        , [ binary "+" (:+:) AssocLeft, binary "-" (:-:) AssocLeft ]
        ]

termAtom :: Parser (Term String)
termAtom =  (Var <$> identifier)
        <|> (Const <$> integer)
        <|> parens term
        <?> "basic term"

term :: Parser (Term String)
term = buildExpressionParser termOperatorTable termAtom <?> "term"

parseOp :: Parser Op
parseOp = (reservedOp "<" *> return Lt) <|>
          (reservedOp "<=" *> return Le) <|>
          (reservedOp "=" *> return Eq) <|>
          (reservedOp "!=" *> return Ne) <|>
          (reservedOp ">" *> return Gt) <|>
          (reservedOp ">=" *> return Ge)

linIneq :: Parser (Formula String)
linIneq = do
        lhs <- term
        op <- parseOp
        rhs <- term
        return (LinearInequation lhs op rhs)

binaryName :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binaryName name fun = Infix  ( reserved name *> return fun )
prefixName :: String -> (a -> a) -> Operator String () Identity a
prefixName name fun = Prefix ( reserved name *> return fun )

formOperatorTable :: [[Operator String () Identity (Formula String)]]
formOperatorTable =
        [ [ prefixName "NOT" Neg ]
        , [ binaryName "AND" (:&:) AssocRight ]
        , [ binaryName "OR"  (:|:) AssocRight ]
        ]

formAtom :: Parser (Formula String)
formAtom =  try linIneq
        <|> (reserved "TRUE" *> return FTrue)
        <|> (reserved "FALSE" *> return FFalse)
        <|> parens formula
        <?> "basic formula"

formula :: Parser (Formula String)
formula = buildExpressionParser formOperatorTable formAtom <?> "formula"

parseFormula :: Parser (Formula String)
parseFormula = do
        whiteSpace
        reserved "FORMULA"
        f <- formula
        eof
        return f
