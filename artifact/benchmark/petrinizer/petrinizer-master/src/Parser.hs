module Parser
    (Parser, parseString, parseFile)
where

import Text.Parsec

type Parser a = Parsec String () a

parseString :: Parser a -> String -> a
parseString p str =
        case parse p "" str of
            Left e -> error $ show e
            Right r -> r

parseFile :: Parser a -> String -> IO a
parseFile p file =  do
        contents <- readFile file
        case parse p file contents of
            Left e -> print e >> fail "parse error"
            Right r -> return r
