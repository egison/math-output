module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+/:<=>?@^_~#"

readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found val: " ++ showLispVal val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | NegativeAtom String
             | Plus [LispVal]
             | Multiply [LispVal]
             | Power LispVal LispVal
             deriving (Show) 

showLispVal :: LispVal -> String
showLispVal (Atom func) = func
showLispVal (NegativeAtom func) = "-" ++ func
showLispVal (Plus []) = ""
showLispVal (Plus [a]) = showLispVal a
showLispVal (Plus lvs) = (showLispVal (head lvs)) ++ " + " ++ (showLispVal (Plus (tail lvs)))
showLispVal (Multiply []) = ""
showLispVal (Multiply [a]) = showLispVal a
showLispVal (Multiply lvs) = (showLispVal2 (head lvs)) ++ " " ++ (showLispVal (Multiply (tail lvs)))

showLispVal2 :: LispVal -> String
showLispVal2 (Plus lvs) = "(" ++ showLispVal (Plus lvs) ++ ")"
showLispVal2 val = showLispVal val

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol <|> digit
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseNegativeAtom :: Parser LispVal
parseNegativeAtom = do
   char '-'
   first <- letter <|> symbol <|> digit
   rest <- many (letter <|> digit <|> symbol)
   let atom = [first] ++ rest
   return $ NegativeAtom atom

parseList :: Parser [LispVal]
parseList = sepBy parseExpr spaces

parsePower :: Parser LispVal
parsePower = do
    x <- parseExpr
    char '^'
    y <- parseExpr
    return $ Power x y

parsePlus :: Parser LispVal
parsePlus = do
    string "(+"
    spaces
    xs <- parseList
    char ')'
    return $ Plus xs

parseMultiply :: Parser LispVal
parseMultiply = do
    string "(*"
    spaces
    xs <- parseList
    char ')'
    return $ Multiply xs

parseExpr :: Parser LispVal
parseExpr = try parsePower
        <|> parseAtom
        <|> parseNegativeAtom
        <|> try parsePlus
        <|> try parseMultiply

