module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found val: " ++ showLispVal val

data LispVal = Atom String
             | NegativeAtom String
             | Plus [LispVal]
             | Multiply [LispVal]
             | Power LispVal LispVal
             | Func LispVal [LispVal]
             | Tensor [LispVal] [LispVal] [LispVal]
             deriving (Show, Eq) 

showLispVal :: LispVal -> String
showLispVal (Atom func) = func
showLispVal (NegativeAtom func) = "-" ++ func
showLispVal (Plus []) = ""
showLispVal (Plus [a]) = showLispVal a
showLispVal (Plus lvs) = case (lvs !! 1) of
                           NegativeAtom na -> (showLispVal (head lvs)) ++ " - " ++ na ++ (showLispVal (Plus $ tail $ tail lvs))
                           Plus (NegativeAtom na:rest) -> (showLispVal (head lvs)) ++ " - " ++ na ++ " + " ++ (showLispVal (Plus $ rest ++ (tail $ tail lvs)))
                           Multiply (NegativeAtom na:rest) -> (showLispVal (head lvs)) ++ " - " ++ na ++ " " ++ (showLispVal (Plus $ rest ++ (tail $ tail lvs)))
                           _ -> (showLispVal (head lvs)) ++ " + " ++ (showLispVal (Plus $ tail lvs))
showLispVal (Multiply []) = ""
showLispVal (Multiply [a]) = showLispVal a
showLispVal (Multiply lvs) = (showLispVal2 (head lvs)) ++ " " ++ (showLispVal (Multiply (tail lvs)))
showLispVal (Power lv1 lv2) = (showLispVal lv1) ++ "^" ++ (showLispVal lv2)
showLispVal (Func f lvs) = case f of
                             Atom "/" -> if (length lvs) == 2 then "frac{" ++ (showLispVal (lvs !! 0)) ++ "}{" ++ (showLispVal (lvs !! 1)) ++ "}"
                                                         else (showLispVal f) ++ "(" ++ (showLispValArg lvs) ++ ")"
                             _ -> (showLispVal f) ++ "(" ++ (showLispValArg lvs) ++ ")"
-- showLispVal (Tensor lvs us ds) = "(" ++ (showLispValArg lvs) ++ ")"
showLispVal (Tensor lvs us ds)
    | us == [] = "(" ++ (showLispValArg lvs) ++ ")"
    | otherwise = "(" ++ (showLispValArg lvs) ++ ")_(" ++ (showLispValArg2 us) ++ ")"

showLispVal2 :: LispVal -> String
showLispVal2 (Plus lvs) = "(" ++ showLispVal (Plus lvs) ++ ")"
showLispVal2 val = showLispVal val

showLispValArg :: [LispVal] -> String
showLispValArg [] = ""
showLispValArg [a] = showLispVal a
showLispValArg lvs = (showLispVal (head lvs)) ++ ", " ++ (showLispValArg $ tail lvs)

showLispValArg2 :: [LispVal] -> String
showLispValArg2 [a] = showLispVal a
showLispValArg2 lvs = (showLispVal (head lvs)) ++ (showLispValArg2 $ tail lvs)

spaces :: Parser ()
spaces = skipMany1 space

spaces0 :: Parser ()
spaces0 = skipMany space

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@#"

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
parseList = sepEndBy parseExpr spaces

parseUsList :: Parser [LispVal]
parseUsList = sepEndBy parseExpr $ char '_'

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

parseFunction :: Parser LispVal
parseFunction = do
    char '('
    func <- parseAtom
    spaces
    xs <- parseList
    char ')'
    return $ Func func xs

parseTensor :: Parser LispVal
parseTensor = do
    -- between lp rp $ Tensor <$> parseList
    --     where lp = string "[|" >> spaces0
    --           rp = spaces0 >> string "|]"
    string "[|"
    spaces0
    xs <- parseList
    spaces0
    string "|]"
    option (Tensor xs [] []) $ try $ char '_' >> parseUsList >>= \us -> return $ Tensor xs us []
    
parseExpr' :: Parser LispVal
parseExpr' = parseNegativeAtom
        <|> parseAtom
        <|> try parsePlus
        <|> try parseMultiply
        <|> try parseFunction
        <|> try parseTensor

parseExpr :: Parser LispVal
parseExpr = do
    x <- parseExpr'
    option x  $ Power x <$> (try $ char '^' >> parseExpr')

