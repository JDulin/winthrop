-- John Dulin
-- January 27, 2012
-- Hi, my name ith Winthrop!
-- I'm a thimple lithp proceththor!

module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
	  putStrLn (readExpr (args !! 0))

data LispVal = Atom String
	| String String
	| Bool Bool
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer

symbol :: Parser Char
symbol = oneOf "!$+-*/^_&|%<=?>#~:@"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
	Left err -> "No match " ++ show err
	Right err -> "Found Value"
