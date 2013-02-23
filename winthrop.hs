-- John Dulin
-- John Dulin
-- February 2013
-- Hi, my name ith Winthrop!
-- I'm a thimple lithp proceththor!

module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

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

parseAtom :: Parser LispVal
parseAtom = do first <- symbol <|> letter
	       rest <- symbol <|> number <|> letter
	       return $ case atom of
               		"#t" -> Bool True
			"#f" -> Bool False
			otherwise -> Atom atom


parseString :: Parser LispVal
parseString = do char '"'
		 x <- many (noneOf "\""
		 char '"'
		 return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
	    <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match " ++ show err
	Right err -> "Found Value"
