module Parsing where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad.Error

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
instance Show LispError where show = showError
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

data LispVal = Atom String
         | List [LispVal]
         | DottedList [LispVal] LispVal
         | Number Integer
         | String String
         | Bool Bool
         | Char Char
instance Show LispVal where show = showVal

----------------- Error handling ------------------
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val



------------ Showing values ------------------
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


--------------- Parsing --------------------

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many ( (char '\\' >> (char '\"'
                                       <|> char 'n'
                                       <|> char 'r'
                                       <|> char 't'
                                       <|> char '\\'))
                            <|> noneOf "\"" )
                char '"'
                return $ String x

parseChar :: Parser LispVal
parseChar = do
                char '#'
                char '\\'
                x <- (liftM (:[]) (letter 
                        <|> digit 
                        <|> symbol )
                    <|> (string "space")
                    <|> (string "newline")
                    )
                return $ case x of
                    "space" -> Char ' '
                    "newline" -> Char '\n'
                    a:[]    -> Char a

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- liftM ------
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- do ---------
--parseNumber :: Parser LispVal
--parseNumber = do
--  x <- many1 digit
--  return $  Number . read $ x

--binToDec :: String -> Int
--binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


------ parse floats, hex, oct and bin -  not supported yet---
--parseNumber :: Parser LispVal
--parseNumber = (( optional (char '-')) >> (((many1 digit) >>= (return . Number . Integer . read ))
--                                       <|> ((((many1 digit) >> (char '.') >> (many1 digit) ))  >>= (return . Number . Float. fst . (!! 0) . readFloat . tail . tail))))
--         <|> ( char '#' >> ((char 'x' >> (many1 $ oneOf "0123456789aAbBcCdDeEfF") >>= (return . Number . Integer . fst . (!! 0) . readHex . tail . tail))
--                       <|> (char 'o' >> (many1 $ oneOf "01234567") >>= (return . Number . Integer . fst . (!! 0) . readOct . tail . tail))
--                       <|> (char 'b' >> (many1 $ oneOf "01") >>= (return . Number . Integer . toInteger . binToDec . tail . tail))
--                       <|> (char 'd' >> ((many1 digit) 
--                                  <|> ((many1 digit) >> (char '.') >> (many1 digit) ))  >>= (return . Number . Float . fst . (!! 0) . readFloat . tail . tail))
--                       ))

-- parseFloat :: Parser LispVal
-- parseFloat =  (char '#' >> char 'd' >> ((many1 digit) 
--                                  <|> ((many1 digit) >> (char '.') >> (many1 digit) ))  >>= (return . LFloat . fst . (!! 0) . readFloat . tail . tail))
--          <|> ((( optional (char '-')) >> (many1 digit) >> (char '.') >> (many1 digit) )  >>= (return . LFloat . fst . (!! 0) . readFloat . tail . tail))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do 
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $DottedList head tail

parseListWithComma :: Parser LispVal
parseListWithComma = liftM List $ sepBy parseExprWithComma spaces

parseDottedListWithComma :: Parser LispVal
parseDottedListWithComma = do 
    head <- endBy parseExprWithComma spaces
    tail <- char '.' >> spaces >> parseExprWithComma
    return $DottedList head tail

parseExprWithComma :: Parser LispVal
parseExprWithComma = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseBackquoted
         <|> parseComma
         <|> do 
                char '('
                x <- try parseListWithComma <|> parseDottedListWithComma
                char ')'
                return x

parseComma :: Parser LispVal
parseComma = do
    c <- char ','
    at <-optionMaybe $ char '@'
    x <- parseExpr
    return $ case at of 
        Just '@' -> List[Atom "unquote-splicing", x]
        _  -> List[Atom "unquote", x]

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr

    return $ List[Atom "quote", x]

parseBackquoted :: Parser LispVal
parseBackquoted = do
    char '`'
    x <- parseExprWithComma
    return $ List[Atom "quasiquote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseBackquoted
         <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val