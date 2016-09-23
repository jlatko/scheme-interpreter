module Evaluation where
import Parsing
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad.Error
 


--------------- Evaluation ----------------
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", expr]) = evalQuasi expr
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- ,@ syntax not supported yet
evalQuasi :: LispVal -> ThrowsError LispVal
evalQuasi (List [Atom "unquote", expr]) = eval expr
--evalQuasi (List [Atom "unquote-splicing", expr]) = eval expr
evalQuasi (List expr) = mapM evalQuasi expr >>= (return . List)
evalQuasi val = return val

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args) $ lookup func primitives


------------------ Functions ----------------------
primitives :: [(String, [LispVal] ->ThrowsError  LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", singleArg (\x -> case x of
                                    String _ -> return $ Bool True
                                    _ ->  return $ Bool False)),
              ("number?", singleArg (\x -> case x of
                                    String _ ->  return $ Bool True
                                    _ ->  return $ Bool False)),
              ("symbol?", singleArg (\x -> case x of
                                    String _ -> return $  Bool True
                                    _ -> return $  Bool False)),
              ("boolean?", singleArg (\x -> case x of
                                    String _ -> return $  Bool True
                                    _ ->  return $ Bool False)),
              ("symbol->string", singleArg (\x -> case x of
                                        Atom val ->  return $ String val
                                        n -> throwError $ TypeMismatch "Symbol" n)),

              ("string->symbol", singleArg (\x -> case x of
                                        String val -> return $ Atom val
                                        n -> throwError $ TypeMismatch "String" n )),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))
              ]

-- wrapper for single argument functions
singleArg :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
singleArg op [] = throwError $ NumArgs 2 []
singleArg op (x:[]) = op x
singleArg op values@(x:y:_) = throwError $ NumArgs 2 values

---- using haskell binary operators
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

---- list handling
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

----------------------- LispVal type conversion ---------------------
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n  in
                            if null parsed 
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool