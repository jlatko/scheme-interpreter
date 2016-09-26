{-# LANGUAGE ExistentialQuantification #-}
module Evaluation where
import Parsing
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad.Error
 

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

--------------- Evaluation ----------------
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             other  -> eval conseq
eval (List (Atom "cond" : cases)) = evalConditional evalTestCond cases
eval (List (Atom "case" : key : cases)) = do
                    caseOf <- eval key
                    evalConditional (evalTestCase caseOf) cases
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


evalConditional :: (LispVal -> ThrowsError (Bool, LispVal)) -> [LispVal] -> ThrowsError LispVal
evalConditional test [] = return $ Atom "unspecified"
evalConditional test (x:xs) = do
            result <- test x
            case result of 
                (True, expr) -> eval expr
                otherwise    -> evalConditional test xs 


evalTestCond :: LispVal -> ThrowsError (Bool, LispVal)
evalTestCond (List (test:[])) = do 
            result <- eval test
            isFalse <- (eqv $ result:(Bool False):[]) >>= unpackBool
            return (not isFalse, List [Atom "quote", result])
evalTestCond (List (Atom "else" : expr)) = return (True, last expr)
evalTestCond (List (test : Atom "=>" : expr)) = do 
            result <- eval test
            isFalse <- (eqv $ result:(Bool False):[]) >>= unpackBool
            expression <- insertToExpr (last expr) result
            return (not isFalse, expression)
evalTestCond (List (test : expr)) = do 
            result <- eval test
            isFalse <- (eqv $ result:(Bool False):[]) >>= unpackBool
            return (not isFalse, last expr )
evalTestCond otherwise = throwError $ BadSpecialForm "Unrecognized special form" otherwise

evalTestCase :: LispVal -> LispVal -> ThrowsError (Bool, LispVal)
evalTestCase key (List ( List clauses : expr)) = do
            let isTrue = map (eqvPair key) clauses
            return (or isTrue, last expr)
                where eqvPair x1 x2 = case eqv [x1, x2] of
                                Right (Bool val) -> val
evalTestCase _ (List ( Atom "else" : expr)) = return (True, last expr)
evalTestCase _ otherwise = throwError $ BadSpecialForm "Unrecognized form in case statement" otherwise

insertToExpr :: LispVal -> LispVal -> ThrowsError LispVal
insertToExpr (List list) arg = return $ List $ list ++ [arg]
insertToExpr otherwise _ = throwError $ BadSpecialForm "Unrecognized form in cond statement" otherwise

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
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
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

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-------------------------------


eqv :: [LispVal] -> ThrowsError LispVal
eqv = compareUsing eqvPair
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- compareUsing eqvPair [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
                where eqvPair (x1, x2) = case equal [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
equal badArgList = throwError $ NumArgs 2 badArgList

------ takes function to be used for list comparison
compareUsing :: ((LispVal,LispVal) -> Bool) -> [LispVal] -> ThrowsError LispVal
compareUsing _ [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
compareUsing _ [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
compareUsing _ [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
compareUsing _ [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
compareUsing eqvPair [(DottedList xs x), (DottedList ys y)] = compareUsing eqvPair [List $ xs ++ [x], List $ ys ++ [y]]
compareUsing eqvPair [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
compareUsing _ [_, _]                                 = return $ Bool False
compareUsing _ badArgList                             = throwError $ NumArgs 2 badArgList