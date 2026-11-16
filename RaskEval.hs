{-|
Module      : RaskEval
Description : The Raskell evaluator: special forms, function application, and
              primitive operations.

Everything you implement in Haskell for HW 9 is in this file. Search for
"TODO (Stage". The provided code above each TODO is a good model.
-}
module RaskEval where

import           RaskAST

import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

-- | Look up a name, or explain that it isn't bound.
envLookup :: String -> Env -> Either String Value
envLookup name env =
    case Map.lookup name env of
        Just v  -> Right v
        Nothing -> Left ("unbound identifier: " ++ name)

-- | Bind several names to several values, on top of an existing environment.
-- (New bindings shadow old ones with the same name.)
envExtend :: [String] -> [Value] -> Env -> Env
envExtend names vals env = Map.union (Map.fromList (zip names vals)) env

-- | The environment every program starts in: just the primitives.
initEnv :: Env
initEnv = Map.fromList [ (name, VPrim name f) | (name, f) <- primitives ]

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Evaluate an expression in an environment, producing either an error
-- message (Left) or a value (Right).
eval :: Env -> SExpr -> Either String Value

-- Numbers and Booleans evaluate to themselves.
eval _   (SNumber n) = Right (VNumber n)
eval _   (SBool b)   = Right (VBool b)

-- A symbol, when evaluated, is a variable: look it up.
eval env (SSymbol name) = envLookup name env

-- () is not a valid expression (quote it to get the empty list).
eval _   (SList []) = Left "empty application: () (did you mean '()?)"

-- Special forms: each has its own evaluation rule.
eval _   (SList (SSymbol "quote"  : parts)) = evalQuote  parts
eval env (SList (SSymbol "lambda" : parts)) = evalLambda env parts
eval env (SList (SSymbol "if"     : parts)) = evalIf     env parts
eval env (SList (SSymbol "cond"   : parts)) = evalCond   env parts
eval env (SList (SSymbol "and"    : parts)) = evalAnd    env parts
eval env (SList (SSymbol "or"     : parts)) = evalOr     env parts
eval env (SList (SSymbol "let"    : parts)) = evalLet    env parts
eval env (SList (SSymbol "letrec" : parts)) = evalLetrec env parts

-- Anything else in parentheses is an ordinary application: evaluate the
-- function and ALL the arguments (call by value), then apply.
eval env (SList (fexpr : argexprs)) = do
    f    <- eval env fexpr
    args <- mapM (eval env) argexprs
    apply f args

-- | Apply a function value to already-evaluated arguments.
apply :: Value -> [Value] -> Either String Value
apply (VPrim _ f) args = f args
apply (VClosure closureEnv params body) args
    | length params /= length args =
        Left (arityMessage "procedure" (length params) (length args))
    | otherwise =
        eval (envExtend params args closureEnv) body
apply notAFunction _ =
    Left ("not a procedure: " ++ showValue notAFunction)

--------------------------------------------------------------------------------
-- Special forms
--------------------------------------------------------------------------------

-- | (quote e): return e as data, without evaluating it.
evalQuote :: [SExpr] -> Either String Value
evalQuote [e] = Right (quoteToValue e)
evalQuote es  = Left (arityMessage "quote" 1 (length es))

-- | (lambda (params...) body): make a closure that remembers the CURRENT
-- environment (that's what makes scope lexical).
evalLambda :: Env -> [SExpr] -> Either String Value
evalLambda env [paramList, body] = do
    params <- parameterList paramList
    return (VClosure env params body)
evalLambda _ _ = Left "lambda: expected (lambda (params...) body)"

-- | (if test then else): evaluate test, then evaluate ONLY the chosen branch.
evalIf :: Env -> [SExpr] -> Either String Value
-- TODO (Stage 2): replace the line below.
evalIf _ _ = notImplemented "if"

-- | (cond (test result) ... (else result)): try the tests in order, and
-- evaluate the result of the first one that is #t.
evalCond :: Env -> [SExpr] -> Either String Value
-- Hint: condClauses (in RaskAST.hs) checks the clauses' shapes for you.
-- TODO (Stage 2): replace the line below.
evalCond _ _ = notImplemented "cond"

-- | (and e ...): #t unless some e is #f. Stops at the first #f.
evalAnd :: Env -> [SExpr] -> Either String Value
-- TODO (Stage 2): replace the line below.
evalAnd _ _ = notImplemented "and"

-- | (or e ...): #f unless some e is #t. Stops at the first #t.
evalOr :: Env -> [SExpr] -> Either String Value
-- TODO (Stage 2): replace the line below.
evalOr _ _ = notImplemented "or"

-- | (let ((name e) ...) body): evaluate every e in the CURRENT environment, then
-- evaluate body with the names bound. Implemented by translating into
--
--   ((lambda (name ...) body) e ...)
evalLet :: Env -> [SExpr] -> Either String Value
-- Hint: bindingList (in RaskAST.hs) checks the bindings' shape for you.
-- TODO (Stage 3): replace the line below.
evalLet _ _ = notImplemented "let"

-- | (letrec ((f (lambda (params...) fbody))) body): like let, but f's own body
-- can call f.
--
-- PROVIDED. The closure for f has to remember an environment in which f is
-- bound to... that same closure. Haskell lets us write that circular
-- definition directly: env' mentions the closure and the closure mentions
-- env'. Laziness makes it work, because neither is inspected until f is called.
evalLetrec :: Env -> [SExpr] -> Either String Value
evalLetrec env [bindings, body] = do
    pairs <- bindingList "letrec" bindings
    case pairs of
        [(f, SList [SSymbol "lambda", paramList, fbody])] -> do
            params <- parameterList paramList
            let closure = VClosure env' params fbody
                env'    = Map.insert f closure env
            eval env' body
        _ -> Left "letrec: expected exactly one binding of the form (f (lambda (params...) body))"
evalLetrec _ _ = Left "letrec: expected (letrec ((f (lambda (params...) body))) body)"

--------------------------------------------------------------------------------
-- Primitive operations
--
-- Each primitive receives its arguments ALREADY EVALUATED, as a list of values.
--------------------------------------------------------------------------------

-- | Every primitive, by the name a Raskell program uses for it.
primitives :: [(String, [Value] -> Either String Value)]
primitives =
    [ -- lists
      ("cons",     primCons)
    , ("first",    primFirst)
    , ("rest",     primRest)
    , ("list",     primList)
    , ("append",   primAppend)
      -- arithmetic
    , ("+",        arith "+" (+))
    , ("-",        arith "-" (-))
    , ("*",        arith "*" (*))
    , ("quotient", primQuotient)
    , ("modulo",   primModulo)
      -- comparisons
    , ("=",        compareWith "="  (==))
    , ("<",        compareWith "<"  (<))
    , (">",        compareWith ">"  (>))
    , ("<=",       compareWith "<=" (<=))
    , (">=",       compareWith ">=" (>=))
      -- Booleans
    , ("not",      primNot)
      -- higher-order list operations
    , ("map",      primMap)
    , ("foldl",    primFoldl)
    ]

-- | (cons x xs): a new list whose first element is x and the rest is xs.
primCons :: [Value] -> Either String Value
primCons args = do
    (x, xsv) <- twoArgs "cons" args
    xs <- expectList "cons" xsv
    return (VList (x : xs))

-- | (first xs): the first element of a nonempty list.
primFirst :: [Value] -> Either String Value
primFirst args = do
    xsv <- oneArg "first" args
    xs  <- expectList "first" xsv
    case xs of
        (x : _) -> Right x
        []      -> Left "first: expected a nonempty list, got ()"

-- | (rest xs): everything after the first element of a nonempty list.
primRest :: [Value] -> Either String Value
primRest args = do
    xsv <- oneArg "rest" args
    xs  <- expectList "rest" xsv
    case xs of
        (_ : rest) -> Right (VList rest)
        []         -> Left "rest: expected a nonempty list, got ()"

-- | (list v ...): a list of all the arguments.
primList :: [Value] -> Either String Value
-- TODO (Stage 4): replace the line below.
primList _ = notImplemented "list"

-- | (append xs ys): the elements of xs followed by the elements of ys.
primAppend :: [Value] -> Either String Value
-- TODO (Stage 4): replace the line below.
primAppend _ = notImplemented "append"

-- | A binary arithmetic operation on two numbers.
arith :: String -> (Integer -> Integer -> Integer) -> [Value] -> Either String Value
-- For example, the entry ("+", arith "+" (+)) in the table above means that
-- (+ 3 5) runs  arith "+" (+) [VNumber 3, VNumber 5].
-- Hint: twoNumbers (in RaskAST.hs) does the checking for you.
-- TODO (Stage 1): replace the line below.
arith op _ _ = notImplemented op

-- | (quotient a b): integer division, rounding toward zero (like Racket).
primQuotient :: [Value] -> Either String Value
-- TODO (Stage 1): replace the line below.
primQuotient _ = notImplemented "quotient"

-- | (modulo a b): the remainder, with the sign of b (like Racket).
primModulo :: [Value] -> Either String Value
-- TODO (Stage 1): replace the line below.
primModulo _ = notImplemented "modulo"

-- | A binary comparison of two numbers.
compareWith :: String -> (Integer -> Integer -> Bool) -> [Value] -> Either String Value
-- TODO (Stage 1): replace the line below.
compareWith op _ _ = notImplemented op

-- | (not b): the opposite Boolean.
primNot :: [Value] -> Either String Value
-- TODO (Stage 2): replace the line below.
primNot _ = notImplemented "not"

-- | (map f xs): the list of (f x) for each x in xs.
primMap :: [Value] -> Either String Value
-- Hint: use apply (above) to call f, which might be a primitive OR a closure.
-- TODO (Stage 4): replace the line below.
primMap _ = notImplemented "map"

-- | (foldl f init xs): combine the elements of xs from left to right. As in
-- Racket, f receives the ELEMENT FIRST and the accumulator second:
--
--   (foldl f init '(a b c))  =  (f c (f b (f a init)))
primFoldl :: [Value] -> Either String Value
-- Hint: use apply (above) to call f, which might be a primitive OR a closure.
-- TODO (Stage 4): replace the line below.
primFoldl _ = notImplemented "foldl"

--------------------------------------------------------------------------------
-- Placeholder for unfinished parts
--------------------------------------------------------------------------------

-- | The result of an operation you haven't implemented yet.
notImplemented :: String -> Either String Value
notImplemented name = Left (name ++ ": not implemented yet")
