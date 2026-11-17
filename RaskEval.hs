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

-- | Look up a name, or stop with an error if it isn't bound.
envLookup :: String -> Env -> Value
envLookup name env =
    case Map.lookup name env of
        Just v  -> v
        Nothing -> error ("unbound identifier: " ++ name)

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

-- | Evaluate an expression in an environment, producing a value.
eval :: Env -> SExpr -> Value

-- Numbers and Booleans evaluate to themselves.
eval _   (SNumber n) = VNumber n
eval _   (SBool b)   = VBool b

-- A symbol, when evaluated, is a variable: look it up.
eval env (SSymbol name) = envLookup name env

-- () is not a valid expression (quote it to get the empty list).
eval _   (SList []) = error "empty application: () (did you mean '()?)"

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
-- (forceValue and forceAll, from RaskAST.hs, make Haskell finish computing
-- them before the call. You don't need to read how they work.)
eval env (SList (fexpr : argexprs)) =
    apply (forceValue (eval env fexpr)) (forceAll (map (eval env) argexprs))

-- | Apply a function value to already-evaluated arguments.
apply :: Value -> [Value] -> Value
apply (VPrim _ f) args = f args
apply (VClosure closureEnv params body) args
    | length params /= length args =
        error ("procedure: expected " ++ show (length params)
               ++ " arguments, got " ++ show (length args))
    | otherwise =
        eval (envExtend params args closureEnv) body
apply notAFunction _ =
    error ("not a procedure: " ++ showValue notAFunction)

--------------------------------------------------------------------------------
-- Special forms
--
-- Each one receives its parts UNEVALUATED, as a list of SExprs.
--------------------------------------------------------------------------------

-- | (quote e): return e as data, without evaluating it.
evalQuote :: [SExpr] -> Value
evalQuote [e] = quoteToValue e

-- | (lambda (params...) body): make a closure that remembers the CURRENT
-- environment (that's what makes scope lexical).
evalLambda :: Env -> [SExpr] -> Value
evalLambda env [SList params, body] = VClosure env (map symbolName params) body

-- | (if test then else): evaluate test, then evaluate ONLY the chosen branch.
evalIf :: Env -> [SExpr] -> Value
-- Hint: use case on the value of the test, as described on the HW page.
-- TODO (Stage 2): replace the line below.
evalIf _ _ = notImplemented "if"

-- | (cond (test result) ... (else result)): try the tests in order, and
-- evaluate the result of the first one that is #t.
evalCond :: Env -> [SExpr] -> Value
-- Hint: one equation for an else clause, one for a (test result) clause.
-- TODO (Stage 2): replace the line below.
evalCond _ _ = notImplemented "cond"

-- | (and e ...): #t unless some e is #f. Stops at the first #f.
evalAnd :: Env -> [SExpr] -> Value
-- TODO (Stage 2): replace the line below.
evalAnd _ _ = notImplemented "and"

-- | (or e ...): #f unless some e is #t. Stops at the first #t.
evalOr :: Env -> [SExpr] -> Value
-- TODO (Stage 2): replace the line below.
evalOr _ _ = notImplemented "or"

-- | (let ((name e) ...) body): evaluate every e in the CURRENT environment, then
-- evaluate body with the names bound. Implemented by translating into
--
--   ((lambda (name ...) body) e ...)
evalLet :: Env -> [SExpr] -> Value
-- TODO (Stage 3): replace the line below.
evalLet _ _ = notImplemented "let"

-- | (letrec ((f (lambda (params...) fbody))) body): like let, but f's own body
-- can call f.
--
-- PROVIDED. The closure for f has to remember an environment in which f is
-- bound to... that same closure. Haskell lets us write that circular
-- definition directly: env' mentions the closure and the closure mentions
-- env'. Laziness makes it work, because neither is inspected until f is called.
evalLetrec :: Env -> [SExpr] -> Value
evalLetrec env [SList [SList [SSymbol f, SList [SSymbol "lambda", SList params, fbody]]], body] =
    let closure = VClosure env' (map symbolName params) fbody
        env'    = Map.insert f closure env
    in  eval env' body

--------------------------------------------------------------------------------
-- Primitive operations
--
-- Each primitive receives its arguments ALREADY EVALUATED, as a list of values.
-- It only needs an equation for arguments of the right shape. Anything else
-- matches no equation, and Raskell reports that as an error.
--------------------------------------------------------------------------------

-- | Every primitive, by the name a Raskell program uses for it.
primitives :: [(String, [Value] -> Value)]
primitives =
    [ -- lists
      ("cons",     primCons)
    , ("first",    primFirst)
    , ("rest",     primRest)
    , ("list",     primList)
    , ("append",   primAppend)
      -- arithmetic
    , ("+",        arith (+))
    , ("-",        arith (-))
    , ("*",        arith (*))
    , ("quotient", primQuotient)
    , ("modulo",   primModulo)
      -- comparisons
    , ("=",        compareWith (==))
    , ("<",        compareWith (<))
    , (">",        compareWith (>))
    , ("<=",       compareWith (<=))
    , (">=",       compareWith (>=))
      -- Booleans
    , ("not",      primNot)
      -- higher-order list operations
    , ("map",      primMap)
    , ("foldl",    primFoldl)
    ]

-- | (cons x xs): a new list whose first element is x and the rest is xs.
primCons :: [Value] -> Value
primCons [x, VList xs] = VList (x : xs)

-- | (first xs): the first element of a nonempty list.
primFirst :: [Value] -> Value
primFirst [VList (x : _)] = x

-- | (rest xs): everything after the first element of a nonempty list.
primRest :: [Value] -> Value
primRest [VList (_ : rest)] = VList rest

-- | (list v ...): a list of all the arguments.
primList :: [Value] -> Value
-- TODO (Stage 4): replace the line below.
primList _ = notImplemented "list"

-- | (append xs ys): the elements of xs followed by the elements of ys.
primAppend :: [Value] -> Value
-- TODO (Stage 4): replace the line below.
primAppend _ = notImplemented "append"

-- | A binary arithmetic operation on two numbers.
arith :: (Integer -> Integer -> Integer) -> [Value] -> Value
-- For example, the entry ("+", arith (+)) in the table above means that
-- (+ 3 5) runs  arith (+) [VNumber 3, VNumber 5].
-- Hint: pattern-match the arguments you expect, as primCons does.
-- TODO (Stage 1): replace the line below.
arith _ _ = notImplemented "arithmetic"

-- | (quotient a b): integer division, rounding toward zero (like Racket).
primQuotient :: [Value] -> Value
-- TODO (Stage 1): replace the line below.
primQuotient _ = notImplemented "quotient"

-- | (modulo a b): the remainder, with the sign of b (like Racket).
primModulo :: [Value] -> Value
-- TODO (Stage 1): replace the line below.
primModulo _ = notImplemented "modulo"

-- | A binary comparison of two numbers.
compareWith :: (Integer -> Integer -> Bool) -> [Value] -> Value
-- TODO (Stage 1): replace the line below.
compareWith _ _ = notImplemented "comparison"

-- | (not b): the opposite Boolean.
primNot :: [Value] -> Value
-- TODO (Stage 2): replace the line below.
primNot _ = notImplemented "not"

-- | (map f xs): the list of (f x) for each x in xs.
primMap :: [Value] -> Value
-- Hint: use apply (above) to call f, which might be a primitive OR a closure.
-- TODO (Stage 4): replace the line below.
primMap _ = notImplemented "map"

-- | (foldl f init xs): combine the elements of xs from left to right. As in
-- Racket, f receives the ELEMENT FIRST and the accumulator second:
--
--   (foldl f init '(a b c))  =  (f c (f b (f a init)))
primFoldl :: [Value] -> Value
-- Hint: use apply (above) to call f, which might be a primitive OR a closure.
-- TODO (Stage 4): replace the line below.
primFoldl _ = notImplemented "foldl"

--------------------------------------------------------------------------------
-- Placeholder for unfinished parts
--------------------------------------------------------------------------------

-- | Stop with a message saying this part isn't implemented yet.
notImplemented :: String -> a
notImplemented name = error (name ++ ": not implemented yet")
