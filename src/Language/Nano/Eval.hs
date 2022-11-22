module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1")))
-- 0
--
-- >>> eval env0 (EVar "p")
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt (EVar "z1") (EVar "x")) (EBin Ne (EVar "y") (EVar "z")) (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq (EVar "z1") (EVar "x")) (EBin Le (EVar "y") (EVar "z")) (EBin Le (EVar "z") (EVar "y"))
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus (EVar "x") (EVar "y")
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval env (EInt val)      =  VInt val  
eval env (EBool val)     =  VBool val
eval env (EVar var)      =  lookupId var env
eval env (EBin op s1 s2) =  evalOp op (eval env s1) (eval env s2)
eval env (ELet id s1 s2) =  eval ((id,(eval env s1)):env) s2
eval env (EIf s1 s2 s3)  |  (ifCheckHelper env s1) = eval env s2
                         |  otherwise              = eval env s3
eval env (ELam id s1)    =  VClos env id s1
eval env (EApp (EVar id) s2) = functionEvalVar (prelude++env) id s2
eval env (EApp (ELam id s1) s2) = eval (prelude++((id, (eval env s2)):env)) s1
eval env (EApp _ s3) = throw (Error "type error: applying argument to non function type")
eval env (ENil)          = VNil

closConverter :: Value -> (Env, (Id, Expr))
closConverter (VClos x y z) = (x, (y, z))

functionEvalVar :: Env -> Id -> Expr -> Value
functionEvalVar currEnv id exprs = eval funcEnv funcExpr
  where 
    funcEnv  = updateFuncEnv currEnv id (eval currEnv exprs)
    funcExpr = snd (snd clos)
      where
        clos = closConverter (lookupId id currEnv)

updateFuncEnv :: Env -> Id -> Value -> Env
updateFuncEnv currEnv id argVal = (id,closVal):((funcArg,argVal):funcEnv)
  where
    closVal   = lookupId id currEnv
    closTuple = closConverter closVal
    funcEnv   = fst closTuple 
    funcArg   = fst (snd closTuple)

--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus (VInt x) (VInt y)  = VInt (x+y)
evalOp Plus _ _                = throw (Error "type error: '+' binop")
evalOp Minus (VInt x) (VInt y) = VInt (x-y)
evalOp Minus _ _                = throw (Error "type error: '-' binop")
evalOp Mul (VInt x) (VInt y)   = VInt (x*y)
evalOp Mul _ _                = throw (Error "type error: '*' binop")
evalOp Eq (VInt x) (VInt y)    = VBool (x == y)
evalOp Eq (VBool x) (VBool y)  = VBool (x == y)
evalOp Eq _ _                  = throw (Error "type error: '==' binop")
evalOp Ne (VInt x) (VInt y)    = VBool (not (x == y))
evalOp Ne (VBool x) (VBool y)  = VBool (not (x == y))
evalOp Ne _ _                  = throw (Error "type error: '!=' binop")
evalOp Lt (VInt x) (VInt y)    = VBool (x < y)
evalOp Lt _ _                  = throw (Error "type error: '<' binop")
evalOp Le (VInt x) (VInt y)    = VBool (x <= y)
evalOp Le _ _                  = throw (Error "type error: '<=' binop")
evalOp And (VBool x) (VBool y) = VBool (x && y)
evalOp And _ _                 = throw (Error "type error: '&&' binop")
evalOp Or (VBool x) (VBool y)  = VBool (x || y)
evalOp Or _ _                  = throw (Error "type error: '||' binop")
evalOp Cons x y                = VCons x y 

ifCheckHelper :: Env -> Expr -> Bool
ifCheckHelper env s | (eval env s) == (VBool True)  = True
                    | (eval env s) == (VBool False) = False
                    | otherwise           = throw (Error "type error: if conditional")


--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId key []     = throw (Error ("unbound variable: " ++ key))
lookupId key (x:xs) | key == fst x = snd x
                    | otherwise    = lookupId key xs

takeHead:: Value -> Value
takeHead (VCons x y) = x
takeHead _           = throw (Error "takeHead only accepts a list as an argument.")

takeTail:: Value -> Value
takeTail (VCons x y) = y
takeTail _           = throw (Error "takeHead only accepts a list as an argument.")

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
