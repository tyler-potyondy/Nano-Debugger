{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0, evalWrapper, evalS2, evalBrick
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser
import Control.Monad.State

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
execExpr e = return (evalWrapper e (prelude,[prelude])) `catch` exitError

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
eval s e = evalState (evalS e) s

evalS :: Expr -> State Env Value
evalS (EInt i)  = do { return (VInt i)}
evalS (EBool b) = do { return (VBool b)}
evalS ENil      = do { return VNil }
evalS (EVar id) = do
                    gets (lookupId id);

evalS (EBin binop e1 e2) = do
                            e1s <- evalS e1
                            e2s <- evalS e2
                            return (evalOp binop e1s e2s)

evalS (EIf e1 e2 e3) = do
                        e1s <- evalS e1
                        case e1s of
                          VBool True  -> evalS e2
                          VBool False -> evalS e3
                          _           -> throw (Error "type error")

evalS (ELet id e1 e2) = do
                          e1s <- evalS e1
                          e <- get
                          put (insertIntoEnv [(id, e1s)] e)
                          evalS e2

evalS (EApp e1@(EVar fname) e2) = do
                                    e1eval <- evalS e1
                                    e2eval <- evalS e2
                                    case e1eval of
                                      VPrim f -> return (f e2eval)
                                      (VClos fro lhs body) -> do
                                                                let newEnv = insertIntoEnv [(fname, e1eval), (lhs, e2eval)] fro
                                                                return (evalState (evalS body) newEnv)

evalS (EApp inner e3) = do
                          innerEval <- evalS inner
                          case innerEval of
                            VClos fro lhs res -> do
                                                    e3Eval <- evalS e3
                                                    let newEnv = insertIntoEnv [(lhs, e3Eval)] fro
                                                    return (evalState (evalS res) newEnv)
                            _                 -> throw (Error "type error")


evalS (ELam id e) = do {env <- get; return (VClos env id e)}

--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus  (VInt v1)  (VInt v2)     = VInt (v1 + v2)
evalOp Plus  _          _             = throw (Error "type error")

evalOp Minus (VInt v1)  (VInt v2)     = VInt (v1 - v2)
evalOp Minus _          _             = throw (Error "type error")

evalOp Mul   (VInt v1)  (VInt v2)     = VInt (v1 * v2)
evalOp Mul   _          _             = throw (Error "type error")

evalOp Div   (VInt _)  (VInt 0)      = throw (Error "Divide by zero")
evalOp Div   (VInt v1)  (VInt v2)     = VInt (v1 `div` v2)
evalOp Div   _          _             = throw (Error "type error")

evalOp Eq    x1 x2 = if isComparable x1 && isComparable x2 then VBool (x1 == x2) else throw (Error "type error")

evalOp Ne    (VInt v1)  (VInt v2)     = VBool (v1 /= v2)
evalOp Ne    (VBool v1) (VBool v2)    = VBool (v1 /= v2)
evalOp Ne    _          _             = throw (Error "type error")

evalOp Lt    (VInt v1)  (VInt v2)     = VBool (v1 < v2)
evalOp Lt    _          _             = throw (Error "type error")

evalOp Le    x          y             = evalOp Or (evalOp Lt x y) (evalOp Eq x y)

evalOp And   (VBool x)  (VBool y)     = VBool (x && y)
evalOp And   _          _             = throw (Error "type error")

evalOp Or    (VBool x)  (VBool y)     = VBool (x || y)
evalOp Or    _          _             = throw (Error "type error")

evalOp Cons  (VInt x)   VNil                         = VCons (VInt x) VNil
evalOp Cons  (VInt x)   rest@(VCons (VInt _) _)      = VCons (VInt x) rest
evalOp Cons  (VBool x)   VNil                        = VCons (VBool x) VNil
evalOp Cons  (VBool x)   rest@(VCons (VBool _) _)    = VCons (VBool x) rest
evalOp Cons  _          _                            = throw (Error "type error")

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
lookupId id []     = throw (Error ("unbound variable: " ++ id))
lookupId id (f:rest)
                    | fst f == id = snd f
                    | otherwise = lookupId id rest


insertIntoEnv :: [(Id, Value)] -> Env -> Env
insertIntoEnv r env = foldl (flip (:)) env r


prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
    ("head", VPrim (\(VCons x _) -> x)),
    ("tail", VPrim (\(VCons _ r) -> r))
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------


evalBrick :: Expr -> (Env,[Env]) -> (Value, (Env,[Env]))
evalBrick expr state = runState (evalS2 expr) state

evalWrapper :: Expr -> (Env,[Env]) -> Value
evalWrapper i x = do 
                    evalState (evalS2 i) x

getEnv :: State (Env,[Env]) ()
getEnv = do
          (currEnv,envList) <- get 
          put (currEnv,currEnv:envList)

evalS2 :: Expr -> State (Env,[Env]) Value
evalS2 (EInt i)  = do { getEnv; return (VInt i) }
evalS2 (EBool b) = do { getEnv; return (VBool b) }
evalS2 ENil      = do { getEnv; return VNil }
evalS2 (EVar id) = do
                    getEnv
                    (env,_) <- get
                    return (lookupId id env)

evalS2 (EBin binop e1 e2) = do
                              getEnv
                              e1s <- evalS2 e1
                              e2s <- evalS2 e2
                              return (evalOp binop e1s e2s)

evalS2 (EIf e1 e2 e3) = do
                          getEnv
                          e1s <- evalS2 e1
                          case e1s of
                            VBool True  -> evalS2 e2
                            VBool False -> evalS2 e3
                            _           -> throw (Error "type error")

evalS2 (ELet id e1 e2) = do
                          getEnv
                          e1s <- evalS2 e1
                          (e,store) <- get
                          put ((insertIntoEnv [(id, e1s)] e),store)
                          evalS2 e2

evalS2 (EApp e1@(EVar fname) e2) = do
                                    e1eval <- evalS2 e1
                                    e2eval <- evalS2 e2
                                    case e1eval of
                                      VPrim f -> return (f e2eval)
                                      (VClos fro lhs body) -> do
                                                                let newEnv = insertIntoEnv [(fname, e1eval), (lhs, e2eval)] fro
                                                                (_,store) <- get
                                                                return (evalWrapper body (newEnv,store))
                                      _ -> throw (Error "type error")
                                                                
                                                                

evalS2 (EApp inner e3) = do
                          innerEval <- evalS2 inner
                          case innerEval of
                            VClos fro lhs res -> do
                                                    e3Eval <- evalS2 e3
                                                    let newEnv = insertIntoEnv [(lhs, e3Eval)] fro
                                                    (_,store) <- get
                                                    return (evalWrapper res (newEnv,store))
                            _                 -> throw (Error "type error")


evalS2 (ELam id e) = do {getEnv; (env,_) <- get; return (VClos env id e)}


--test expression (ELet "x" (EInt 3) (EBin Plus (EVar x) (EVar x)))
-- >>> let e1 = EBin Plus (EVar "x")  (EVar "y")
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3

--IGNORE THESE FUNCTION...USED FOR TESTING--
-- evalPrinter::Expr -> (Env,[Env]) -> IO Value
-- evalPrinter expr state = do 
--                             let (a, state2) = runState (evalS2 expr) state
--                             print (snd state2)
--                             return a

-- testExecExpr :: Expr -> IO Value
-- --------------------------------------------------------------------------------
-- testExecExpr e = evalPrinter e (prelude,[prelude]) `catch` exitError

-- execFile2 :: FilePath -> IO Value
-- --------------------------------------------------------------------------------
-- execFile2 f = (readFile f >>= execString2) `catch` exitError

-- execString2 :: String -> IO Value
-- --------------------------------------------------------------------------------
-- execString2 s = testExecExpr (parseExpr s) `catch` exitError
