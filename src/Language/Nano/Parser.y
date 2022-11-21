{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }

-- Operators
%right in
%nonassoc '=' if then else
%right '->'
%left '||'
%left '&&'
%nonassoc '==' '/=' '<' '<='
%right ':'
%left '+' '-'
%left '*'
%%

Top  : ID '=' Expr                 { $3 }
     | TNUM '=' TNUM               { EInt 0 }
     | Expr                        { $1 }



BinOp : '+'  {Plus}
      | '-'  {Minus}
      | '*'  {Mul}
      | '||' {Or}
      | '&&' {And}
      | '==' {Eq}
      | '/=' {Ne}
      | '<=' {Le}
      | '<'  {Lt}

Ids : ID         { [$1] } 
    | ID Ids     { $1 : $2 }

Expr : TNUM                        { EInt $1 }
     | ID                          { EVar $1 } 
     | let Ids '=' Expr in Expr    { mkLet $2 $4 $6 }
     | Expr BinOp Expr             { EBin $2 $1 $3 }
     | '(' Expr ')'                { $2 }
     | '\\' Ids '->' Expr          { mkLam $2 $4 }
     | true                        { EBool True }
     | false                       { EBool False }
     | if Expr then Expr else Expr { EIf $2 $4 $6 }
     | Expr Expr                   { EApp $1 $2 }

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

mkLet :: [Id] -> Expr -> Expr -> Expr
mkLet []     e1 e2 = e2
mkLet (x:xs) e1 e2 = ELet x (mkLam xs e1) e2

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
