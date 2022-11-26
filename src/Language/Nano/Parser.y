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

Expr : if Expr then Expr else Expr { EIf $2 $4 $6 }
     | let Ids '=' Expr in Expr    { mkLet $2 $4 $6 }
     | Expr2                       { $1 }

Expr2 : '\\' Ids '->' Expr          { mkLam $2 $4 }
      | Expr3                      { $1 }

Expr3 : Expr '||' Expr             { EBin Or $1 $3}
      | Expr4                     { $1 }

Expr4 : Expr '&&' Expr             { EBin And $1 $3}
      | Expr5                     { $1 }

Expr5 : Expr '==' Expr             { EBin Eq $1 $3}
      | Expr '/=' Expr             { EBin Ne $1 $3}
      | Expr '<=' Expr             { EBin Le $1 $3}
      | Expr '<' Expr              { EBin Lt $1 $3}
      | Expr6                     { $1 }

Expr6 : Expr ':' Expr             { EBin Cons $1 $3}
      | Expr7                     { $1 }

Expr7 : Expr '+' Expr             { EBin Plus $1 $3}
      | Expr '-' Expr             { EBin Minus $1 $3}
      | Expr8                     { $1 }

Expr8 : Expr '*' Expr             { EBin Mul $1 $3}
      | Expr9                     { $1 }

Expr9 : TNUM                     { EInt $1 }
      | true                     { EBool True }
      | false                    { EBool False }
      | Expr Expr                { EApp $1 $2 }
      | '(' Expr ')'             { $2 }
      | ID                       { EVar $1 }

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
