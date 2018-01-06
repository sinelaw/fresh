{
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Fresh.Lexer (Token(..), LToken,  AlexPosn, lexer) where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]        -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                       ;
  "<"                           { \p _ -> TokenTriangleOpen p   }
  ">"                           { \p _ -> TokenTriangleClose p  }
  "("                           { \p _ -> TokenParenOpen p      }
  ")"                           { \p _ -> TokenParenClose p     }
  "{"                           { \p _ -> TokenBraceOpen p      }
  "}"                           { \p _ -> TokenBraceClose p     }
  "="                           { \p _ -> TokenEq p             }
  ";"                           { \p _ -> TokenSemi p           }
  ":"                           { \p _ -> TokenColon p          }
  ","                           { \p _ -> TokenComma p          }
  "@"                           { \p _ -> TokenAt p             }
  "->"                          { \p _ -> TokenArrow p          }
  "//".*                        { \p s -> TokenComment p s      }
  $digit+                       { \p s -> TokenInt p (read s)   }
  union                         { \p _ -> TokenTUnion p         }
  var                           { \p _ -> TokenVar p            }
  func                          { \p _ -> TokenFunc p           }
  switch                        { \p _ -> TokenSwitch p         }
  case                          { \p _ -> TokenCase p           }
  return                        { \p _ -> TokenReturn p         }
  lam                           { \p _ -> TokenLam p            }
  $lower [$alpha $digit \_ \']* { \p s -> TokenIdent p s        }
  $upper [$alpha $digit \_ \']* { \p s -> TokenConstr p s       }

{

data Token a
    = TokenIdent a String
    | TokenTypeIdent a String
    | TokenConstr a String
    | TokenTUnion a
    | TokenFunc a
    | TokenSwitch a
    | TokenCase a
    | TokenReturn a
    | TokenLam a
    | TokenVar a
    | TokenColon a
    | TokenTriangleOpen a
    | TokenTriangleClose a
    | TokenParenOpen a
    | TokenParenClose a
    | TokenBraceOpen a
    | TokenBraceClose a
    | TokenArrow a
    | TokenComma a
    | TokenEq a
    | TokenSemi a
    | TokenAt a
    | TokenComment a String
    | TokenOp a String
    | TokenInt a Int
    deriving (Eq,Show,Functor,Foldable)

type LToken = Token AlexPosn

lexer :: String -> [LToken]
lexer = alexScanTokens

}
