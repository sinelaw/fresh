{
module Fresh.Lexer (Token(..), lexer) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]        -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                       ;
  "<"                           { \_ -> TokenTriangleOpen   }
  ">"                           { \_ -> TokenTriangleClose  }
  "("                           { \_ -> TokenParenOpen      }
  ")"                           { \_ -> TokenParenClose     }
  "{"                           { \_ -> TokenBraceOpen      }
  "}"                           { \_ -> TokenBraceClose     }
  "="                           { \_ -> TokenEq             }
  ";"                           { \_ -> TokenSemi           }
  ":"                           { \_ -> TokenColon          }
  ","                           { \_ -> TokenComma          }
  "@"                           { \_ -> TokenAt             }
  "->"                          { \_ -> TokenArrow          }
  "//".*                        { \s -> TokenComment s      }
  $digit+                       { \s -> TokenInt (read s)   }
  union                         { \_ -> TokenTUnion         }
  var                           { \_ -> TokenVar            }
  func                          { \_ -> TokenFunc           }
  switch                        { \_ -> TokenSwitch         }
  case                          { \_ -> TokenCase           }
  return                        { \_ -> TokenReturn         }
  lam                           { \_ -> TokenLam            }
  $lower [$alpha $digit \_ \']* { \s -> TokenIdent s        }
  $upper [$alpha $digit \_ \']* { \s -> TokenConstr s       }

{

data Token
    = TokenIdent String
    | TokenTypeIdent String
    | TokenConstr String
    | TokenTUnion
    | TokenFunc
    | TokenSwitch
    | TokenCase
    | TokenReturn
    | TokenLam
    | TokenVar
    | TokenColon
    | TokenTriangleOpen
    | TokenTriangleClose
    | TokenParenOpen
    | TokenParenClose
    | TokenBraceOpen
    | TokenBraceClose
    | TokenArrow
    | TokenComma
    | TokenEq
    | TokenSemi
    | TokenAt
    | TokenComment String
    | TokenOp String
    | TokenInt Int
    deriving (Eq,Show)

lexer = alexScanTokens

}
