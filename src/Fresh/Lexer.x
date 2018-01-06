{
module Fresh.Lexer (main) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]        -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                       ;
  "<"                           { \s -> TokenTriangleOpen   }
  ">"                           { \s -> TokenTriangleClose  }
  "("                           { \s -> TokenParenOpen      }
  ")"                           { \s -> TokenParenClose     }
  "{"                           { \s -> TokenBraceOpen      }
  "}"                           { \s -> TokenBraceClose     }
  "="                           { \s -> TokenEq             }
  ";"                           { \s -> TokenSemi           }
  ":"                           { \s -> TokenColon          }
  ","                           { \s -> TokenComma          }
  "@"                           { \s -> TokenAt             }
  "->"                          { \s -> TokenArrow          }
  "//".*                        { \s -> TokenComment s      }
  $digit+                       { \s -> TokenInt (read s)   }
  union                         { \s -> TokenTUnion         }
  var                           { \s -> TokenVar            }
  func                          { \s -> TokenFunc           }
  switch                        { \s -> TokenSwitch         }
  case                          { \s -> TokenCase           }
  return                        { \s -> TokenReturn         }
  lam                           { \s -> TokenLam            }
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

main = do
  s <- getContents
  print (alexScanTokens s)
}
