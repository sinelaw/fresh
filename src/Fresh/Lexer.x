{
module Fresh.Lexer (LToken, AlexPosn, lexer) where

import Fresh.Token (Token(..))

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
  "."                           { \p _ -> TokenDot p            }
  "@"                           { \p _ -> TokenAt p             }
  "->"                          { \p _ -> TokenArrow p          }
  "//".*                        ;
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
  [\~\!\#\$\%\^\&\*\-\+\/]      { \p s -> TokenOp p s           }

{

type LToken = Token AlexPosn

lexer :: String -> [LToken]
lexer = alexScanTokens

}
