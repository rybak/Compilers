import Prelude
import AbsL
import ParL (pExp)
import LexL (tokens)
import qualified ErrM

data Result = Ok Integer | Error String deriving (Eq, Show)

instance Num Result where
  Ok a + Ok b = Ok $ a + b
  Error a + Error b = Error $ a ++ ". " ++ b
  Error a + _ = Error a
  _ + Error b = Error b
  Ok a * Ok b = Ok $ a * b
  Error a * Error b = Error $ a ++ ". " ++ b
  Error a * _ = Error a
  _ * Error b = Error b
  abs (Ok n) = if n > 0 then Ok n else (Ok $ -n)
  abs (Error s) = Error $ "can't `abs`: " ++ s
  negate (Ok n) = (Ok $ -n)
  negate (Error s) = Error $ "can't `negate`: " ++ s
  signum (Ok n) = Ok $ signum n
  signum (Error s) = Error $ "can't `signum`: " ++ s

eval :: Exp -> Result
eval (EAdd a b ) = eval a + eval b
eval (ESub a b ) = eval a - eval b
eval (EMul a b ) = eval a * eval b
eval (EDiv a b ) = case eval b of
                     Ok 0 -> Error $ "Div by zero"
                     Error s -> Error $ "div error in denominator: " ++ s
                     Ok m -> case eval a of
                       Ok n -> Ok (n `div` m)
                       Error s -> Error $ "div error in numerator: " ++ s
eval (ENegInt x) = - eval x
eval (EInt x) = Ok x


parse = pExp . tokens

main = do
  s <- getContents
  let
    result = case parse s of
      ErrM.Ok e  -> s ++ " = " ++ (show $ eval e)
      ErrM.Bad s -> "Error: " ++ s
    in putStrLn result
