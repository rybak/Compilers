import Prelude
import System.IO

import AbsL
import ParL (pExp)
import LexL (tokens)
import qualified ErrM
import qualified Data.Map.Strict as Map

type Ident = String
type Environment = Map.Map Ident Integer

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
  fromInteger = Ok

eval :: Environment -> Exp -> Result
eval env (EAdd a b ) = eval env a + eval env b
eval env (ESub a b ) = eval env a - eval env b
eval env (EMul a b ) = eval env a * eval env b
eval env (EDiv a b ) = case eval env b of
                     Ok 0 -> Error $ "Div by zero"
                     Error s -> Error $ "div error in denominator: " ++ s
                     Ok m -> case eval env a of
                       Ok n -> Ok (n `div` m)
                       Error s -> Error $ "div error in numerator: " ++ s
eval env (ENegInt x) = - (eval env x)
eval _ (EInt x) = Ok x
eval env (EVar (PIdent ((p1,p2),ident))) = case Map.lookup ident env of
                          Just x -> Ok x
                          Nothing -> Error $ "Name error: " ++ ident
-- eval env (EAss ident expr) = case eval env expr of
 --                          Error s -> Error $ "Can't assign '" ++ ident ++ "': " ++ s
 --                          Ok x -> 
eval _ _ = Error "Not implemented operation."

parse = pExp . tokens

repl :: Environment -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  s <- getLine
  case s of
    "quit" -> return ()
    _ -> do
      putStrLn $ case parse s of
        ErrM.Ok e  -> s ++ " = " ++ (show e)
        ErrM.Bad s -> "Error: " ++ s
      repl env

main = repl Map.empty
