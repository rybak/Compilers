import Prelude
import System.IO
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)

import Control.Monad.Trans
import System.Console.Haskeline

import Lang.Abs
import Lang.Par (pParExp, pParStm, pParLType)
import Lang.Lex (tokens, Token)
import qualified Lang.ErrM as ErrM

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

eval :: Environment -> ParExp -> Result
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
 --                          Ok x -> 
eval _ _ = Error "Not implemented operation."

parseExp = pParExp . tokens
parseStm = pParStm . tokens
parseType = pParLType . tokens

data ReplCommand = Statement String | Expr String | Type String

specialPrefix = " "
typePrefix = ":t "
parseReplCmd s
--  | s == "quit"             = Quit
  | specialPrefix `isPrefixOf` s = Expr $ drop (length specialPrefix) s
  | typePrefix `isPrefixOf` s = Type $ drop (length typePrefix) s
  | otherwise               = Statement s

myerror :: String -> String
myerror = (++) "Error: "
--printExp :: Environment -> String -> IO ()
printExp env s = outputStrLn $ case parseExp s of
  ErrM.Ok e  -> s ++ " = " ++ (show $ eval env e) ++ "\n" ++ (show e)
  ErrM.Bad s -> myerror s

-- printType :: String -> IO ()
printType s = outputStrLn $ case parseType s of
  ErrM.Ok t  -> s ++ " ~ " ++ (show t)
  ErrM.Bad s -> myerror s

varName :: PIdent -> String
varName (PIdent ((_,_), s)) = s

main = runInputT defaultSettings (repl Map.empty) where
  repl env = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just s -> case parseReplCmd s of
        Statement stm -> case parseStm stm of
          ErrM.Ok (SAss v e) -> case eval env e of
            Ok n -> repl (Map.insert (varName v) n env)
            Error s -> (outputStrLn $ "Can't assign '" ++ (varName v) ++ "': " ++ s) >> repl env
          ErrM.Ok (SPrint e) -> (outputStrLn (show $ eval env e)) >> repl env
          ErrM.Ok x          -> (outputStrLn ("not supported : " ++ show x)) >> repl env
          ErrM.Bad s -> (outputStrLn $ "Parsing error: " ++ s) >> repl env
        Expr expr -> (printExp env expr) >> repl env
        Type t    -> (printType t) >> repl env

--    repl Map.empty
