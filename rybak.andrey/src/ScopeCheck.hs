module ScopeCheck where

import Lang.Abs
import Lang.ErrM

import Control.Monad (unless, when)
import Utils.SM
import Data.Maybe
import qualified Data.Map as M

data SymTabKinds = STFun Name [ParLType] ParLType
                 | STVar Name Type deriving (Eq,Show)
type Scope = (String,[Integer]) -- function name and nesting
type Name = String
type SymTab = M.Map Scope (M.Map Name SymTabKinds)

data BuildSt = St { mainFound :: Bool,
                    syms :: SymTab,
                    scope :: Scope,
                    counter :: Integer,
                    errs :: [String]
                    } deriving Show
type Result = SM BuildSt ()

nameToString :: PIdent -> String
nameToString (PIdent ((_,_), str)) = str

stKindToName :: SymTabKinds -> Name
stKindToName (STFun name _ _) = name
stKindToName (STVar name _) = name

checkScope :: ParProgram -> BuildSt
checkScope prog = let
  ((), buildStGlobal) = runState (collectGlobal prog) (St False predefinedFuncs ("", []) 0 [])
  in
    snd $ runState (buildSTProgram prog) buildStGlobal

getMainFound :: SM BuildSt Bool 
getMainFound = SM (\st -> (mainFound st,st))

setMainFound :: Bool -> SM BuildSt ()
setMainFound b = SM (\st -> ((), St b (syms st) (scope st) (counter st) (errs st)  ))

getErrs :: SM BuildSt [String]
getErrs = SM (\st -> (errs st,st))

setErrs :: [String] -> SM BuildSt ()
setErrs strs = SM (\st -> ((), St (mainFound st) (syms st) (scope st) (counter st) strs ))

addToErrs :: String -> SM BuildSt ()
addToErrs str = do
  errors <- getErrs
  setErrs (str:errors)

getCounter :: SM BuildSt Integer
getCounter = SM (\st -> (counter st,st))

setCounter :: Integer -> SM BuildSt ()
setCounter cntr = SM (\st -> ((), St (mainFound st) (syms st) (scope st) cntr (errs st)   ))

incrCounter :: SM BuildSt ()
incrCounter = do
  cntr <- getCounter
  setCounter (cntr + 1)

getScope :: SM BuildSt Scope
getScope = SM (\st -> (scope st,st))

setScope :: Scope -> SM BuildSt ()
setScope scp = SM (\st -> ((),St (mainFound st) (syms st) scp (counter st) (errs st)  ))

pushScope :: SM BuildSt ()
pushScope = do
  ctr <- getCounter
  (str,l) <- getScope
  setScope (str,ctr:l)

popScope :: SM BuildSt ()
popScope = do
  (str,l:ls) <- getScope
  setScope (str,ls)

resetScope :: SM BuildSt ()
resetScope = do
  setCounter 0
  setScope ("",[])

getSyms :: SM BuildSt SymTab
getSyms = SM (\st -> (syms st,st))

setSyms :: SymTab -> SM BuildSt ()
setSyms symtab = SM (\st -> ((), St (mainFound st) symtab (scope st) (counter st) (errs st) ))


