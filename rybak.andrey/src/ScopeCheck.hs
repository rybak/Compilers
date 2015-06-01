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

-- SM helper functions
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

-- helper for building error messages:
getErrorStringCoords :: PIdent -> String
getErrorStringCoords (PIdent ((x,y),name)) = name ++ " at line " ++ showx ++ " col " ++ show y
--

-- Collect global symbols
collectGlobal :: ParProgram -> Result
collectGlobal (Prog topLvls) = collectGlobalFuns topLvls

collectGlobalFuns :: [ParTopLevel] -> Result
collectGlobalFuns [] = return ()
collectGlobalFuns (t:ts) do
  collectGlobalFun t
  collectGlobalFuns ts

mainFunctionName :: String
mainFunctionName = "main"
mainFunctionType :: ParLType
mainFunctionType = TFunType [] TVoid
isMainFunction :: PIdent -> Bool
isMainFunction name = (nameToString name == mainFunctionName)

duplicateError :: PIdent -> SM BuildSt ()
duplicateError name = addToErrs ("duplicate declared : " ++ getErrorStringCoords name)
wrongDefError :: PIdent -> ParLType -> SM BuildSt ()
wrongDefError name typ = addToErrs ("Definition doesn't meet type : " ++ getErrorStringCoords name ++ " should be " ++ show typ)

collectGlobalFun :: ParTopLevel -> Result
collectGlobalFun (Decl name typ') =
  case (isMainFunction name) of
    True -> case (typ' == mainFunctionType) of
      False -> do
        addToErrs ("main must be of type " ++ show mainFunctionType ++ " : " ++ (getErrorStringLoc name))
        return ()
      True  -> do -- main function is void, adding to symtab
        let stKind = STFun (nameToString name) [] TVoid
        added <- addSymCurScope stKind
        when (isNothing added) (duplicateError name)
        return ()
    False -> case typ' of
      TFunType argsTypes retType -> undefined -- TODO
      _ -> -- add? global variable

      return () -- TODO
collectGlobalFun (FuncDef name args body) =
  case (isMainFunction name) of
    True -> case args of
      [] -> do -- ok
        setMainFound True
        return ()
      _  -> do
        wrongDefError name mainFunctionType
        return ()
  


