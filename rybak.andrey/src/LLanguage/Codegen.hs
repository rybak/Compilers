{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
module LLanguage.Codegen where

import L.Abs
import L.ErrM

import LLanguage.Annotated
import LLanguage.Utils

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Global as G
import LLVM.General.AST.Instruction
import LLVM.General.AST.Operand
import qualified LLVM.General.Module as CModule

import Data.Monoid
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad.State hiding (void)
import Control.Applicative
import qualified Data.Map as M

-- data Binding = Global String
--     deriving (Eq, Ord, Show, Read)

isVarArgSupported = False

newtype LLVM a = LLVM { unLLVM :: State Module a }
    deriving (Functor, Applicative, Monad, MonadState Module )
runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

data BlockState = BlockState {
    idx :: Int,
    stack :: [Named Instruction],
    term :: Maybe (Named Terminator)
} deriving Show

type CGSymTab = [(String, Operand)]
data CodegenState = CodegenState {
    currentBlock :: Name,
    blocks :: M.Map Name BlockState,
    symtab :: CGSymTab,
    blockCount :: Int,
    count :: Word,
    names :: Names
}

newtype Codegen a = Codegen {
    runCodegen :: State CodegenState a
} deriving (Functor, Applicative, Monad, MonadState CodegenState)

codegen :: AProgram (Maybe ParLType) -> Module
codegen (AProg topLevels) = runLLVM commemorativeModule gp where
    gp = mapM codegenTop topLevels

codegenTop :: ATopLevel (Maybe ParLType) -> LLVM ()
codegenTop (ATopFun pi args rt body) = do
    define (compileName pi) cArgs (compileType rt) bb
    where
        cArgs = compileParameters args
        bb = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a@(ADec pi t) -> do
                var <- alloca t
                let llt = compileType t
                store llt var (local (compileName pi) llt)
                assign (pIdentToString pi) var
            x <- codegenFunctionBody body
            case x of
                Just r -> do
                    ret r
                    return ()
                _ -> return ()
codegenTop (ATopDecl (ADec pi t)) = addDefinition $ compileGlobalVar pi t

compileGlobalVar :: PIdent -> ParLType -> Definition
compileGlobalVar pi t = GlobalDefinition $ G.globalVariableDefaults {
    G.name = compileName pi,
    G.type' = compileType t
}

compileName pi = Name $ pIdentToString pi
compileType TVoid = T.void
compileType TInt = T.i32
compileType (TFun ats rt) = T.ptr $
    T.FunctionType (compileType rt) (map compileType ats) isVarArgSupported

compileParameters :: [ADecl] -> [(T.Type, Name)]
compileParameters = map compileParameter -- TODO add "([Parameter], Bool)
compileParameter (ADec pi t) = ((compileType t), (compileName pi))

codegenFunctionBody :: ABlock (Maybe ParLType) -> Codegen (Maybe Operand)
codegenFunctionBody (ABlockB astms) = do
    mapM_ codegenStm astms
    return Nothing

codegenStm :: AStm a -> Codegen ()
codegenStm (ASDecl (ADec pi t)) = do
    i <- alloca t
    let val = ConstantOperand $ C.Int 32 0 -- TODO add other types
    store (compileType t) i val
    assign (pIdentToString pi) i
codegenStm (ARet e) = return ()
codegenStm _ = return ()

my_name p v = Name $ p ++ v

--
fresh :: Codegen Word
fresh = do
    i <- gets count
    let newCount = i + 1
    modify $ \s -> s { count = newCount }
    return newCount
instr :: T.Type -> Instruction -> Codegen Operand
instr t i = do
    n <- fresh
    let ref = (UnName n)
    b <- current
    let s = stack b
    modifyBlock (b { stack = s ++ [ref := i] } )
    return $ local ref t
terminator :: Named Terminator -> Codegen (Named Terminator)
terminator t = do
    b <- current
    modifyBlock (b { term = Just t })
    return t
current :: Codegen BlockState
current = do
    b <- gets currentBlock
    bs <- gets blocks
    case M.lookup b bs of
        Just x -> return x
        Nothing -> internalError $ "no such block : " ++ show b

alloca :: ParLType -> Codegen Operand
alloca t = instr llt $ Alloca llt Nothing 0 [] where
    llt = compileType t

store :: T.Type -> Operand -> Operand -> Codegen Operand
store t ptr val = instr t $ Store False ptr val Nothing 0 []
--
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
-- LLVM vars infrastructure
type Names = M.Map String Int
startIndex :: Int
startIndex = 0
uniqueName :: String -> Names -> (String, Names)
uniqueName name names = maybe
    (name ++ show startIndex, M.insert name startIndex names)
    (\i -> let newIndex = i + 1 in
        (name ++ show newIndex, M.insert name newIndex names))
    (M.lookup name names)

-- symtab
assign :: String -> Operand -> Codegen ()
assign var x = do
    localVars <- gets symtab
    modify $ \s -> s { symtab = [(var, x)] ++ localVars }
getvar :: String -> Codegen Operand
getvar var = do
    st <- gets symtab
    case lookup var st of
        Just x -> return x
        Nothing -> internalError $ "Local var not in scope: " ++ show var
-- refs
local :: Name -> T.Type -> Operand
local n t = LocalReference t n
    
-- global defs
addDefinition :: Definition -> LLVM ()
addDefinition d = do
    ds <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = ds ++ [d] }
define :: Name -> [(T.Type, Name)] -> T.Type -> [G.BasicBlock] -> LLVM ()
define label args rt body = addDefinition $
    GlobalDefinition $ G.functionDefaults {
      G.name = label
    , G.parameters = ([Parameter t n [] | (t, n) <- args], isVarArgSupported)
    , G.returnType = rt
    , G.basicBlocks = body
    }
-- module 
commemorativeModule = emptyModule "vit"
emptyModule label = defaultModule { moduleName = label }

-- Blocks

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [G.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

makeBlock :: (Name, BlockState) -> G.BasicBlock
makeBlock (n, (BlockState _ s t)) = G.BasicBlock n s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = internalError $ "Block has no terminator: " ++ (show n)

entry :: Codegen Name
entry = gets currentBlock
entryBlockName = "entry"
emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing
addBlock :: String -> Codegen Name
addBlock n = do
    bs <- gets blocks
    i <- gets blockCount
    ns <- gets names
    let newBlock = emptyBlock i
        (un, uns) = uniqueName n ns
        llName = Name un
    modify $ \s -> s {
          blocks = M.insert llName newBlock bs
        , blockCount = i + 1
        , names = uns
        }
    return llName

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = M.insert active new (blocks s) }
-- CodegenState
emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty [] 1 0 M.empty
execCodegen :: Codegen a -> CodegenState
execCodegen cg = execState (runCodegen cg) emptyCodegen

--
internalError s = error $ "Internal error: " ++ s
