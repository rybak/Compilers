module AST where

data LType =
   TVoid
 | TInt
 | TFunType LType LType
  deriving (Eq,Ord,Show,Read)

data Stm =
   SAss PIdent Exp
 | SPrint Exp
  deriving (Eq,Ord,Show,Read)

data Exp =
   ECall PIdent [Exp]
 | EVar PIdent
 | EInt Integer
 | EBool Bool
  deriving (Eq,Ord,Show,Read)

