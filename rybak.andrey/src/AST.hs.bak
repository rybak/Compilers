module AST (
  Identifier,
  BooleanExpr(..),
  ArithmeticExpr(..),
  RBinaryOp(..),
  ABinaryOp(..),
  
  ) where

type Identifier = String

data ArithmeticExpr = IntConst Int
                    | Variable Identifier
                    | ABinary ABinaryOp ArithmeticExpr ArithmeticExpr

data BooleanExpr = BoolConst Bool
                   | Variable Identifier
                   | Not BooleanExpr
                   | BBinary BBinaryOp BooleanExpr BooleanExpr
                   | RBinary RBinaryOp ArithmeticExpr ArithmeticExpr

data BBinaryOp = And, Or
data RBinaryOp = GT, LT, EQ, NE, GE, LE

data ABinaryOp = Add | Sub | Mul | Div | Mod 

data Expression = StringConst String
                  | AExpr ArithmeticExpr
                  | BExpr BooleanExpr
                  | FunctionCall Identifier [Expression]

type FunctionT = ([Type], Type)
data Type = IntegerType
            | StringType
            | BooleanType
            | UnionType
            | FunctionType FunctionT
type FunctionDefinition = FunctionDefinition { name :: Identifier,
                         ret  :: Type,
                         args :: [(Type, Identifier)],
                         body :: [Statement]
                         }
newtype Variable = (Identifier, Type)
data Statement = Skip
                 | Assign Identifier Expression
                 | Block [Statement]
                 | If BExpr Statement Statement
                 | While BExpr Statement
                 | Read Identifier
                 | Write Identifier
                 | VariableDecl Type Identifier 
                 | FuncDef FunctionDefinition
                 | 

