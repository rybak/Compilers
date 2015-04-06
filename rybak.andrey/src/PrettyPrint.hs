module PrettyPrint (
  ) where

import AST
import Lexer (languageDef)
-- Warning! Not accurate to the language definition.

ppStatement :: Statement -> String
ppStatement Skip = "skip;"
ppStatement Assign name expr = name ++ " = " ++ prettyPrint expr ++ ";"
ppStatement Block statements = intercalate ";\n" $ map prettyPrint statements
ppStatement If cond t e = "if " ++ parentheses (ppBExpr cond)
                       ++ "\nthen\n" ++ ppStatement t
                       ++ "\nelse\n" ++ ppStatement e ++ "\nfi"
ppStatement While cond body = "while " ++ parentheses (ppBExpr cond)
                              ++ ppBody ++ "\nend;"
ppStatement Read name = "read" ++ parentheses name ++ ";"
ppStatement Write name = "write" ++ parentheses name ++ ";"
ppStatement VariableDecl t name = ppType t ++ " " ++ name ++ ";"
ppStatement FuncDef fd = ppFuncDef fd
ppStatement _  = "not defined pretty print"

ppFuncDef :: FunctionDefinition -> String
ppFuncDef fd = name fd ++ " " ++ parentheses ppArgs ++ " -> " ++ ppType (ret fd) ++ "\n{\n" ++ ppStatement (Block $ body fd) ++ "\n}\n" where
  ppArgs = intercalate ", " $ map printArg $ args fd where
    printArg (t, name) = ppType t ++ " " ++ name
ppType :: Type -> String
ppType IntegerType = "int"
ppType StringType  = "str"
ppType BooleanType = "bool"
ppType UnionType   = "union"
ppType FunctionType (args, ret) = parentheses $ parentheses ( intercalate ", " $ map ppType args ) ++ " -> " ++ ppType ret

ppExpression :: Expression -> String
ppExpression StringConst = (enclosed '"') . escape
ppExpression AExpr ae = ppAExpr ae
ppExpression BExpr be = ppBExpr be
ppExpression FunctionCall name args = name ++ parentheses ppArgs where
  ppArgs = intercalate ", " $ map ppExpression args

ppAExpr :: ArithmeticExpr -> String
ppAExpr IntConst Int   = show Int
ppAExpr Variable name  = name
ppAExpr ABinary op a b = parentheses (ppAExpr a ++ operator (opStr op) ++ ppAExpr b) where
  opStr Add = "+"
  opStr Sub = "-"
  opStr Mul = "*"
  opStr Div = "/"
  opStr Mod = "%"

ppBExpr :: BooleanExpr -> String
ppBExpr BoolConst x = show x
ppBExpr Variable name = name
ppBExpr Not x = "not " ++ parentheses (ppBExpr x)
ppBExpr BBinary op a b = parentheses (ppBExpr a ++ operator (opStr op) ++ ppBExpr b) where
  opStr And = "and"
  opStr Or  = "or"
ppBExpr RBinary op a b = parentheses (ppAExpr a ++ operator (opStr op) ++ ppAExpr b) where
  opStr GT = ">"
  opStr LT = "<"
  opStr EQ = "=="
  opStr NE = "!="
  opStr LE = "<="
  opStr GE = ">="

enclosed :: Char -> String -> String
enclosed c s = (c : s) ++ [c]
operator :: String -> String
operator = enclosed ' '
parentheses :: String -> String
parentheses s = ('(' : s) ++ ")"
               
escape :: String -> String
escape = show

