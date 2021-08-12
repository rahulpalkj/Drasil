module Language.Drasil.Printing.Import.DisplayExpr (dispExpr) where

import Language.Drasil.Development (dePrec, dePrecAssoc, DisplayExpr(..),
  DisplayBinOp(..), DisplayAssocBinOp(Equivalence))

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, ckdb, stg)

import Language.Drasil.Printing.Import.Expr (expr)
import Language.Drasil.Printing.Import.Space (space)
import Data.List (intersperse)
import Control.Lens ((^.))
import Language.Drasil.Printing.Import.Helpers (lookupC, parens)
import Language.Drasil.Printing.Import.Symbol (symbol)

-- | Helper that adds parenthesis to a display expression where appropriate.
dispExpr' :: PrintingInformation -> Int -> DisplayExpr -> P.Expr
dispExpr' s p e = fence $ dispExpr e s
  where fence = if dePrec e < p then parens else id

-- | Convert 'DisplayBinOps' into the operators of the AST language.
deBinOp :: DisplayBinOp -> P.Ops
deBinOp IsIn    = P.IsIn
deBinOp Defines = P.Eq

-- | Convert 'DisplayAssocBinOp's into the operators of the AST language.
deAssocBinOp :: DisplayAssocBinOp -> P.Ops
deAssocBinOp Equivalence = P.Eq 
deAssocBinOp _           = P.And

-- | Translate DisplayExprs to printable layout AST.
dispExpr :: DisplayExpr -> PrintingInformation -> P.Expr
dispExpr (AlgebraicExpr e)  sm = expr e sm
dispExpr (SpaceExpr s)      sm = space sm s
dispExpr (BinOp b l r)      sm = P.Row [dispExpr l sm, P.MO $ deBinOp b, dispExpr r sm]
dispExpr (AssocBinOp b des) sm = P.Row $ intersperse (P.MO op) $ map (dispExpr' sm prec) des
  where prec = dePrecAssoc  b
        op   = deAssocBinOp b
dispExpr (ForAll c s de)    sm = P.Row [P.MO P.ForAll, symbol $ lookupC (sm ^. stg) (sm ^. ckdb) c, P.MO P.IsIn, space sm s, P.MO P.Dot, dispExpr de sm ]