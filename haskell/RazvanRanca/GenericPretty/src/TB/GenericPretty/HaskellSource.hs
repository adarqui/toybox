{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module TB.GenericPretty.HaskellSource (
) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Language.Haskell.Exts.Annotated.Fixity as Fix
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser           hiding (parseExp,
                                                         parsePat, parseType)
import qualified Language.Haskell.Exts.Pretty           as Pr
import qualified Language.Haskell.Exts.Syntax           as Hs
import qualified Language.Haskell.Exts.Syntax           as Hs
import           Language.Haskell.Meta
import           Language.Haskell.Meta.Syntax.Translate
import           System.IO
import           Text.PrettyPrint.GenericPretty

import           TB.Language.HaskellSrcMeta.Examples    (parseModule')

instance Out Hs.Module
instance Out Hs.ModuleName
instance Out Hs.ExportSpec
instance Out Hs.Namespace
instance Out Hs.CName
instance Out Hs.Name
instance Out Hs.QName
instance Out Hs.SpecialCon
instance Out Hs.Boxed
instance Out Hs.ImportDecl
instance Out Hs.ImportSpec
instance Out Hs.SrcLoc
instance Out Hs.WarningText
instance Out Hs.Decl
instance Out Hs.Activation
instance Out Hs.Annotation
instance Out Hs.Exp
instance Out Hs.Alt
instance Out Hs.Rhs
instance Out Hs.GuardedRhs
instance Out Hs.Stmt
instance Out Hs.Binds
instance Out Hs.IPBind
instance Out Hs.IPName
instance Out Hs.Pat
instance Out Hs.Sign
instance Out Hs.Literal
instance Out Hs.PXAttr
instance Out Hs.XName
instance Out Hs.PatField
instance Out Hs.RPat
instance Out Hs.RPatOp
instance Out Hs.Asst
instance Out Hs.BangType
instance Out Hs.Kind
instance Out Hs.Promoted
instance Out Hs.TyVarBind
instance Out Hs.Splice
instance Out Hs.Bracket
instance Out Hs.FieldUpdate
instance Out Hs.QOp
instance Out Hs.QualStmt
instance Out Hs.XAttr
instance Out Hs.Assoc
instance Out Hs.BooleanFormula
instance Out Hs.CallConv
instance Out Hs.ClassDecl
instance Out Hs.DataOrNew
instance Out Hs.FunDep
instance Out Hs.GadtDecl
instance Out Hs.InstDecl
instance Out Hs.QualConDecl
instance Out Hs.ConDecl
instance Out Hs.Match
instance Out Hs.Op
instance Out Hs.Overlap
instance Out Hs.Rule
instance Out Hs.RuleVar
instance Out Hs.Safety
instance Out Hs.TypeEqn
instance Out Hs.ModulePragma
instance Out Hs.Tool

deriving instance Generic Hs.Type
instance Out Hs.Type

parseIt path = putStrLn . pretty =<< parseModule' path
parseIt' path = putStrLn . Pr.prettyPrint . fromJust =<< parseModule' path

-- | Using their pretty printer
--
-- >>> Pr.prettyPrint $ (Var (UnQual (Ident "e")))
-- "e"

-- | Using GenericPretty
--
-- >>> pretty $ (Var (UnQual (Ident "e")))
-- "Var (UnQual (Ident \"e\"))"
