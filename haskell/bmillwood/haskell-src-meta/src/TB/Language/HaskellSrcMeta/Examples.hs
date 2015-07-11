module TB.Language.HaskellSrcMeta.Examples (
  printModule,
  parseModule',
  parseClasses,
  parseInstances
) where

import           Control.Monad
import           Language.Haskell.Exts.Annotated.Fixity as Fix
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser           hiding (parseExp,
                                                         parsePat, parseType)
import           Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.Syntax           as Hs
import qualified Language.Haskell.Exts.Syntax           as Hs
import           Language.Haskell.Meta
import           Language.Haskell.Meta.Syntax.Translate
--import           Language.Haskell.TH.Syntax
import           System.IO

-- Module in haskell-src-exts/src/Language/Haskell/Exts/Annotated/Syntax.hs

myDefaultParseMode' :: ParseMode
myDefaultParseMode' = ParseMode
  {parseFilename = []
  ,baseLanguage = Haskell2010
  ,extensions = map EnableExtension myDefaultExtensions'
  ,ignoreLinePragmas = False
  ,ignoreLanguagePragmas = False
  ,fixities = Nothing}

myDefaultExtensions' :: [KnownExtension]
myDefaultExtensions' = [PostfixOperators
                      ,QuasiQuotes
                      ,UnicodeSyntax
                      ,PatternSignatures
                      ,MagicHash
                      ,ForeignFunctionInterface
                      ,TemplateHaskell
                      ,RankNTypes
                      ,MultiParamTypeClasses
                      ,RecursiveDo
                      ,MultiParamTypeClasses
                      ,FlexibleInstances
                      ,FlexibleContexts
                      ,UndecidableInstances
                      ,TypeOperators
                      ]

parseHsModule' :: String -> Either String Hs.Module
parseHsModule' = parseResultToEither . parseModuleWithMode myDefaultParseMode'

printModule :: String -> IO ()
printModule path = do
  r <- liftM parseHsModule' (readFile path)
  case r of
    Left e -> print e
    Right code -> putStrLn $ pprHsModule code

parseModule' :: String -> IO (Maybe Hs.Module)
parseModule' path = do
  r <- liftM parseHsModule' (readFile path)
  case r of
    Left e -> return $ Nothing
    Right code -> return $ Just code

isType :: Hs.Decl -> Bool
isType (Hs.TypeDecl _ _ _ _) = True
isType _ = False

isTypeFam :: Hs.Decl -> Bool
isTypeFam (Hs.TypeDecl _ _ _ _) = True
isTypeFam _ = False

isData :: Hs.Decl -> Bool
isData (Hs.DataDecl _ _ _ _ _ _ _) = True
isData _ = False

isClass :: Hs.Decl -> Bool
isClass (Hs.ClassDecl _ _ _ _ _ _) = True
isClass _ = False

isInstance :: Hs.Decl -> Bool
isInstance (Hs.InstDecl _ _ _ _ _ _ _) = True
isInstance _ = False

isTypeSig :: Hs.Decl -> Bool
isTypeSig (Hs.TypeSig _ _ _) = True
isTypeSig _ = False

isFunBind :: Hs.Decl -> Bool
isFunBind (Hs.FunBind _) = True
isFunBind _ = False

parseTypes = filter isType . moduleDecls
parseTypeFams = filter isTypeFam . moduleDecls
parseData = filter isData . moduleDecls

parseClasses :: Hs.Module -> [Hs.Decl]
parseClasses = filter isClass . moduleDecls

parseInstances :: Hs.Module -> [Hs.Decl]
parseInstances = filter isInstance . moduleDecls
