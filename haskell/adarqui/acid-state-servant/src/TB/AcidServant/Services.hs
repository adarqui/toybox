module TB.AcidServant.Services (
) where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid
import           Data.Acid.Remote

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy
import           Network
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Typeable

import qualified Data.Map as M
