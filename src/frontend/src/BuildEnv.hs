{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BuildEnv (getCompileEnvExp)

where

-- Código copiado de https://stackoverflow.com/questions/19679024/how-to-properly-communicate-compile-time-information-to-template-haskell-functio e alterado


import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Environment (getEnvironment)

-- Functions that work with compile-time configuration

-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key `liftM` runIO getEnvironment

-- | Looks up an compile-time environment variable and fail, if it's not
-- present.
getCompileEnv :: String -> Q String
getCompileEnv key =
  lookupCompileEnv key >>=
  maybe (fail $ "Environment variable " ++ key ++ " not defined") return

-- | Looks up an compile-time environment variable and fail, if it's not
-- present. The result is a TH expression of type @String@.
getCompileEnvExp :: String -> Q Exp
getCompileEnvExp = lift <=< getCompileEnv