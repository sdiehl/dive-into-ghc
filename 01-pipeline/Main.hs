{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import GHC
import GHC.Paths (libdir)

import DynFlags

import Data.List
import Data.Monoid
import Data.Dynamic
import Unsafe.Coerce

import Control.Exception
import Control.Monad.Trans

import System.Console.Haskeline

type Repl a = InputT IO a

initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  liftIO $ putStrLn "Setting up HscEnv"
  dflags <- getSessionDynFlags
  let dflags' = dflags { hscTarget = HscInterpreted , ghcLink = LinkInMemory }
                `xopt_set` Opt_ExtendedDefaultRules
  setSessionDynFlags dflags'
  setContext [ IIDecl $ simpleImportDecl (mkModuleName "Prelude") ]
  env <- getSession
  return env

addImport :: String -> Ghc ()
addImport mod = do
  ctx <- getContext
  setContext ( (IIDecl $ simpleImportDecl (mkModuleName mod)) : ctx )

session :: HscEnv -> Ghc a -> IO HscEnv
session env m = runGhc (Just libdir) $ do
  setSession env 
  m
  env <- getSession
  return env

eval :: String -> Ghc ()
eval inp = do
  dyn <- fromDynamic <$> dynCompileExpr inp
  case dyn of
    Nothing -> do
      act <- compileExpr ("Prelude.print (" <> inp <> ")")
      -- 'print' is constrained to 'IO ()' so unsafeCoerce is "safe"
      liftIO (unsafeCoerce act)
    Just act -> liftIO $ act

ghcCatch :: MonadIO m => IO a -> m (Maybe a)
ghcCatch m = liftIO $ do
  mres <- try m
  case mres of
    Left (err :: SomeException) -> do
      liftIO $ print err
      return Nothing
    Right res -> return (Just res)

repl :: HscEnv -> Repl ()
repl env = do
  minput <- getInputLine ">>> "
  case minput of
    Nothing -> outputStrLn "Goodbye."

    Just input | "import" `isPrefixOf` input -> do
      let mod = concat $ tail $ words input
      env' <- ghcCatch (session env (addImport mod))
      maybe (repl env) repl env'

    Just input -> do
      env' <- ghcCatch (session env (eval input))
      maybe (repl env) repl env'

main :: IO ()
main = do
  env <- initSession
  runInputT defaultSettings (repl env)
