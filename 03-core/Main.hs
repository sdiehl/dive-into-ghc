module Main where

-- Compiler
import GHC
import DynFlags
import HscMain
import HscTypes
import Outputable
import GHC.Paths ( libdir )

-- Core Types
import Var
import Name
import Kind
import Avail
import IdInfo
import Module
import TypeRep
import Unique
import OccName
import InstEnv
import NameSet
import RdrName
import FamInstEnv
import qualified Stream
import qualified CoreSyn as Syn

-- Core Passes
import CorePrep (corePrepPgm)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import StgCmm (codeGen)
import CmmInfo (cmmToRawCmm )
import CmmLint (cmmLint)
import CmmPipeline (cmmPipeline)
import CmmBuildInfoTables (emptySRT, srtToData)
import AsmCodeGen ( nativeCodeGen )
import UniqSupply ( mkSplitUniqSupply, initUs_ )

import System.IO
import Data.Time
import Control.Monad.Trans

-------------------------------------------------------------------------------
-- Module
-------------------------------------------------------------------------------

mkName :: Int -> String -> Name
mkName i n = mkInternalName (mkUnique 'n' i) (mkOccName OccName.varName n) noSrcSpan

xn :: Name
xn = mkName 0 "x"

an :: Name
an = mkName 1 "a"

fn :: Name
fn = mkExternalName (mkUnique 'n' 2) modl (mkOccName OccName.varName "f") noSrcSpan

-- a :: *
a :: TyVar
a = mkTyVar an anyKind

-- x :: a
x :: Var
x = mkLocalVar VanillaId xn (TyVarTy a) vanillaIdInfo

-- f :: a -> a
fv :: Var
fv = mkGlobalVar VanillaId fn (FunTy (TyVarTy a) (TyVarTy a)) vanillaIdInfo

def :: [Syn.CoreBind]
def = [Syn.NonRec fv f]

f :: Syn.Expr Var
{-f = Syn.Lam x $ Syn.Lam x (Syn.Var x)-}
f = Syn.Lam x (Syn.Var x)

modl :: Module
modl = mkModule mainPackageKey (mkModuleName "Example")

guts :: ModGuts
guts = ModGuts
  {
      mg_module          = modl,
      mg_exports         = [Avail fn],
      mg_deps            = noDependencies,
      mg_dir_imps        = emptyModuleEnv,
      mg_used_names      = mkNameSet [fn],
      mg_used_th         = False,
      mg_rdr_env         = emptyGlobalRdrEnv,
      mg_fix_env         = emptyFixityEnv,
      mg_tcs             = [],
      mg_insts           = [],
      mg_fam_insts       = [],
      mg_patsyns         = [],
      mg_rules           = [],
      mg_binds           = def,
      mg_foreign         = NoStubs,
      mg_warns           = NoWarnings,
      mg_hpc_info        = NoHpcInfo False,
      mg_modBreaks       = emptyModBreaks,
      mg_vect_decls      = [],
      mg_vect_info       = noVectInfo,
      mg_boot            = False,
      mg_anns            = [],
      mg_inst_env        = emptyInstEnv,
      mg_fam_inst_env    = emptyFamInstEnv,
      mg_safe_haskell    = Sf_None,
      mg_trust_pkg       = False,
      mg_dependent_files = []
  }

summ :: DynFlags -> ModSummary
summ dflags = ModSummary 
  {
      ms_mod          = modl,
      ms_hsc_src      = HsSrcFile,
      ms_location     = ModLocation {
          ml_hs_file  = Nothing
      ,   ml_hi_file  = "Example.hi"
      ,   ml_obj_file = "Example.o"
      },
      ms_hs_date      = UTCTime (toEnum 0) 0,
      ms_obj_date     = Nothing,
      ms_iface_date   = Nothing,
      ms_srcimps      = [],
      ms_textual_imps = [],
      ms_hspp_file    = "Example.hs",
      ms_hspp_opts    = dflags,
      ms_hspp_buf     = Nothing
  }

modloc :: ModLocation
modloc = ModLocation 
 { ml_hs_file  = Nothing
 , ml_hi_file  = "Example.hi"
 , ml_obj_file = "Example.o"
 }

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

main :: IO ()
main = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags

  setSessionDynFlags $ dflags { hscTarget = HscAsm, ghcLink = LinkBinary }

  dflags <- getSessionDynFlags
  env <- getSession

  setTargets [Target 
    { targetId = TargetModule (mkModuleName "Example")
    , targetAllowObjCode = True
    , targetContents = Nothing }]

  -- Run the Core prep pass
  prep <- liftIO $ corePrepPgm env (ms_location (summ dflags)) (mg_binds guts) (mg_tcs guts)

  -- Transform Core into STG
  stg <- liftIO $ coreToStg dflags (mg_module guts) prep

  -- STG Transformer
  (stg_binds2, cost_centre_info) <- liftIO $ stg2stg dflags (mg_module guts) stg

  -- Generated Cmm
  let cmms = codeGen dflags (mg_module guts) (mg_tcs guts) cost_centre_info stg_binds2 (mg_hpc_info guts)

  -- Initialize a name supply for the Cmm pipeline
  us <- liftIO $ mkSplitUniqSupply 'S'
  let initTopSRT = initUs_ us emptySRT
      run_pipeline = cmmPipeline env

  -- Collect the Cmm code stream after running the pipeline.
  let cmmstream = do
       a <- Stream.mapAccumL run_pipeline initTopSRT cmms
       Stream.yield (srtToData a)

  -- Prepare the Cmm for 
  genraw <- liftIO $ cmmToRawCmm dflags cmmstream

  -- Initialize name supply for the native code generator and generate x86 to a
  -- file from the prepared Cmm.
  ncg_uniqs <- liftIO $ mkSplitUniqSupply 'n'
  fname <- liftIO $ (openFile "Example.asm" WriteMode)
  liftIO $ nativeCodeGen dflags (mg_module guts) modloc fname ncg_uniqs genraw

  -- Dump the outputted Stg and  Cmm out
  gen <- liftIO $ Stream.collect cmmstream
  liftIO $ putStrLn "=== STG ==="
  liftIO $ putStrLn $ showGhc stg_binds2

  liftIO $ putStrLn "=== CMM ==="
  liftIO $ putStrLn $ showGhc gen
