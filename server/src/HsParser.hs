module HsParser(parseFile, extractDecls) where

import qualified GHC
import GHC(Ghc, GhcMonad)
import GHC.Paths(libdir)

-- Various other random stuff that we need
import Config
import Constants
import HscTypes
import Packages         ( pprPackages, pprPackagesSimple, pprModuleMap )
import DriverPhases
import BasicTypes       ( failed )
import StaticFlags
import DynFlags hiding (getDynFlags)
import InteractiveEval
import Name (getOccString)
import Bag
import ErrUtils
import SrcLoc
import Outputable
import HsSyn
import HsDecls
import RdrName
import OccName
import FastString

-- Standard Haskell libraries
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Unsafe
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as S

import ErrorParser(ErrorMessage(..))

dynFlags :: DynFlags
dynFlags = unsafePerformIO $ getDynFlags

getDynFlags :: IO DynFlags
getDynFlags = GHC.runGhc (Just libdir) getDynFlags'

getDynFlags' :: Ghc DynFlags
getDynFlags' = do
  dflags0 <- GHC.getSessionDynFlags

  let dflt_target = hscTarget dflags0
      mode = CompManager
      lang = HscInterpreted
      link = LinkInMemory

  let dflags1 = let platform = targetPlatform dflags0
                    dflags0a = updateWays $ dflags0 { ways = interpWays }
                    dflags0b = foldl gopt_set dflags0a
                               $ concatMap (wayGeneralFlags platform) interpWays
                    dflags0c = foldl gopt_unset dflags0b
                               $ concatMap (wayUnsetGeneralFlags platform) interpWays
                    in dflags0c

      dflags2 = dflags1{ ghcMode   = mode,
                         hscTarget = lang,
                         ghcLink   = link,
                         verbosity = 5 }

  _ <- GHC.setSessionDynFlags dflags2
  GHC.getSessionDynFlags

formatError :: ErrMsg -> ErrorMessage
formatError err =
  let start = srcSpanStart (errMsgSpan err)
  in
   case start of
     RealSrcLoc real ->
       ErrorMessage (unpackFS $ srcLocFile real) (srcLocLine real) (srcLocCol real) (errMsg err)
     _ -> ErrorMessage "<unknown>" 1 1 (errMsg err)
  where
    errMsg :: ErrMsg -> String
    errMsg err = showSDoc dynFlags $ errMsgShortDoc err <> errMsgExtraInfo err

parseFile :: String -> FilePath -> Either [ErrorMessage] (HsModule GHC.RdrName)
parseFile code filePath = case GHC.parser code dynFlags filePath of
  Left errs -> Left $ map formatError $ bagToList errs
  Right (_, mod) -> Right $ unLoc mod

extractDecls :: String -> FilePath -> Either [ErrorMessage] (Set String)
extractDecls code filePath = do
  mod <- parseFile code filePath
  return $ S.fromList $ map fromJust $ filter isJust $ map extractdecl $ hsmodDecls mod
  where
    nameToString :: RdrName -> String
    nameToString = showSDoc dynFlags . pprOccName . occName

    extractdecl :: LHsDecl GHC.RdrName -> Maybe String
    extractdecl decl = case unLoc decl of
      ValD bind -> case bind of
        FunBind _ _ _ _ _ _ -> Just $ nameToString $ unLoc $ fun_id bind
        _ -> Nothing
      _ -> Nothing
