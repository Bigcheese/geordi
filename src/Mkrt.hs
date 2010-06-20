{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}

import System.Posix (createFile, createDirectory, closeFd,
  FileMode, unionFileModes, accessModes, nullFileMode,
  ownerReadMode, ownerWriteMode, ownerExecuteMode,
  groupReadMode, groupWriteMode, groupExecuteMode, 
  otherReadMode, otherWriteMode, otherExecuteMode,
  setFileCreationMask)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnv)
import Control.Monad (when, forM)
import Text.Regex (matchRegex, mkRegex)
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Util (findM, (.))
import Prelude hiding ((.))
import Prelude.Unicode
import Paths_geordi (getDataFileName)
import CompileConfig

split_paths :: String → [FilePath]
split_paths [] = []
split_paths (span (/= ':') → (f, r)) = f : split_paths (drop 1 r)

which :: String → IO (Maybe FilePath)
which s = getEnv "PATH" >>= findM doesFileExist . (s:) . map (</> s) . filter (not . null) . split_paths

which_or_error :: String → IO FilePath
which_or_error s = maybe (error $ "Could not find " ++ s ++ " in PATH.") id . which s

modes :: [FileMode] → FileMode
modes = foldl1 unionFileModes

readModes, writeModes, executeModes :: FileMode
readModes = modes [ownerReadMode, groupReadMode, otherReadMode]
writeModes = modes [ownerWriteMode, groupWriteMode, otherWriteMode]
executeModes = modes [ownerExecuteMode, groupExecuteMode, otherExecuteMode]

ldd :: FilePath → IO [FilePath]
ldd f = do
  (status, out, err) ← readProcessWithExitCode "ldd" [f] ""
  if status ≠ ExitSuccess then error err else do
  return $ map head $ mapMaybe (matchRegex $ mkRegex "[[:blank:]](/[^[:blank:]]*)") $ lines out

copyFileWithParents :: FilePath → FilePath → IO ()
copyFileWithParents f to = do
  createDirectoryIfMissing True $ takeDirectory to
  copyFile f to

main :: IO ()
main = do
  setFileCreationMask $ modes [groupWriteMode, otherWriteMode]
  rt ← getDataFileName "rt"
  putStr $ "Setting up " ++ rt ++ " ..."
  hFlush stdout
  clang ← which_or_error "clang"
  lli ← which_or_error "lli"
  copyFileWithParents clang $ rt </> "usr/bin/clang"
  copyFileWithParents lli $ rt </> "usr/bin/lli"
  (ldd clang >>=) $ mapM_ $ \f → copyFileWithParents f (rt ++ "/" ++ f) -- can't use </> here because f is absolute
  setFileCreationMask nullFileMode
  createFile (rt </> "lock") readModes >>= closeFd
  forM ["t.cpp", "t.bc"] $ (>>= closeFd) . flip createFile (unionFileModes writeModes readModes) . (rt </>)
  putStrLn " done."
