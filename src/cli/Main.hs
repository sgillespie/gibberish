module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random

import Elocrypt.Password

version = "elocrypt 0.1.0"

main :: IO ()
main = do
  args <- getArgs
  opts <- elocryptOpts args
  gen  <- getStdGen

  -- TODO: Refactor me
  if (optHelp opts) then do
    hPutStrLn stderr usage
    exitSuccess
  else return ()

  -- TODO: Refactor me
  if (optVersion opts) then do
    hPutStrLn stderr version
    exitSuccess
  else return ()

  putStrLn (password opts gen)

password :: RandomGen g => Options -> g -> String
password opts gen = fst . generate gen $ (optSize opts)

data Options
  = Options { optSize    :: Int,
              optHelp    :: Bool,
              optVersion :: Bool }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optSize = 8,
                           optHelp = False,
                           optVersion = False }

options :: [OptDescr (Options -> Options)]
options = [Option ['h'] ["help"] (NoArg (\o -> o { optHelp = True })) "Show this help",
           Option ['v'] ["version"] (NoArg (\o -> o { optVersion = True })) "Show version and exit"]

elocryptOpts :: [String] -> IO Options
elocryptOpts args = do
  (opts, nonopts) <- elocryptOpts' args
  return $ if (null nonopts)
     then opts
     else opts { optSize = read (head nonopts) }

elocryptOpts' :: [String] -> IO (Options, [String])
elocryptOpts' args = case getOpt Permute options args of
  (opts, nonopts, [])   -> return (foldl (flip id) defaultOptions opts, nonopts)
  (_   , _      , errs) -> do
    -- TODO: Refactor me
    hPutStrLn stderr (concat errs)
    hPutStrLn stderr usage
    exitFailure

usage :: String
usage = usageInfo header options
  where header = "Usage: elocrypt [option...] length"
