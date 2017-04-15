module Main where

import Control.Monad
import Data.List
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random

import Data.Elocrypt

version    :: String
termLen    :: Int
termHeight :: Int

version    = "elocrypt 0.6.0"
termLen    = 80
termHeight = 10

data Options = Options {
      optLength  :: Int,        -- Size of the password(s)
      optNumber  :: Maybe Int,  -- Number of passwords to generate
      optHelp    :: Bool,
      optVersion :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
      optLength  = 8,
      optNumber  = Nothing,
      optHelp    = False,
      optVersion = False
  }

options :: [OptDescr (Options -> Options)]
options = [
      Option ['n'] ["number"]
        (ReqArg (\n o -> o {optNumber = Just (read n)}) "NUMBER")
        "The number of passwords to generate",

      Option ['p'] ["passphrase"]
        (NoArg id)
        "Generate passphrases instead of passwords",
           
      Option ['h'] ["help"]
        (NoArg (\o -> o { optHelp = True }))
        "Show this help",

      Option ['v'] ["version"]
        (NoArg (\o -> o { optVersion = True }))
        "Show version and exit"
  ]

main :: IO ()
main = do
  args <- getArgs
  opts <- elocryptOpts args
  gen  <- getStdGen

  when (optHelp opts) $ do
    hPutStrLn stderr usage
    exitSuccess

  when (optVersion opts) $ do
    hPutStrLn stderr version
    exitSuccess

  when (optLength opts == 0)
    exitSuccess    -- Nothing to do

  putStrLn (passwords opts gen)

elocryptOpts :: [String] -> IO Options
elocryptOpts args = do
  (opts, nonopts) <- elocryptOpts' args

  return $ if null nonopts
     then opts
     else opts { optLength = read (head nonopts) }

elocryptOpts' :: [String] -> IO (Options, [String])
elocryptOpts' args = case getOpt Permute options args of
  (opts, nonopts, []) -> do
    let opts' = foldl (flip id) defaultOptions opts
    return (opts', nonopts)

  (_   , _      , errs) -> do
    -- TODO: Refactor me
    hPutStrLn stderr (concat errs)
    hPutStrLn stderr usage
    exitFailure

passwords :: RandomGen g => Options -> g -> String
passwords opts gen = format . group' cols $ ps
  where ps = newPasswords (optLength opts) num False gen    -- TODO[sgillespie]: Add caps
        cols = columns (optLength opts)
        num = fromMaybe (nWords cols)
                        (optNumber opts)

passphrases :: RandomGen g => Options -> g -> String
passphrases = undefined

group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' i ls = g : group' i ls'
  where (g, ls') = splitAt i ls

usage :: String
usage = usageInfo (intercalate "\n" headerLines) options
  where headerLines =
          ["Usage: elocrypt [option...] length",
           "       elocrypt -p [option...] min-length max-length",
           ""]

-- Utilities

-- Calculate the number of passwords to print per line
columns :: Int -> Int
columns len | len <= termLen - 2 = termLen `div` (len + 2)
            | otherwise = 1

-- Format a 2D list of Strings,
--  1 list per line
format :: [[String]] -> String
format = intercalate "\n" . map unwords

-- Calculate the number of words to print
nWords :: Int -> Int
nWords cols = cols * termHeight
