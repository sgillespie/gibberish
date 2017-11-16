module Main where

import Control.Monad
import Data.Char
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

version    = "elocrypt 1.0.0"
termLen    = 80
termHeight = 10

data Options = Options {
      optCapitals  :: Bool,       -- Include capital letters?
      optLength    :: Int,        -- Size of the password(s)
      optMaxLength :: Int,
      optNumber    :: Maybe Int,  -- Number of passwords to generate
      optPassType  :: PassType,   -- Generate passwords or passphrases
      optHelp      :: Bool,
      optVersion   :: Bool
  } deriving (Show)

data PassType
  = Phrase
  | Word
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
      optCapitals  = False,
      optLength    = 8,
      optMaxLength = 10,
      optNumber    = Nothing,
      optPassType  = Word,
      optHelp      = False,
      optVersion   = False
  }

options :: [OptDescr (Options -> Options)]
options = [
      Option ['c'] ["capitals"] 
        (NoArg (\o -> o { optCapitals = True }))
        "Include capital letters",

      Option ['n'] ["number"]
        (ReqArg (\n o -> o { optNumber = Just (read n) }) "NUMBER")
        "The number of passwords to generate",

      Option ['p'] ["passphrase"]
        (NoArg (\o -> o { optPassType = Phrase }))
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

  putStrLn (generate opts gen)

elocryptOpts :: [String] -> IO Options
elocryptOpts args = do
  (opts, nonopts) <- elocryptOpts' args

  return $ case nonopts of
    (o:os:_) -> opts { optLength = read o, optMaxLength = read os }
    (o:_)    -> opts { optLength = read o }
    []       -> opts

elocryptOpts' :: [String] -> IO (Options, [String])
elocryptOpts' args = case getOpt Permute options args of
  (opts, nonopts, []) -> do
    let opts' = foldl (flip id) defaultOptions opts
    return (opts', nonopts)

  (_   , _      , errs) -> do
    hPutStrLn stderr (concat errs)
    hPutStrLn stderr usage
    exitFailure

generate :: RandomGen g => Options -> g -> String
generate opts@Options{optPassType=Word}   = passwords opts
generate opts@Options{optPassType=Phrase} = passphrases opts

passwords :: RandomGen g => Options -> g -> String
passwords Options{optCapitals = caps, optLength = len, optNumber = n} gen
  = format "  " . groupWith splitAt' width "  " $ ps
  where ps = newPasswords len num caps gen
        cols = columns len
        num = fromMaybe (nWords cols) n
        width = max termLen (len + 2)

passphrases :: RandomGen g => Options -> g -> String
passphrases Options{optCapitals = caps,
                    optLength = minLen,
                    optMaxLength = maxLen,
                    optNumber = n} gen
  = format " " . take lines' . map (map capitalize) . groupWith splitAt' width " " $ passphrase
  where passphrase = newPassphrase words' minLen maxLen gen
        words' = columns minLen * lines'
        width = max termLen (maxLen + 1)
        lines' = fromMaybe termHeight n
        capitalize word@(c:cs) | caps = toUpper c : cs
                               | otherwise = word

usage :: String
usage = usageInfo (intercalate "\n" headerLines) options
  where headerLines = [
              "Usage: elocrypt [option...] length",
              "       elocrypt -p [option...] min-length max-length"
          ]

-- Utilities

-- Calculate the number of passwords to print per line
columns :: Int -> Int
columns len | len <= termLen - 2 = termLen `div` (len + 2)
            | otherwise = 1

-- Format a 2D list of Strings,
--  1 list per line
format :: String -> [[String]] -> String
format sep = intercalate "\n" . map (intercalate sep)

-- Calculate the number of words to print
nWords :: Int -> Int
nWords cols = termHeight * cols

-- Group a 2D array with a function by total length
groupWith
  :: (Int -> [a] -> [[a]] -> ([[a]], [[a]]))
  -> Int
  -> [a]
  -> [[a]]
  -> [[[a]]]
groupWith _ _ _ [] = []
groupWith f i sep ls | null groups = [[head ls]]
                     | otherwise = groups
  where groups = groupWith' f i sep ls

groupWith'
  :: (Int -> [a] -> [[a]] -> ([[a]], [[a]]))
  -> Int
  -> [a]
  -> [[a]]
  -> [[[a]]]
groupWith' _ _ _ [] = []
groupWith' f i sep ls = g : groupWith f i sep ls'
  where (g, ls') = f i sep ls

-- Split a 2D array by the total length
splitAt' :: Int -> [a] -> [[a]] -> ([[a]], [[a]])
splitAt' 0 _ ls = ([], ls)
splitAt' _ _ [] = ([], [])
splitAt' n sep (l:ls) | n >= length l + sl = (l:xs, xs')
                      | otherwise = ([], l:ls)
  where (xs, xs') = splitAt' (n - length l - sl) sep ls
        sl = length sep
