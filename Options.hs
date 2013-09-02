{-# LANGUAGE FlexibleInstances, OverloadedStrings, MultiParamTypeClasses,
             TupleSections, TypeSynonymInstances #-}
-- |A wrapper for accessing to System.Console.GetOpt.
module Options
(
module System.Console.GetOpt,
-- *Accessing options
-- |The idea is to create a 'Monad' in which the do notation can be used to enter
-- the different options and create the list of 'OptDescr' that is passed to 'getOpt'.
-- To illustrate this usage, let's rewrite an example from the documentation of
-- 'System.Console.GetOpt':
-- The original list is constructed as follows:
--
-- >    options :: [OptDescr Flag]
-- >    options =
-- >     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
-- >     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
-- >     , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
-- >     , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
-- >     , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
-- >     ]
--
-- The same result can be obtained by:
--
-- >    options :: [OptDescr Flag]
-- >    options = processOptions $ do
-- >                'v' ~: "verbose"         ==> NoArg Verbose ~: "chatty output on stderr"
-- >                ['V','?'] ~: ["version"] ==> NoArg Version ~: "show version number"
-- >                'o' ~: "output"          ==> OptArg outp "FILE"  ~: "output FILE"
-- >                'c'                      ==> OptArg inp  "FILE"  ~: "input FILE"
-- >                'L' ~: "libdir"          ==> ReqArg LibDir "DIR" ~: "library directory"

-- *The functions and operators
optionsFromHandle, processOptions, (==>), (~:)
)
where

import Control.Applicative((<$>))
import Control.Arrow((***))
import Control.Monad(forM)
import Data.Either(partitionEithers)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), getOpt, usageInfo, OptDescr (..))
import System.IO(Handle)

data OptionProcessor a b = OptionProcessor { process :: [OptDescr a] -> ([OptDescr a], b)}

instance Monad (OptionProcessor a) where
  p >>= f = OptionProcessor $ \ops -> let
                                        (ops', a) = process p ops
                                      in process (f a) ops'

  return v = OptionProcessor $ \ops -> (ops, v)


addOption :: OptDescr a -> OptionProcessor a ()
addOption op = OptionProcessor $ \ops -> (ops++ [op], ())

class Flags t where
    makeFlags :: t -> ([Char], [String])

instance Flags Char where
    makeFlags c = ([c], [])

instance Flags String where
    makeFlags s = ([], [s])

instance Flags (Char, String) where
    makeFlags (c, s) = ([c], [s])

instance Flags ([Char], [String]) where
    makeFlags = id

class Description t a where
    makeDescription :: t -> (ArgDescr a, String)

instance Description (ArgDescr a) a where
    makeDescription arg = (arg, "")

instance Description (ArgDescr a, String) a where
    makeDescription = id

infix 2 ==>
infix 3 ~:

-- |Takes a left part that can be:
--
-- * A character for options that only have a short version.
--
-- * A string for options that only have a long option.
--
-- * A pair '(char, String)' for options with a short and a long
--   version. The pair can be easily constructed using '~:'.
--
-- * A pair '([Char], [String])' for the general case.
--
-- The right part can be:
--
-- * An 'ArgDescr a' for options without documentation.
--
-- * A pair '(ArgDescr a, String)' for options with documentation.
--   The pair can be easily constructed using '~:'.
(==>) :: (Flags f, Description d a) => f -> d -> OptionProcessor a ()
f ==> d = let
            (cs, ss) = makeFlags f
            (arg, help) = makeDescription d
          in addOption $ Option cs ss arg help

-- |A synonym for '(,)' useful for writing tuples without the parenthesis.
(~:) = (,)

-- |Build a list of 'OptDescr' apt for 'GetOpt'. The input can be written in
-- do notation using the function 'addOption' or the operators '==>' and '~:'.
processOptions :: OptionProcessor a () -> [ OptDescr a ]
processOptions p = fst $ process p []

-- |Given a list of 'OptDescr', use it for processing the contents of
-- a text file. Each line of the file can begin with one of the long
-- options. If it has any argument, it is written after a colon. Empty
-- lines and those beginning with a # are ignored. It returns the
-- lists of the values that matched and the lines that didn't.
optionsFromHandle :: [ OptDescr a ] -> Handle -> IO ([a], [String])
optionsFromHandle opts h = do
                             ls <- (map T.strip . T.lines) <$> TIO.hGetContents h
                             return . partitionEithers $ do
                               l <- clean ls
                               return $ maybe (Right $ T.unpack l) Left $ processLine descs l
                           where descs = M.fromList $ do
                                             Option _ ls desc _ <- opts
                                             map ((, desc).T.pack) ls

clean :: [Text] -> [Text]
clean = filter (\t -> not (T.null t) && T.head t /= '#')

splitArg :: Text -> (Text, String)
splitArg = (T.strip *** \a -> if T.null a then "" else T.unpack . T.strip . T.tail $ a) . T.breakOn ":"

processLine :: Map Text (ArgDescr a) -> Text -> Maybe a
processLine descs l = do
                        let (opt, arg) = splitArg l
                        desc <- M.lookup opt descs
                        if null arg
                        then case desc of
                                NoArg x -> return x
                                ReqArg _ _ -> Nothing
                                OptArg f _ -> return $ f Nothing
                        else case desc of
                                NoArg _ -> Nothing
                                ReqArg f _ -> return $ f arg
                                OptArg f _ -> return . f $ Just arg
