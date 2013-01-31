{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
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
processOptions, (==>), (~:)
)
where

import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), getOpt, usageInfo, OptDescr (..))

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
