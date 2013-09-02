import Control.Exception(ioError, IOException(..))
import Data.Maybe(fromMaybe)
import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), getOpt, usageInfo, OptDescr (..))
import System.Environment(getArgs)

import Options


data Flag
     = Verbose  | Version
     | Input String | Output String | LibDir String
       deriving Show

options :: [OptDescr Flag]
options = processOptions $ do
                 'v' ~: "verbose"         ==> NoArg Verbose ~: "chatty output on stderr"
                 ['V','?'] ~: ["version"] ==> NoArg Version ~: "show version number"
                 'o' ~: "output"          ==> OptArg outp "FILE"  ~: "output FILE"
                 'c'                      ==> OptArg inp  "FILE"  ~: "input FILE"
                 'L' ~: "libdir"          ==> ReqArg LibDir "DIR" ~: "library directory"

inp,outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"

main = do
         args <- getArgs
         case getOpt Permute options args of
            (o, n, []) -> do
                            putStrLn $ "Options: " ++ show o
                            putStrLn $ "Arguments: " ++ show n
            (_, _, e) -> ioError (userError $ concat e)
