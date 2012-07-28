import System.Environment (getArgs)
import Data.Binary (decodeFile)

import Data.Guesser
import SGD

main = do
    [guesserPath, tagset, inPath] <- getArgs
    guesser <- decodeFile guesserPath
    tagFile 10 guesser tagset inPath
