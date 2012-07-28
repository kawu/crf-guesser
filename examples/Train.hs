import System.Environment (getArgs)
import Data.Binary (encodeFile)

import Data.Guesser
import SGD

main = do
    [tagset, train, test, output] <- getArgs
    let sgdArgs = sgdArgsDefault { iterNum = 10 }
    guesser <- learn sgdArgs tagset train test
    encodeFile output guesser
