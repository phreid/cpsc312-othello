module Player where

import Othello
import System.Random
import Text.Read
import Data.Maybe

-- A human player. Asks the user for input from the console.
humanPlayer :: Player
humanPlayer board color = do
    if hasValidMoves board color then (do
        putStr "Enter a row: "
        r <- getLine
        putStr "Enter a col: "
        c <- getLine
        case (readMaybe r, readMaybe c) of 
            (Nothing, _) -> do
                putStrLn "\nInvalid input."
                humanPlayer board color
            (_, Nothing) -> do
                putStrLn "\nInvalid input."
                humanPlayer board color
            (Just row, Just col) -> do
                let move = ((row, col), color)
                case doMove board move of
                    Nothing -> do
                        putStrLn "\nNot a valid move. Try again."
                        humanPlayer board color
                    Just nextBoard -> return $ Just move) 
    else return Nothing
            
-- A random player. Randomly chooses from the list of available moves.
randomPlayer :: Player
randomPlayer board color = do
    if hasValidMoves board color then (do
        gen <- newStdGen
        let moves = getValidMoves board color
        let (choice, _) = randomR (0, length moves - 1) gen
        let move = moves !! choice
        return $ Just move) 
    else return Nothing 