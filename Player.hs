module Player where

import Othello
import System.Random

checkForValidMoves :: Board -> Color -> IO Bool
checkForValidMoves board color = do
    if hasValidMoves board color then return True else (do
        putStrLn $ show color ++ " has no valid moves."
        return False)

humanPlayer :: Player
humanPlayer board color = do
    hasValid <- checkForValidMoves board color
    if hasValid then (do
        putStr "Enter a row: "
        r <- getLine
        putStr "Enter a col: "
        c <- getLine
        let move = ((read r, read c), color)
        case doMove board move of
            Nothing -> do
                putStrLn "\nNot a valid move. Try again."
                humanPlayer board color
            Just nextBoard -> return $ Just move) else return Nothing 

randomPlayer :: Player
randomPlayer board color = do
    hasValid <- checkForValidMoves board color
    if hasValid then (do
        gen <- newStdGen
        let moves = getValidMoves board color
        let (choice, _) = randomR (0, length moves - 1) gen
        let move = moves !! choice
        return $ Just move) else return Nothing 