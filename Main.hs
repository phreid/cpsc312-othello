module Main where

import Othello
import Player
import System.Exit
import Text.Read

startState = Game startBoard humanPlayer randomPlayer randomPlayer Black 0 0
setup :: IO GameState 
setup = do
    putStrLn "What type of game would you like to play? (or 9 to quit)\n"
    putStrLn "\t 1. Human vs. Human"
    putStrLn "\t 2. Human vs. Random AI"
    putStrLn "\t 3. Human vs. Minimax AI"
    putStrLn "\t 4. AI vs. AI"
    putStr "Your choice: "
    line <- getLine 
    case readMaybe line of
        Just 1 -> return $ Game startBoard humanPlayer humanPlayer humanPlayer Black 0 0
        Just 2 -> return $ Game startBoard randomPlayer humanPlayer humanPlayer Black 0 0
        Just 3 -> undefined 
        Just 4 -> undefined
        Just 9 -> exitSuccess
        _ -> do
            putStrLn "\nInvalid choice.\n"
            setup


printBoard :: Board -> IO ()
printBoard board = do
    let str = showBoard board
    putStrLn str

play :: GameState -> IO ()
play GameOver{winner = w, board = bd} = do
    let msg = maybe "Tie." show w
    putStrLn ""
    printBoard bd
    putStrLn $ "\nGame over. Winner: " ++ msg
play game = do
        putStrLn ""
        printBoard $ board game
        putStrLn $ "Turn: " ++ show (currentTurn game)
        nextMove <- currentPlayer game (board game) (currentTurn game)
        play $ nextState game nextMove

main :: IO ()
main = do
        putStrLn "Welcome to Othello\n"
        state <- setup
        play state