module Main where

import Othello
import Player
import System.Exit
import Text.Read

-- Default starting game state.
startState :: GameState 
startState = Game startBoard humanPlayer humanPlayer humanPlayer Black 0 0

-- Start menu. If the user chooses a human vs. AI game, human player always plays Black.
setup :: IO GameState
setup = do
    putStrLn "What type of game would you like to play? (or 9 to quit)\n"
    putStrLn "\t 1. Human vs. Human"
    putStrLn "\t 2. Human vs. Random AI"
    putStrLn "\t 3. Human vs. Minimax AI"
    putStrLn "\t 4. AI vs. AI\n"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line of
        Just 1 -> return startState
        Just 2 -> return startState { whitePlayer = randomPlayer, blackPlayer = humanPlayer }
        Just 3 -> undefined
        Just 4 -> return startState { whitePlayer = randomPlayer, 
                                      blackPlayer = randomPlayer, 
                                      currentPlayer = randomPlayer }
        Just 9 -> exitSuccess
        _ -> do
            putStrLn "\nInvalid choice.\n"
            setup

-- Print a board to the console.
printBoard :: Board -> IO ()
printBoard board = do
    let str = showBoard board
    putStrLn str

-- Game loop.
play :: GameState -> IO ()
play GameOver{winner = w, board = board} = do
    let msg = maybe "Tie." showColor w
    putStrLn ""
    printBoard board
    putStrLn $ "\nGame over. Winner: " ++ msg
play game@Game{board = board, currentTurn = turn, currentPlayer = player} = do
        putStrLn ""
        printBoard board
        putStrLn $ "Turn: " ++ showColor turn
        nextMove <- currentPlayer game board turn
        play $ nextState game nextMove

-- Program entry point.
main :: IO ()
main = do
        putStrLn "\nWelcome to Othello\n"
        state <- setup
        play state