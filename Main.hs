module Main where

import Othello
import Player

startState = Game startBoard humanPlayer randomPlayer randomPlayer Black 0 0

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
main = play startState