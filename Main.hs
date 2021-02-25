module Main where

import Othello
import Player
import Tournament

import System.Exit
import Text.Read
import Data.Maybe

-- Default starting game state.
startState :: GameState 
startState = Game startBoard humanPlayer humanPlayer humanPlayer Black 0 0

-- Start menu. If the user chooses a human vs. AI game, human player always plays Black.
setup :: IO (Maybe GameState)
setup = do
    putStrLn "What type of game would you like to play? (or 9 to quit)\n"
    putStrLn "\t 1. Human vs. Human"
    putStrLn "\t 2. Human vs. Random AI"
    putStrLn "\t 3. Human vs. Heuristic AI"
    putStrLn "\t 4. Human vs. Heuristic AI with Lookahead"
    putStrLn "\t 5. Human vs. Minimax AI"
    putStrLn "\t 6. AI vs. AI Tournament\n"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line of
        Just 1 -> return $ Just startState
        Just 2 -> return $ Just startState { whitePlayer = randomPlayer, blackPlayer = humanPlayer }
        Just 3 -> return $ Just startState { whitePlayer = heuristicPlayer, blackPlayer = humanPlayer }
        Just 4 -> return $ Just startState { whitePlayer = lookaheadPlayer, blackPlayer = humanPlayer }
        Just 5 -> return $ Just startState { whitePlayer = lookaheadPlayer, 
                                      blackPlayer = minimaxPlayer, 
                                      currentPlayer = minimaxPlayer
                                       }
        Just 6 -> do
            setupTournament
            return Nothing
        Just 9 -> exitSuccess
        _ -> do
            putStrLn "\nInvalid choice.\n"
            setup

-- Prompt the user to set up an AI tournament, then play the tournament
setupTournament :: IO ()
setupTournament = do
    putStr "\nEnter the number of games to play (warning: 5+ games can be slow): "
    numGames <- getLine
    case readMaybe numGames of
        Just n -> do
            white <- chooseAI "White"
            let gameState = startState {whitePlayer = white, blackPlayer = randomPlayer, currentPlayer = randomPlayer}
            putStrLn "\nPlaying tournament..."
            playTournament $ Tournament gameState gameState n 0 0 0
        Nothing -> do
            putStrLn "\nInvalid input."
            setupTournament

chooseAI :: String -> IO Player
chooseAI color = do
    putStrLn $ "\nChoose an AI for: " ++ color
    putStrLn "\t 1. Random (fastest)"
    putStrLn "\t 2. Heuristic"
    putStrLn "\t 3. Heuristic with Lookahead"
    putStrLn "\t 4. Minimax (slowest)"
    putStr "\nYour choice: "
    line <- getLine
    case readMaybe line of
        Just 1 -> return randomPlayer
        Just 2 -> return heuristicPlayer
        Just 3 -> return lookaheadPlayer
        Just 4 -> return minimaxPlayer
        _ -> do
            putStrLn "\nInvalid choice.\n"
            chooseAI color

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
        if isNothing nextMove 
            then do
                putStrLn $ showColor turn ++ " has no valid moves."
                play $ nextState game nextMove
            else
                play $ nextState game nextMove

-- Play an AI vs AI tournament. 
-- TODO: integrate this with the console menu.
--       for now, can just call this from ghci with a tournmanent state to do AI benchmarking.
playTournament :: TournamentState -> IO ()
playTournament TournamentOver{blackWon = bw, whiteWon = ww, ties = t} = do
    putStrLn $ "Tournament Finished. White Won: " ++ show ww ++ 
                ". Black Won: " ++ show bw ++
                ". Ties: " ++ show t


playTournament tment@Tournament{game = 
        game@Game{board = board, currentPlayer = player, currentTurn = turn}} = do
    nextMove <- currentPlayer game board turn
    let gameState = nextState game nextMove
    let tournamentState = nextTournamentState tment gameState
    playTournament tournamentState

testTment :: TournamentState
testTment = Tournament {game = startState { whitePlayer = randomPlayer, 
                                      blackPlayer = randomPlayer, 
                                      currentPlayer = randomPlayer },
                             startGame = startState { whitePlayer = randomPlayer, 
                                      blackPlayer = randomPlayer, 
                                      currentPlayer = randomPlayer },
                             gamesLeft = 10,
                             blackWon = 0,
                             whiteWon = 0,
                             ties = 0}

-- Program entry point.
main :: IO ()
main = do
        putStrLn "\nWelcome to Othello\n"
        state <- setup
        case state of 
            Just state -> do
                play state
            Nothing ->
                return ()