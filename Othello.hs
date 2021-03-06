module Othello where

import Data.Maybe

data Color = White | Black deriving Eq
type Position = (Int, Int)
type Move = (Position, Color)
type Board = [Move]

type Player = Board -> Color -> IO (Maybe Move)

data GameState = Game {
    board :: Board,
    whitePlayer :: Player,
    blackPlayer :: Player,
    currentPlayer :: Player,
    currentTurn :: Color,
    scoreWhite :: Int,
    scoreBlack :: Int
} | GameOver {board :: Board, winner :: Maybe Color}

-- Set the game board size.
maxRow :: Int
maxRow = 7
maxCol :: Int
maxCol = 7
midRow :: Int
midRow = maxRow `div` 2
midCol :: Int
midCol = maxCol `div` 2

-- For printing board pieces.
instance Show Color where
    show White = "W"
    show Black = "B"

--  For printing messages to the user.
showColor :: Color -> String
showColor White = "White"
showColor Black = "Black"

-- Starting board.
startBoard :: Board
startBoard = [((midRow, midCol), White),
              ((midRow + 1, midCol + 1), White),
              ((midRow, midCol + 1), Black),
              ((midRow + 1, midCol), Black)]

-- Adds a valid move to the board, replacing an existing move if necessary
addMove :: Board -> Move -> Board
addMove board ((row, col), color)
    | null found = ((row, col), color) : board
    | otherwise = foldr replace [] board
    where found = filter (\((r, c), _) -> r == row && c == col) board
          replace ((r, c), clr) acc = if row == r && col == c
                                       then ((r, c), color) : acc
                                       else ((r, c), clr) : acc

-- Produce the color at the given position on the given board, or Nothing if the position is empty
getSquare :: Board -> Position -> Maybe Color
getSquare board (row, col)
    | null found = Nothing
    | otherwise = Just $ snd $ head found
    where found = filter (\((r, c), _) -> r == row && c == col) board

-- Directions to search for flipped pieces when making a move
searchDirs :: [(Int, Int)]
searchDirs = [(-1, 0)  {-- up --},
              (1, 0)   {-- down --},
              (0, -1)  {-- left --},
              (0, 1)   {-- right --},
              (-1, -1) {-- up/left --},
              (-1, 1)  {-- up/right --},
              (1, -1)  {-- down/left --},
              (1, 1)   {-- down/right --}]

-- Flip the given color.
flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

-- Convert one board position to a string
showSquare :: Board -> Position -> String
showSquare board (row, col)
    | isNothing square = addLineBreak "_ "
    | square == Just Black = addLineBreak "B "
    | otherwise = addLineBreak "W "
    where square = getSquare board (row, col)
          addLineBreak str = if col == maxCol
                             then str ++ show row ++ " \n"
                             else str

-- Produce a string representation of the given board
showBoard :: Board -> String
showBoard board = unwords (map show [0..maxCol]) ++ " \n"
                  ++ concatMap (showSquare board)
                        [(x, y) | x <- [0..maxRow], y <- [0..maxCol]]

-- Given a board and a list of moves, produce a new
--  board with the given squares flipped
flipSquares :: Board -> [Move] -> Board
flipSquares board toFlip = map helper board
    where helper ((row, col), color)
            | null found = ((row, col), color)
            | otherwise = ((row, col), flipColor color)
            where found = filter (\((r, c), _) -> r == row && c == col) toFlip

-- Produce a list of the pieces that would be flipped if the given move was made. 
--  If no there are no flips, the list is empty.
getFlipped :: Board -> Move -> [Move]
getFlipped board ((row, col), color) = concatMap (helper board (row, col) color []) searchDirs
    where helper board (x, y) color acc (dx, dy)
            | isOffBoard = []
            | isNothing nextSquare = []
            | fromJust nextSquare == color = acc
            | otherwise = helper board nextPos color ((nextPos, fromJust nextSquare) : acc) (dx, dy)
            where isOffBoard = x + dx < 0 || x + dx > maxRow || y + dy < 0 || y + dy > maxCol
                  nextPos = (x + dx, y + dy)
                  nextSquare = getSquare board nextPos

-- Attempt to produce a new board by placing a piece at the given position. If the move 
--  is invalid (because the board isn't empty at that position, or the move wouldn't flip 
--  any pieces), produce Nothing. 
doMove :: Board -> Move -> Maybe Board
doMove board (pos, color)
    | isJust $ getSquare board pos = Nothing
    | null flipped = Nothing
    | otherwise = Just $ flipSquares (addMove board (pos,color)) flipped
    where flipped = getFlipped board (pos, color)

-- Produce a list of all the valid moves on the given board for the given color by exhaustively
--  checking every position on the board.
getValidMoves :: Board -> Color -> [Move]
getValidMoves board color = [move | move <- allMoves, isJust $ doMove board move]
    where allMoves = [((x, y), color) | x <- [0..maxRow], y <- [0..maxCol]]

-- Produces true if the given color has a valid move
hasValidMoves :: Board -> Color -> Bool
hasValidMoves board color = getValidMoves board color /= []

-- Apply the given move and produce the next game state. Caller passes in Nothing if
--  a player has no valid moves available, in which case nextState just switches the current turn.
--  Otherwise, nextState updates the board and player scores. It's the caller's responsibility to check 
--  if players have valid moves available and pass either Nothing or a valid move to nextState.
nextState :: GameState -> Maybe Move -> GameState
nextState game move
    | isNothing move = game {
        currentPlayer = nextPlayer,
        currentTurn = nextTurn }
    | isGameOver nextBoard = GameOver {
        board = nextBoard,
        winner = getWinner nextScoreWhite nextScoreBlack }
    | otherwise = game {
        board = nextBoard,
        currentPlayer = nextPlayer,
        currentTurn = nextTurn,
        scoreWhite = nextScoreWhite,
        scoreBlack = nextScoreBlack }
    where nextBoard = fromJust $ doMove (board game) (fromJust move)
          nextPlayer = if currentTurn game == White
                            then blackPlayer game
                            else whitePlayer game
          nextTurn = flipColor $ currentTurn game
          nextScoreWhite = scoreBoard nextBoard White
          nextScoreBlack = scoreBoard nextBoard Black

-- Produces true if neither color has a valid move i.e. the game is over
isGameOver :: Board -> Bool
isGameOver board = not $ hasValidMoves board White || hasValidMoves board Black

-- Scores the board for the given color. The score for a color is the number 
--  of pieces of that color on the board
scoreBoard :: Board -> Color -> Int
scoreBoard board color = length $ filter (\((_,_), clr) -> clr == color) board

-- Produces the Color with the highest score on the given board, or Nothing
--  if the game is a tie
getWinner :: Int -> Int -> Maybe Color
getWinner whiteScore blackScore
  | whiteScore > blackScore = Just White
  | whiteScore == blackScore = Nothing
  | otherwise = Just Black