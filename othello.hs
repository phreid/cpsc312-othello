data Player = White | Black deriving (Show, Eq)
data Square = W | B | Empty deriving Eq
type Position = (Int, Int)
type Board = [(Position, Square)]

-- Set the game board size. Set to a small board for now to make game loop
--  testing easier.
maxRow = 2
maxCol = 2
midRow = maxRow `div` 2
midCol = maxCol `div` 2

instance Show Square where
    show W  = "W"
    show B  = "B"
    show Empty  = "_"

emptyBoard :: Board
emptyBoard = [((x, y), Empty) | x <- [0..maxRow], y <- [0..maxCol]]

startBoard :: Board
startBoard = setSquare
                    (setSquare
                        (setSquare
                            (setSquare emptyBoard (midRow, midCol) W)
                        (midRow, midCol + 1) B)
                    (midRow + 1, midCol) B)
                (midRow + 1, midCol + 1) W

-- Produce a new board with the given square set at the given position
setSquare :: Board -> Position -> Square -> Board
setSquare board (row, col) square = foldr helper [] board
        where helper ((r, c), sq) acc = if row == r && col == c
                                        then ((r, c), square) : acc
                                        else ((r, c), sq) : acc

-- Get the square at the given position
getSquare :: Board -> Position -> Square
getSquare board (row, col) = snd $ head $ filter (\((r, c), sq) -> r == row && c == col) board

-- Directions to search for flipped pieces when making a move
searchDirs = [(-1, 0)  {-- up --},
              (1, 0)   {-- down --},
              (0, -1)  {-- left --},
              (0, 1)   {-- right --},
              (-1, -1) {-- up/left --},
              (-1, 1)  {-- up/right --},
              (1, -1)  {-- down/left --},
              (1, 1)   {-- down/right --}]

-- Flip the given square. Flipping an empty square does nothing.
flipSquare :: Square -> Square
flipSquare W = B
flipSquare B = W
flipSquare Empty = Empty

-- Given a board and a list of (position, square) pairs to flip, produce a new
--  board with the given squares flipped
flipSquares :: Board -> [(Position, Square)] -> Board
flipSquares board toFlip = map helper board
    where helper ((row, col), sq)
            | null found = ((row, col), sq)
            | otherwise = ((row, col), flipSquare sq)
            where found = filter (\((r, c), _) -> r == row && c == col) toFlip

-- Produce a list of the pieces that would be flipped if the given piece were 
--  placed at the given position. If no there are no flips, the list is empty.
getSwitched :: Board -> Position -> Square -> [(Position, Square)]
getSwitched board (x, y) sq = concatMap (helper board (x, y) sq []) searchDirs
    where helper board (x, y) sq acc (dx, dy)
            | isOffBoard = []
            | nextSquare == Empty = []
            | nextSquare == sq = acc
            | otherwise = helper board nextPos sq ((nextPos, nextSquare) : acc) (dx, dy)
            where isOffBoard = x + dx < 0 || x + dx > maxRow || y + dy < 0 || y + dy > maxCol
                  nextPos = (x + dx, y + dy)
                  nextSquare = getSquare board nextPos

-- Attempt to produce a new board by placing a piece at the given position. If the move 
--  is invalid (because the board isn't empty at that position, or the move wouldn't flip 
--  any pieces), produce Nothing. 
doMove :: Board -> Position -> Square -> Maybe Board
doMove board pos sq
    | getSquare board pos /= Empty = Nothing
    | null switched = Nothing
    | otherwise = Just $ flipSquares (setSquare board pos sq) switched
    where switched = getSwitched board pos sq

-- Produce a list of all the valid moves on the given board for the given color by exhaustively
--  checking every position on the board.
getValidMoves :: Board -> Square -> [(Position, Square)]
getValidMoves board square = [(pos,sq) | (pos,sq) <- allPos, doMove board pos sq /= Nothing]
    where allPos = [((x, y), square) | x <- [0..maxRow], y <- [0..maxCol]]

-- Produces true if the given color has a valid move
hasValidMoves :: Board -> Square -> Bool
hasValidMoves board square = getValidMoves board square /= []

-- Produces true if neither color has a valid move i.e. the game is over
isGameOver :: Board -> Bool
isGameOver board = not $ hasValidMoves board W || hasValidMoves board B

-- Scores the board for the given color. The score for a color is the number 
--  of pieces of that color on the board
scoreBoard :: Board -> Square -> Int
scoreBoard board square = length $ filter (\((_,_), sq) -> sq == square) board

-- Produces the player with the highest score on the given board, or Nothing
--  if the game is a tie
getWinner :: Board -> Maybe Player
getWinner board
  | whiteScore > blackScore = Just White
  | whiteScore == blackScore = Nothing
  | otherwise = Just Black
  where
      whiteScore = scoreBoard board W
      blackScore = scoreBoard board B

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White

printSquare :: (Position, Square) -> String
printSquare ((r, c), sq)
    | c == maxCol = show sq ++ " " ++ show r ++ "\n"
    | otherwise = show sq ++ " "

printBoard :: Board -> IO ()
printBoard board = do
    let str = concatMap printSquare board
    putStrLn $ unwords (map show [0..maxCol])
    putStrLn str

-- Game loop. 
play :: Board -> Player -> IO ()
play board player
    | isGameOver board = do
        let winner = getWinner board
        let msg = maybe "Tie." show winner
        putStrLn $ "\nGame over. Winner: " ++ msg
    | not $ hasValidMoves board sq = do
        putStrLn $ "\n" ++ show player ++ " is out of moves."
        play board $ nextPlayer player
    | otherwise = do
        putStrLn ""
        putStrLn $ "Turn: " ++ show player
        printBoard board
        putStr "Enter a row: "
        r <- getLine
        putStr "Enter a col: "
        c <- getLine

        let nextBoard = doMove board (read r, read c) sq
        case nextBoard of
            Nothing -> do
                putStrLn "\nNot a valid move. Try again."
                play board player
            Just nextBoard -> play nextBoard $ nextPlayer player
    where sq = if player == White then W else B

main :: IO ()
main = play startBoard Black