-------      CPSC 312      -------
-- Project 2            12/6/15 --
-- Brian Taylor    &    Leo Liu --
-- 52311859            51845139 --
-- z2b0b                   x8b9 --
----------------------------------

-------- INITIAL COMMENTS -----------------------------------------------
--
-- We chose to use the starter file that was provided, creating helper 
-- functions where necessary. The only functions we modified in any way
-- with respect to their signatures were the two functions that implement
-- the minimax algorithm.
--
-- The program is started with the crusher function. The crusher function
-- requires a starting board (with or without a history), a player who
-- moves first, a depth to search down to and the dimensions of the board.
-- The crusher function sets everything up based on its inputs and calls
-- the stateSearch function, which checks to see if the game is over. If
-- not it generates a search tree and calls the necessary functions to apply
-- the minimax algorithm to that search tree. Those functions first determine
-- the best value, and then the appropriate next board corresponding to
-- that value. This 'best' next board is returned to stateSearch, which
-- returns it to crusher, which prepends it to the list of prior boards.

data Piece = D | W | B deriving (Eq, Show)
type Point = (Int, Int)
type Tile  = (Piece, Point)
type Board = [Piece]
type Grid = [Point]
type State = [Tile]
data Next a = Next {usedDepth :: Int,
                    newBoard :: a,
                    seenBoards :: [a],
                    cplayer :: Piece}
data Tree a = Node {depth :: Int,
                    board :: a,
                    nextBoards :: [Tree a]
                    } deriving (Show)
type BoardTree = Tree Board
type Slide = (Point,Point)
type Jump = (Point,Point,Point)
type Move = (Point,Point)

-- Some test results to see what functions are producing 

setDepth = 2  -- constant for testing
setDim = 3  -- constant for testing

-- A 'complete' game resulting in a win for White with the computer
-- alternating players.
-- The command printHistory (reverse run40) displays the game, board by
-- board. The final few boards show a static board as is the prescribed
-- behaviour for the end of the game.
run01 = crusher ["WWW-WW-------BB-BBB"] 'W' setDepth setDim
run02 = crusher run01 'B' setDepth setDim
run03 = crusher run02 'W' setDepth setDim
run04 = crusher run03 'B' setDepth setDim
run05 = crusher run04 'W' setDepth setDim
run06 = crusher run05 'B' setDepth setDim
run07 = crusher run06 'W' setDepth setDim
run08 = crusher run07 'B' setDepth setDim
run09 = crusher run08 'W' setDepth setDim
run10 = crusher run09 'B' setDepth setDim
run11 = crusher run10 'W' setDepth setDim
run12 = crusher run11 'B' setDepth setDim
run13 = crusher run12 'W' setDepth setDim
run14 = crusher run13 'B' setDepth setDim
run15 = crusher run14 'W' setDepth setDim
run16 = crusher run15 'B' setDepth setDim
run17 = crusher run16 'W' setDepth setDim
run18 = crusher run17 'B' setDepth setDim
run19 = crusher run18 'W' setDepth setDim
run20 = crusher run19 'B' setDepth setDim
run21 = crusher run20 'W' setDepth setDim
run22 = crusher run21 'B' setDepth setDim
run23 = crusher run22 'W' setDepth setDim
run24 = crusher run23 'B' setDepth setDim
run25 = crusher run24 'W' setDepth setDim
run26 = crusher run25 'B' setDepth setDim
run27 = crusher run26 'W' setDepth setDim
run28 = crusher run27 'B' setDepth setDim
run29 = crusher run28 'W' setDepth setDim
run30 = crusher run29 'B' setDepth setDim
run31 = crusher run30 'W' setDepth setDim
run32 = crusher run31 'B' setDepth setDim
run33 = crusher run32 'W' setDepth setDim
run34 = crusher run33 'B' setDepth setDim
run35 = crusher run34 'W' setDepth setDim
run36 = crusher run35 'B' setDepth setDim
run37 = crusher run36 'W' setDepth setDim
run38 = crusher run37 'B' setDepth setDim
run39 = crusher run38 'W' setDepth setDim
run40 = crusher run39 'B' setDepth setDim

-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) plyr depth n = (newnew:(current:old))
  where
    newnew = boardToStr newboard
    newboard = stateSearch board history grid slides jumps player depth n
    player = convertCharToPiece plyr
    board = sTrToBoard current
    history = map sTrToBoard old
    grid = generateGridn n
    slides = generateSlides grid n
    jumps = generateLeaps grid n

-- helper function for crusher that converts a single character to a Piece
convertCharToPiece :: Char -> Piece
convertCharToPiece 'W' = W
convertCharToPiece 'B' = B
convertCharToPiece '-' = D

-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, o/w False

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n
    | countPieces W board < n = True
    | countPieces B board < n = True
    | otherwise               = elem board history

-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Returns: the Board corresponding to the string

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
    where
      check 'W' = W
      check 'B' = B
      check '-' = D

-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Returns: the String corresponding to the board 

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where 
      check W = 'W'
      check B = 'B'
      check D = '-'

-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--         initialized to []
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
    | n3 == -1    = acc
    | otherwise   = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
      where
        row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
        nn1 = if n2 > 0 then n1 + 1 else n1 - 1

-- wrapper version of generateGrid, with only one argument, the side length
generateGridn :: Int -> Grid
generateGridn n = generateGrid n (n-1) (2 * (n - 1)) []

-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- It was difficult to find an elegant way to complete this function.
-- The arithmetic of a hexagonal playing board proved challenging.
-- In the end we broke the problem up into horizontal slides, slides that
-- were in the top half of the board, and slides that were in the bottom
-- half of the board. The swap2 function was applied to the latter two sets
-- as the list comprehensions only generated the 'up-down' slides.
--
-- Returns: the list of all Slides possible on the given grid

generateSlides :: Grid -> Int -> [Slide]  
generateSlides b n = horiz_neighbor b ++
                     above ++ swap2 above ++
                     below ++ swap2 below
                     where
                       above = above_neighbor b n
                       below = below_neighbor b n

-- horiz_neighbor finds all slides on the horizontal axis                     
horiz_neighbor :: Grid -> [Slide]
horiz_neighbor l = [(p1,p2) | p1 <- l, p2 <- l,
                     abs (fst p1 - fst p2) == 1,
                     snd p1 == snd p2]
       
-- above_neighbor finds all slides from the midline looking up                   
above_neighbor :: Grid -> Int -> [Slide]
above_neighbor l n = [(p1,p2) | p1<-l, p2<-l, snd p1 < n, 
                      snd p1 - snd p2 == 1,
                      fst p2 == fst p1 || fst p2 == fst p1 - 1]

-- below_neighbor finds all slides from the midline looking down
below_neighbor :: Grid -> Int -> [Slide]                     
below_neighbor l n = [(p1, p2) | p1 <- l, p2 <- l, snd p1 >= n - 1,
                      snd p2 - snd p1 == 1,
                      fst p2 == fst p1 || fst p2 == fst p1 - 1]

-- swap2 generates a list of tuples where the 
-- first and second elements have been swapped
swap2 :: [(a, a)] -> [(a, a)]                   
swap2 l = [(p2, p1) | (p1, p2) <- l]

--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Having solved the problem of generating the slides this function wasn't
-- too bad. We broke it into four separate sub-problems. Generating horizontal
-- leaps was straightforward, leaps entirely in the top half or bottom half
-- were handled independently and then finally leaps that spanned the midline.
-- A special helper was written to swap the triples.
--
-- Returns: the list of all Jumps possible on the given grid
--

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = flat ++ swap3 flat ++
                    top ++ swap3 top ++
                    bottom ++ swap3 bottom ++
                    span ++ swap3 span
                    where
                      flat   = horizontalLeaps b
                      top    = topLeaps b n
                      bottom = bottomLeaps b n
                      span   = spanLeaps b n

horizontalLeaps :: Grid -> [Jump]
horizontalLeaps b = [(p1, p2, p3) | p1 <- b, p2 <- b, p3 <- b,
                     (snd p1 == snd p2 && snd p2 == snd p3) &&
                     (fst p3 == fst p2 + 1 && fst p2 == fst p1 + 1)] 

topLeaps :: Grid -> Int -> [Jump]
topLeaps b n = [(p1, p2, p3) | p1 <- b, p2 <- b, p3 <- b,
                snd p3 < n,
                (fst p3 == fst p2 && fst p2 == fst p1 &&
                 snd p3 == snd p2 + 1 && snd p2 == snd p1 + 1) ||
                (fst p3 == fst p2 + 1 && fst p2 == fst p1 + 1 &&
                 snd p3 == snd p2 + 1 && snd p2 == snd p1 + 1)]

bottomLeaps :: Grid -> Int -> [Jump]
bottomLeaps b n = [(p1, p2, p3) | p1 <- b, p2 <- b, p3 <- b,
                snd p1 >= n - 1,
                (fst p3 == fst p2 && fst p2 == fst p1 &&
                 snd p3 == snd p2 + 1 && snd p2 == snd p1 + 1) ||
                (fst p3 == fst p2 - 1 && fst p2 == fst p1 - 1 &&
                 snd p3 == snd p2 + 1 && snd p2 == snd p1 + 1)]
                 
spanLeaps :: Grid -> Int -> [Jump]
spanLeaps b n = [(p1, p2, p3) | p1 <- b, p2 <- b, p3 <- b,
                snd p2 == n - 1,
                (fst p3 == fst p2 && fst p2 == fst p1 + 1 &&
                 snd p3 == snd p2 + 1 && snd p2 == snd p1 + 1) ||
                (fst p3 == fst p2 - 1 && fst p2 == fst p1 &&
                 snd p3 == snd p2 + 1 && snd p2 == snd p1 + 1)]

-- swap3 generates a list of tuples where the 
-- first and third elements have been swapped
swap3 :: [(a, a, a)] -> [(a, a, a)]                   
swap3 l = [(p3, p2, p1) | (p1, p2, p3) <- l]

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--

stateSearch :: Board -> [Board] -> Grid -> [Slide] ->
               [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth n
    | gameOver board history n = board
    | otherwise = minmax tree history False player n
  where
    tree = generateTree board history grid slides jumps player depth n
--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTree :: Board -> [Board] -> Grid -> [Slide] -> 
               [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n
    | depth == 0         = Node {depth=depth, board=board, nextBoards=[]}
    | null nextBds       = Node {depth=depth, board=board, nextBoards=[]}
    | elem board history = Node {depth=depth, board=board, nextBoards=[]}
    | otherwise          = Node {depth=depth, board=board, nextBoards =
      [generateTree x history grid slides jumps (switchPlayer player)
       (depth-1) 3| x <- nextBds]}
  where
    nextBds = generateNewStates board (board:history) grid slides jumps player

-- helper function to switch player
switchPlayer :: Piece -> Piece
switchPlayer p
    | p == W    = B
    | otherwise = W

--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> 
                     [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player =
    filter (\x -> not $ elem x history) (map (nextBoard player state) moves)
  where
    moves = moveGenerator state slides jumps player
    state = zip board grid

nextBoard player state move = fst (unzip (changeInState move player state))

changeInState move player state
    | null state     = []
    | pt == fst move = (D, pt):changeInState move player (tail state)
    | pt == snd move = (player, pt):changeInState move player (tail state)
    | otherwise      = (pc, pt):changeInState move player (tail state)
  where
    pc = fst (head state)
    pt = snd (head state)

--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state:  a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps:  the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns:   the list of all valid moves that the player could make
--

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player = valSlides ++ valJumps
  where
    valSlides = validSlides state slides player
    valJumps  = validJumps state jumps player

-- validJumps determines all the valid jumps and returns them as moves
validJumps :: State -> [Jump] -> Piece -> [Move]
validJumps state jumps player =
    [(fst3 jp, thd3 jp) | jp <- jumps,
                          elem (fst3 jp) playerPoints,
                          elem (snd3 jp) playerPoints,
                          not (elem (thd3 jp) playerPoints)]
  where
    playerTiles = filter (\x -> fst x == player) state
    playerPoints = [(fst (snd x), snd (snd x)) | x <- playerTiles]

-- helper functions for dealing with triples
fst3 :: (a, a, a) -> a 
fst3 (a, _, _) = a

snd3 :: (a, a, a) -> a 
snd3 (_, b, _) = b

thd3 :: (a, a, a) -> a 
thd3 (_, _, c) = c

-- validSlides determines all the valid slides and returns them as moves
validSlides :: State -> [Slide] -> Piece -> [Move]
validSlides state slides player =
    [(fst sl, snd sl) | sl <- slides,
                        elem (fst sl) playerPoints,
                        elem (snd sl) emptyPoints]
  where
    playerTiles = filter (\x -> fst x == player) state
    emptyTiles = filter (\x -> fst x == D) state
    playerPoints = [(fst (snd x), snd (snd x)) | x <- playerTiles]
    emptyPoints = [(fst (snd x), snd (snd x)) | x <- emptyTiles]

--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Bool indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
boardEvaluator player history n board myTurn
    | countPieces player board < n                = -100
    | countPieces (switchPlayer player) board < n = 100
    | player == W                                 = white - black + bonus
    | otherwise                                   = black - white + bonus
  where
    white = countPieces W board
    black = countPieces B board
    bonus = bonusForCentre board player

-- if player occupies the centre of the board there is a one point bonus
bonusForCentre :: Board -> Piece -> Int
bonusForCentre board player
    | head (findCentre board) == player                = 1
    | head (findCentre board) == (switchPlayer player) = -1
    | otherwise                                        = 0

-- findCentre returns the middle element of an odd length list
findCentre :: [a] -> [a]
findCentre l
    | length l == 1 = l
    | otherwise     = findCentre $ take (length l - 2) (drop 1 l)

-- helper function to count a player's pieces on the board
countPieces :: Piece -> Board -> Int
countPieces player board
    | null board           = 0
    | player == head board = 1 + countPieces player (tail board)
    | otherwise            = countPieces player (tail board)

--
-- minmax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and applies the minmaxTree function to find out the best course of action
-- It looks for the move that resulted in the value that minmaxTree returned.
-- There may be more than one such move and this function simply takes the 
-- first one.
--
-- Arguments:
-- -- tree:      the current tree generated to the specified depth
-- -- history:   the list of boards played to that point
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
--               or miniziming the goodness values of its children
-- -- player:    the current player
-- -- n:         the dimension of the board
--
-- Returns:      the next 'best board'
--

minmax :: BoardTree -> [Board] -> Bool -> Piece -> Int -> Board
minmax tree history maxPlayer player n
    = head [board (snd tr) | tr <- ziptrees, fst tr == minimum treevalues]
  where
    treevalues = applyMinmax listoftrees (board tree:history) 
                 (not maxPlayer) (switchPlayer player) n
    listoftrees = genListOfTrees (board tree) history grid slides
                  jumps player
                 (depth tree - 1) n
    ziptrees = zip treevalues listoftrees
    slides = generateSlides grid n
    jumps = generateLeaps grid n
    grid = generateGridn n

-- generate a list of new trees, one for each board in nextBoards
-- set depth to one less than original depth
genListOfTrees :: Board -> [Board] -> Grid -> [Slide] -> 
                  [Jump] -> Piece -> Int -> Int -> [BoardTree]
genListOfTrees bd history grid slides jumps player depth n = 
    [generateTree (board bd) history grid slides jumps player depth n | 
     bd <- nextBoards tree]
   where
     tree = generateTree bd history grid slides jumps player depth n

-- helper function for minmax function
applyMinmax :: [BoardTree] -> [Board] -> Bool -> Piece -> Int -> [Int]
applyMinmax listOfTrees history maxPlayer player n =
    map (\x -> minmaxTree x history maxPlayer player n) listOfTrees

--
-- minmaxTree
--
-- This function is a helper to the actual minmax function, it consumes 
-- a search tree, a board history a player and based on whether it would
-- have been the maximizing player's turn, it accordingly propogates the
-- values upwards until it reaches the top to the base node, and produces
-- that value.
--
-- Arguments:
-- -- tree:      a BoardTree
-- -- history:   a list of all the boards that preceded this move
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
--               or miniziming the goodness values of its children
-- -- player:    the player whose turn it is
-- -- n:         the dimension of the board
--
-- Returns:      the minimax value at the top of the tree
--

minmaxTree :: BoardTree -> [Board] -> Bool -> Piece -> Int -> Int
minmaxTree tree history maxPlayer player n
    | depth tree == 0 = boardEvaluator player history n (board tree) True
    | maxPlayer       = maximum (map maxHelper (nextBoards tree))
    | otherwise       = minimum (map minHelper (nextBoards tree))
  where
    maxHelper node = minmaxTree node history (not maxPlayer) player n
    minHelper node = minmaxTree node history (not maxPlayer) player n

------ END OF MAIN PROGRAM --------

-- The following functions are for testing purposes only and to get an
-- idea of what a typical game (n=3) looks like on the board.

-- prints the output of run into readable form (only for sidelength = 3)
printHistory :: [String] -> IO ()
printHistory listOfBoard = putStr (joinbd listOfBoard)

-- joins all the boards into one string
joinbd :: [String] -> String
joinbd listOfBoard
    | null listOfBoard = ""
    | otherwise        = a ++ joinbd (tail listOfBoard)
  where
    b = addSpace (head listOfBoard)
    a = "  " ++ take 6 b ++ "\n" ++ 
        " " ++ take 8 (drop 6 b) ++ "\n" ++
        take 10 (drop 14 b) ++ "\n" ++
        " " ++ take 8 (drop 24 b) ++ "\n" ++
        "  " ++ drop 32 b ++ "\n\n"
  
-- prettyPrint outputs a human readable board for sidelength = 3
prettyPrint :: Board -> IO()
prettyPrint bd = putStr a
  where
    b = addSpace (boardToStr bd)
    a = "  " ++ take 6 b ++ "\n" ++ 
        " " ++ take 8 (drop 6 b) ++ "\n" ++
        take 10 (drop 14 b) ++ "\n" ++
        " " ++ take 8 (drop 24 b) ++ "\n" ++
        "  " ++ drop 32 b

-- helper for prettyPrint - addSpace adds one space after each char in a string
addSpace :: String -> String
addSpace xs = if length xs <= 1
              then xs
              else take 1 xs ++ " " ++ addSpace (tail xs)
              
