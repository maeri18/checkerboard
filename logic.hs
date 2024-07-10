import Data.List

type Coord = (Int, Int) --position on the checkerboard
type Checkerboard = [[Char]] --checkerboard is a list of 8 * 8 Chars. b for black pawn, B for black lady. Similarly, w for white pawn and W for white lady. empty cell is e
size = 8
--test = [['']]
{-Valide_move takes the initial position of the pawn and the position after playing.
It also takes the checkboard and the color of the pawn we want to move.
The function's output is true if the move is a valid move, and false else. -}

init_board_whites_below :: Int->Checkerboard --initializes a board of size l * l, with whites below
init_board_whites_below l | mod l 2 == 1 = error "Odd raw size to initialize the checker!!!"
                          | l <= 0 = error "Enter a positive and even raw size  !!"
                          |otherwise = [[cell|y<-[0..l-1], let cell = if x == (div l 2) || x == (div l 2) - 1 then 'e' else if  x < (div l 2) && (((mod x 2)==0 && (mod y 2)==1) || ((mod x 2) == 1 && (mod y 2) == 0)) then 'b' else if x >= (div l 2) && (((mod x 2)==0 && (mod y 2)==1) || ((mod x 2) == 1 && (mod y 2) == 0)) then 'w' else  'e' ]|x<-[0..l-1]]

valid_move_pawn_whites_below :: Coord -> Coord ->Char-> Checkerboard -> Bool
valid_move_pawn_whites_below _ _ _ [] = False
valid_move_pawn_whites_below (x1,y1) (x2,y2) color xs | (xs!!x1)!!y1 /=  color = False
                                         | color == 'w' && x2 == (x1 - 1) && (y2 == y1 - 1 || y2 == y1 + 1) = True
                                         | color == 'b' && x2 == (x1 + 1) && (y2 == y1 - 1 || y2 == y1 + 1) = True
                                         | otherwise = False



--valid_move_lady :: Coord -> Coord -> Char -> Checkerboard -> Bool

move_pawn_whites_below :: Coord -> Coord ->Char -> Checkerboard -> Checkerboard
move_pawn_whites_below _ _ _ [] = []
move_pawn_whites_below pos1@(x1,y1) pos2@(x2,y2) color board  | (board !!x1)!!y1 /= color = board
                                                              |(valid_move_pawn_whites_below pos1 pos2 color board ) = [[res|y<-[0..(size-1)], let res = if x1 == x && y1 == y then 'e' else if x == x2 && y == y2 then color else (board!!x)!!y ]|x<-[0..(size-1)]]

----------------Blacks below
init_board_blacks_below :: Int->Checkerboard --initializes a board of size l * l, with blacks below
init_board_blacks_below l | mod l 2 == 1 = error "Odd raw size to initialize the checker!!!"
                          | l <= 0 = error "Enter a positive and even raw size  !!"
                          |otherwise = [[cell|y<-[0..l-1], let cell = if x == (div l 2) || x == (div l 2) - 1 then 'e' else if  x < (div l 2) && (((mod x 2)==0 && (mod y 2)==1) || ((mod x 2) == 1 && (mod y 2) == 0)) then 'w' else if x >= (div l 2) && (((mod x 2)==0 && (mod y 2)==1) || ((mod x 2) == 1 && (mod y 2) == 0)) then 'b' else  'e' ]|x<-[0..l-1]]


valid_move_pawn_blacks_below :: Coord -> Coord -> Char -> Checkerboard -> Bool
valid_move_pawn_blacks_below _ _ _ [] = False
valid_move_pawn_blacks_below (x1,y1) (x2,y2) color board | (board !!x1)!!y1 /=  color = False
                                         | color == 'w' && x2 == (x1 + 1) && (y2 == y1 - 1 || y2 == y1 + 1) = True
                                         | color == 'b' && x2 == (x1 - 1) && (y2 == y1 - 1 || y2 == y1 + 1) = True
                                         | otherwise = False       

move_pawn_blacks_below :: Coord -> Coord -> Char -> Checkerboard -> Checkerboard
move_pawn_blacks_below _ _ _ [] = []
move_pawn_blacks_below pos1@(x1,y1) pos2@(x2,y2) color board  | (board !!x1)!!y1 /= color = board
                                            |(valid_move_pawn_blacks_below pos1 pos2 color board ) = [[res|y<-[0..(size-1)], let res = if x1 == x && y1 == y then 'e' else if x == x2 && y == y2 then color else (board!!x)!!y ]|x<-[0..(size-1)]]
                                            
--move_lady_blacks_below

{-
-- player_white

-- player_black

win

jump_pawn_possible

jump_pawn

jump_lady

play-}

print_board :: Checkerboard -> IO ()
print_board [] = putStr "\n"
print_board (x:xs) = do putStrLn (intersperse ' ' x) 
                        print_board xs

