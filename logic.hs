import Data.List

type Coord = (Int, Int) --position on the checkerboard
type Checkerboard = [[Char]] --checkerboard is a list of 8 * 8 Chars. b for black pawn, B for black lady. Similarly, w for white pawn and W for white lady. empty cell is e
size = 10
--test = [['']]
{-Valide_move takes the initial position of the pawn and the position after playing.
It also takes the checkboard and the color of the pawn we want to move.
The function's output is true if the move is a valid move, and false else. -}

within_borders :: Coord -> Bool
within_borders (x,y) = x >=0 && x<size && y >=0 && y < size

access_cell :: Checkerboard -> Coord -> Char
access_cell [] _ = ' '
access_cell xs (x,y) = if (within_borders (x,y)) then (xs!!x)!!y else ' '

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



--valid_move_lady_whites_below :: Coord -> Coord -> Char -> Checkerboard -> Bool

move_pawn_whites_below :: Coord -> Coord ->Char -> Checkerboard -> Checkerboard
move_pawn_whites_below _ _ _ [] = []
move_pawn_whites_below pos1@(x1,y1) pos2@(x2,y2) color board  | (board !!x1)!!y1 /= color = board
                                                              |(valid_move_pawn_whites_below pos1 pos2 color board ) = [[res|y<-[0..(size-1)], let res = if x1 == x && y1 == y then 'e' else if x == x2 && y == y2 then color else (board!!x)!!y ]|x<-[0..(size-1)]]

--move_lady_whites_below

{-This function takes the current disposition of the checkboard and the position of the pawn, and outputs the best possible jump(s) to take the most adversarial pawns
It outputs all possible jumps as lists in a list. If there is only one possible, then the list will contain a single list.-}


jump_pawn_possible_whites_below :: Checkerboard -> Coord -> [[Coord]]
jump_pawn_possible_whites_below [] _ = []
jump_pawn_possible_whites_below xs pos@(x,y) | x >= size || y >= size = error "A pawn can't be located there. Can't jump"
                                             | otherwise = longest_jumps $ reverse (search_jumps_pawn_whites_below xs color pos [])    --we have the reverse the lsit because we had coordinates at the start of the list each time
                                             where color = (access_cell xs (x,y))


search_jumps_pawn_whites_below :: Checkerboard -> Char -> Coord -> [Coord] -> [Coord]
search_jumps_pawn_whites_below [] _ _ _= []
search_jumps_pawn_whites_below _ _ (0,0) current_jumps = []
search_jumps_pawn_whites_below _ ' ' _ _ = []
search_jumps_pawn_whites_below _ 'e' _ _ = []
search_jumps_pawn_whites_below xs color pos@(x,y) current_jumps|(only_jumpable jump_pos1) == (0,0) && (only_jumpable jump_pos2) == (0,0) && (only_jumpable jump_pos3) == (0,0) && (only_jumpable jump_pos4) == (0,0)  = pos:current_jumps 
                                                               {-The line above checks for each possible jumping position if the position is not occupied by a white pawn, or if the position is occupied by a white pawn but the cell behind it is not empty. In all these case, we stop exploring because it is no more possible to perform a jump, whatever the direction.-}
                                                               | otherwise =  (search_jumps_pawn_whites_below xs color (only_jumpable jump_pos1) (pos:current_jumps)) ++ (search_jumps_pawn_whites_below xs color (only_jumpable jump_pos2) (pos:current_jumps)) ++ (search_jumps_pawn_whites_below xs color (only_jumpable jump_pos3) (pos:current_jumps)) ++ (search_jumps_pawn_whites_below xs color (only_jumpable jump_pos4) (pos:current_jumps))
                                                       where color_pawn1 = (access_cell xs (x+1,y+1))
                                                             color_pawn2 = (access_cell xs (x+1,y-1))
                                                             color_pawn3 = (access_cell xs (x-1,y+1))
                                                             color_pawn4 = (access_cell xs (x-1,y-1))
                                                             jump_pos1 = ((x+1,y+1),(x+2,y+2)) --first tuple is coordinate of the pawn to jump, and second tuple is coordinate of the (empty) cell behind it
                                                             jump_pos2 = ((x+1,y-1),(x+2,y-2))
                                                             jump_pos3 = ((x-1,y+1),(x-2,y+2))
                                                             jump_pos4 = ((x-1,y-1),(x-2,y-2))
                                                             cannot_jump pawn_color cell_color |cell_color == 'e' ||  cell_color == ' ' = True
                                                                                               | pawn_color == cell_color  = True
                                                                                               | otherwise = False
                                                             
                                                             only_jumpable ((x1,y1),(ex,ey)) | cannot_jump color (access_cell xs (x1,y1)) = (0,0)
                                                                                             | (access_cell xs (ex,ey)) /= 'e' = (0,0)
                                                                                             | elem (ex,ey) current_jumps = (0,0)
                                                                                             | otherwise = (ex,ey)

--To separate the output of search_jumps_pawn_whites_below into series of jumps [[Coord]]
separate_jump_series :: [Coord] -> Coord -> [[Coord]] -> [Coord] -> [[Coord]]
separate_jump_series [] _ res acc = (reverse acc):res
separate_jump_series (x:xs) pos0 res acc | x == pos0 = separate_jump_series xs pos0 ((reverse acc):res) [pos0] 
                                         | otherwise = separate_jump_series xs pos0 res (x:acc) 

longest_jumps :: [Coord] -> [[Coord]]
longest_jumps [] = []
longest_jumps xs = [x|x<-separated_list, (length x) == max_length]
                 where pos0 = xs!!0
                       separated_list = (separate_jump_series xs pos0 [] [])
                       max_length = maximum (map length separated_list)
                       

-----Functions to handle the white player's round and black player's round. It takes both the first 
--player_white_whites_below :: Coord -> Coord -> Checkerboard -> Checkerboard

 
--player_black_whites_below ::

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



jump_lady

play-}

print_board :: Checkerboard -> IO ()
print_board [] = putStr "\n"
print_board (x:xs) = do putStrLn (intersperse ' ' x) 
                        print_board xs



{- 
*****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
I kept this version of search jump to understand the first guard "cannot jump color_pawn1 ...." if I ever forget what I did in the version actually used above.

Note that this version doesn't not work.

search_jumps_whitepawn_whites_below :: Checkerboard -> Coord -> [Coord] -> [Coord]
search_jumps_whitepawn_whites_below [] _ _= []
search_jumps_whitepawn_whites_below xs pos@(x,y) current_jumps | cannotJump color_pawn1 && cannotJump color_pawn2 && cannotJump color_pawn3 && cannotJump color_pawn4 = current_jumps 
                                                               | otherwise = (search_jumps_whitepawn_whites_below xs (only_jumpable jump_pos1) pos:current_jumps) ++ (search_jumps_whitepawn_whites_below xs (only_jumpable jump_pos2) pos:current_jumps) ++ (search_jumps_whitepawn_whites_below xs (only_jumpable jump_pos3) pos:current_jumps) ++ (search_jumps_whitepawn_whites_below xs (only_jumpable jump_pos4) pos:current_jumps)
                                                               
                                                 where color_pawn1 = (access_cell xs (x+1,y+1))
                                                       color_pawn2 = (access_cell xs (x+1,y-1))
                                                       color_pawn3 = (access_cell xs (x-1,y+1))
                                                       color_pawn4 = (access_cell xs (x-1,y-1))
                                                       jump_pos1 = ((x+1,y+1),(x+2,y+2)) --first tuple is coordinate of the pawn to jump, and second tuple is coordinate of the (empty) cell behind it
                                                       jump_pos2 = ((x+1,y-1),(x+2,y-2))
                                                       jump_pos3 = ((x-1,y+1),(x-2,y+2))
                                                       jump_pos4 = ((x-1,y-1),(x-2,y-2))
                                                       cannotJump color = color == 'e' || color == 'w' || color == ' '
                                                       only_jumpable ((x1,y1),(ex,ey)) | cannotJump (access_cell xs (x1,y1)) = (0,0)
                                                                                       | (access_cell xs (x1,y1)) != 'e' = (0,0)
                                                                                       | otherwise = (x1,y1)
*****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
This version works.

search_jumps_blackpawn_whites_below [] _ _= []
search_jumps_blackpawn_whites_below _ (0,0) current_jumps = []
search_jumps_blackpawn_whites_below xs pos@(x,y) current_jumps |(only_jumpable jump_pos1) == (0,0) && (only_jumpable jump_pos2) == (0,0) && (only_jumpable jump_pos3) == (0,0) && (only_jumpable jump_pos4) == (0,0)  = pos:current_jumps 
                                                               {-The line below checks for each possible jumping position if the position is not occupied by a white pawn, or if the position is occupied by a white pawn but the cell behind it is not empty. In all these case, we stop exploring because it is no more possible to perform a jump, whatever the direction.-}
                                                               | otherwise =  (search_jumps_blackpawn_whites_below xs (only_jumpable jump_pos1) (pos:current_jumps)) ++ (search_jumps_blackpawn_whites_below xs (only_jumpable jump_pos2) (pos:current_jumps)) ++ (search_jumps_blackpawn_whites_below xs (only_jumpable jump_pos3) (pos:current_jumps)) ++ (search_jumps_blackpawn_whites_below xs (only_jumpable jump_pos4) (pos:current_jumps))
                                                       where color_pawn1 = (access_cell xs (x+1,y+1))
                                                             color_pawn2 = (access_cell xs (x+1,y-1))
                                                             color_pawn3 = (access_cell xs (x-1,y+1))
                                                             color_pawn4 = (access_cell xs (x-1,y-1))
                                                             jump_pos1 = ((x+1,y+1),(x+2,y+2)) --first tuple is coordinate of the pawn to jump, and second tuple is coordinate of the (empty) cell behind it
                                                             jump_pos2 = ((x+1,y-1),(x+2,y-2))
                                                             jump_pos3 = ((x-1,y+1),(x-2,y+2))
                                                             jump_pos4 = ((x-1,y-1),(x-2,y-2))
                                                             cannotJump color = color == 'e' || color == 'b' || color == ' '
                                                             
                                                             only_jumpable ((x1,y1),(ex,ey)) | cannotJump (access_cell xs (x1,y1)) = (0,0)
                                                                                             | (access_cell xs (ex,ey)) /= 'e' = (0,0)
                                                                                             | otherwise = (ex,ey)

*****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
Some useless function.

searches_jumps_pawn_whites_below :: Checkerboard -> Coord -> [[Coord]]
searches_jumps_pawn_whites_below [] _ = []
searches_jumps_pawn_whites_below xs pos@(x,y)|color == 'w' || color == 'b'  = (search_jumps_pawn_whites_below xs color pos []):[]
                                             |otherwise = []
                                            where color = (access_cell xs (x,y))

-}
