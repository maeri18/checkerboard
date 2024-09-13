import Data.List
import Data.Char

type Coord = (Int, Int) --position on the checkerboard
type Checkerboard = [[Char]] --checkerboard is a list of 8 * 8 Chars (because size = 8). b for black pawn, B for black lady. Similarly, w for white pawn and W for white lady. empty cell is e
size = 8

--test = [['']]
{-Valide_move takes the initial position of the pawn and the position after playing.
It also takes the checkboard and the color of the pawn we want to move.
The function's output is true if the move is a valid move, and false else. -}

-- Whites below

{-This function checks whether the coordinate entered are within the borders of the checkerboard-}
within_borders :: Coord -> Bool
within_borders (x,y) = x >=0 && x<size && y >=0 && y < size

{-Function to access a cell and return the content of that cell : 
either 'b', 'w', 'e', 'B' or 'W' given that the cell is within the borders of the board.
 Else it returns ' '  (for example if the board is empty).-} 
access_cell :: Checkerboard -> Coord -> Char
access_cell [] _ = ' '
access_cell board (x,y) = if (within_borders (x,y)) then (board!!x)!!y else ' '

{-Function to check if the cell at position 'pos' is of the color c. 
colors : b for black pawn, w for white pawn, e for empty cell, B for black lady and W for white lady.
-}
check_cell_contain :: Checkerboard -> Coord -> Char -> Bool
check_cell_contain [] _ _ = False
check_cell_contain board pos@(x,y) c = ((access_cell board pos) == c)

{-Function to check if a cell contains a pawn (white or black), a lady or is empty-}
{-Functions to check if a move is valid. 
    It takes as parameters:
    The board, the initial coordinates of the pawn, the target coordinates, and returns True if the move is valid
    It returns False if the move is not valid, or the initial cell is empty
    Conditions for valid move:
    * The initial cell can't be empty. The pawn/lady on that cell will determine how to examine the validity of the move. 
    * Neither pawns nor ladies can land on an occupied cell, be it occupied by an adversary pawn/lady or not
    * Both the initial and target positions must be within the borders of the board

    * pawns and ladies only move on black cells. 
        Black cells have coordinates of the form (2x+1, 2y) or (2x,2y+1) where x and y are integers so that
        coordinates are within the borders of the board. It means that for black cells, one coordinate must be odd and the other even.
    * Pawns skip one cell while moving if they are not jumping an adversary pawn
    * Ladies can skip any number of cells, even if not jumping an adversary pawn
    * In case of possible jumping, the longest (or one of the longest) jump is mandatory to be taken
    * The board should not be an empty list-}
valid_move :: Checkerboard -> Coord -> Coord -> Bool
valid_move [] _ _ _ = False
valid_move board pos_init@(xi,yi) pos_target@(xt,yt) |cell_init == 'e' || cell_target \= 'e' || cell_init == ' ' || cell_target == ' '  = False --checks first three validity conditions above
                                                           |((odd xi) && (odd yi)) || ((even xi) && (even yi)) = error "Somehow the initial position of the pawn to move is not a black cell"
                                                           |((odd xt) && (odd yt)) || ((even xt) && (even yt)) = False
                                                           |(jumping_possible board cell_init) =
                                                           |not (jumping_possible board cell_init) = elem pos_target (possible_positions board pos_init)
                                            where cell_init = (access_cell board pos_init)
                                                  cell_target = (access_cell board pos_target)

{-Function to return the list of possible positions where a pawn/lady could land given that no jumping is possible.
Takes as argument the board and the position of the pawn
Doesn't test if the positions outputed are empty, so this function must be used by another one that tests it.-}
possible_positions :: Checkerboard -> Coord -> [Coord]
possible_positions [] _ = []
possible_positions board pos@(x,y)| cell_init == 'B' || cell_init == 'W' = diagonales
                                  | cell_init == 'b' || cell_init == 'w' = first_level_diagonales
                                  | otherwise = [] --in case the cell is empty or out of the board
                                  where cell_init = (access_cell board pos)
                                       diagonales = [ res|i<-[1..size], let res = (x+i,y+i), (within_borders res)] ++ [ res|i<-[1..size], let res = (x-i,y+i), (within_borders res)] ++ [ res|i<-[1..size], let res = (x-i,y-i), (within_borders res)] ++ [ res|i<-[1..size], let res = (x+i,y-i), (within_borders res)]
                                       first_level_diagonales = [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]

{-Function to decide whether or not a jump movement is possible for the pawn/lady in a cell
Returns True if the jump is possible, and False else-}
jump_possible :: Checkerboard -> Coord -> Bool
jump_possible [] _ = []
jump_possible board pos@(x,y)| null possible_landings  = False
                             | otherwise = (exist_jumpable_cell possible_landings)
                             where cell_init = (access_cell board pos)
                                   possible_landings = (possible_positions board pos)
                                   exist_jumpable_cell xs | xs == [] = False
                                                           | let cell = (access_cell board (head xs)) in (cell /= 'e' && cell /= ' ' && (access_cell board (cell_behind_potentially_jumpable_cell pos cell))) == 'e' = True
                                                           | otherwise = (exist_non_empty_cell (tail xs))
                                   
{-Utilitary function to determine the cell 'behind' an occupied cell with respect to the cell that jump
This function helps to calculate the coordinates of the cell behind the jumpable cell, so that the pawn/lady at position pos can jump
if the cell behind is empty.-}
cell_behind_potentially_jumpable_cell pos@(x,y) jump_cell@(xj,yj) | x > xj && y > yj = (xj-1,yj-1)
                                                      | x < xj && y > yj = (xj+1,yj-1)
                                                      | x > xj && y < yj = (xj-1,yj+1)
                                                      | x < xj && y < yj = (xj+1,yj+1)                                 

{-Function to determine the longest jump possible given the board and a position.-}
{-longest_jumps :: Checkerboard -> Coord -> [[Coord]]
longest_jumps [] _ = []
longest_jumps board pos@(x,y) | null possible_landings  = []
                              | 


                               where cell_init = (access_cell board pos)
                                     possible_landings = (possible_positions board pos)
                                     non_empty_cells = [ cell|(i,j)<-possible_landings, let cell = (access_cell board (xt,yt), cell \= 'e' && cell \=' ')] -- gathering cell among the possible landing cells that is not empty
                                    
                                     jumpable_cells = [(i,j)|(i,j)<-non_empty_cells, (access_cell board (cell_behind_potentially_jumpable_cell (i,j))) == 'e']
-}
{-Function to update the checkerboard after verifying that the move is valid-}

-- black below
