%% Suduko puzzle and solver

%%% Utility

%split_at(Index, FullList, Prefix, Postfix)            
% the element at Index will be part of Postfix
split_at(0, XS, [], XS) :- !.
split_at(Id, [X|XS], [X|PreX], Post) :-    
    Nid is Id - 1,
    split_at(Nid, XS, PreX, Post).

% **** BUILTIN in Tau-Prolog but not in SWI-Prolog    
% replicate(Element, Count, FullList)

replicate(_, 0, []) :- !.
replicate(Elem, Cnt, [Elem|XS]) :-
    Ncnt is Cnt - 1,
    replicate(Elem, Ncnt, XS).

% I used "once" becaue I thought it meant succeed if exactly one solution
% no.  It meant, give first solution.
% need way to check that only ONE solution exists.
unique(X) :-
    aggregate_all(count, X, 1).

%%%%%%%%%%%%%%%%%%%%%%
%% Grid data structure
%%%%%%%%%%%%%%%%%%%%%%

% empty_position is a constant atom indicating an empty position
% considered several grid options - such as nested loops or assert'd terms
% ended up with a linear list with the square representation abstracted on top.

% an empty grid is 9*9 = 81 empty positions.
empty_grid(Grid) :- replicate(empty_position, 81, Grid).

% all_XY is, in list of tuple form, all allowed (X, Y) positions in the grid
all_XY(XYS) :-
    bagof((X, Y), (
        between(0, 8, X),
        between(0, 8, Y)
     ), XYS).

% read_grid_element(Grid, X, Y, Element) maps the X and Y into the linear list
% and matches the element there.
read_grid_element(Grid, X, Y, E) :-
    Eid is Y * 9 + X,
    nth0(Eid, Grid, E).
    
% set_grid_element(Grid, X, Y, Element, NewGrid) replaces the grid element at X, Y
% and binds it to new grid.
set_grid_element(Grid, X, Y, E, Grid2) :-
    Eid is Y * 9 + X,
    split_at(Eid, Grid, Pre, [_|Post]),  % Exact element is first of Post, discard it
    append([Pre, [E], Post], Grid2).  % put E where the discarded element was
    
% row_value(Grid, Y, Value)  is true if Value is found in row Y of grid.
% excludes empty positions.
row_value(Grid, Y, Val) :-    
    between(0, 8, Id),
    read_grid_element(Grid, Id, Y, Val),
    Val \= empty_position.

% col_value(Grid, X, Value)  is true if Value is found in column X of grid.
% excludes empty positions.    
col_value(Grid, X, Val) :-
    between(0, 8, Id),
    read_grid_element(Grid, X, Id, Val),
    Val \= empty_position.
    
%%%%%%%%%%%%%%%%%%%%%%
%% Blocks data structure
%%%%%%%%%%%%%%%%%%%%%%
    
% blocks are 3x3 squares, such as (0,0) - (2,2); (3,0) - (5,2); (3,3) - (5,5); and (6,6) - (8,8)

% block(Pos, BlockPos) is true if Pos (an X or Y coordinate) is within the zone of the given BlockPos
% for example, position 1 is in the block (0, 1, 2); position 6 is in the block (6, 7, 8)
% this is identical for X and Y, so we don't care if position is X or Y.
block(V, VS) :-
    between(0, 2, V) -> between(0, 2, VS) ;
    between(3, 5, V) -> between(3, 5, VS) ;
    between(6, 8, V) -> between(6, 8, VS).

% block_pos(X, Y, BX, BY)  is true is X and Y are within the block described by BX and BY
% this is primarily used to generate all block-relative positions to a given X, Y
% so we can check what else is in the block.
block_pos(X, Y, BX, BY) :-
    block(X, BX),
    block(Y, BY).

% block_value(Grid, X, Y, Value) is true if Value is an element in the block which contains X, Y
% for example, if coordinate (3, 1) is given, then Value binds to all values in the block (3,0) - (5,2)
% excludes empty positions.    
block_value(Grid, X, Y, Val) :-
    block_pos(X, Y, BX, BY),
    read_grid_element(Grid, BX, BY, Val),
    Val \= empty_position.

    
%%%%%%%%%%%%%%%%%%%%%%
%% Sudoku rules/solver
%%%%%%%%%%%%%%%%%%%%%%

% propose_value(Grid, X, Y, Val) is designed to be used when X, Y is empty_position in the grid.
% binds Val to all possible values, compliant with Sudoku rules/solver
% considering row, column, and block uniqueness given existing values in the Grid.
% note: Values are bound in random order to facilitate puzzle construction, this does not adversely impact solving.
% propose_value can also be queried with all params instantiated to determine if a potential value is "plausible"
% this doesn't mean its right, just that it can fit in the current grid situation.
propose_value(Grid, X, Y, Val) :-        
    random_permutation([1, 2, 3, 4, 5, 6, 7, 8, 9], RX),
    member(Val, RX),
    \+ col_value(Grid, X, Val),  % a proposed value must not exist in the column
    \+ row_value(Grid, Y, Val), % a proposed value must not exist in the row
    \+ block_value(Grid, X, Y, Val).  % a proposed value must not exist in the block
    

% a wrapper around propose_value, designed to be used in an iterative loop/foldl
% fills in all empty positions with valid values
% the given coordinates may already be occupied by a value (useful for solving)    
% in which case, we ASSUME the value is valid and move on with NO CHANGES.
iterably_assign_proposed_value((X, Y), StartGrid, EndGrid) :-
    read_grid_element(StartGrid, X, Y, empty_position) -> % see if the current value at the position is empty
        (propose_value(StartGrid, X, Y, Val),  % if so, propose a new value for the position
        set_grid_element(StartGrid, X, Y, Val, EndGrid)  % update the grid with the proposed value
        ) ; EndGrid = StartGrid.  % otherwise, if the current value is non-empty, retain the grid unchanged.
     
     
% given a possibly empty or partially solved grid
% complete the solution!
% using randomness, suitable for generating from blank, new puzzles
solve(StartGrid, EndGrid)  :-
    all_XY(XYS),      % when we randomized position order, solver was unreliable.
    foldl(iterably_assign_proposed_value, XYS, StartGrid, EndGrid).
    
    
%%%%%%%%%%%%%%%%%%%%%%
%% Sudoku puzzle
%%%%%%%%%%%%%%%%%%%%%%

% A sudoku puzzle is a partially complete grid with exactly ONE unique valid solution.
% In this implementation, we create puzzles "backwards" - that is,
% we create fully solved Sudoku grid first, then punch holes in it,
% while validating that the holes do not take away the uniqueness of the solution.

% It should also be possible to assign numbers iteratively to a blank grid
% following placement rules
% until the solution becomes unique
% I have not tried this yet.
% Note: originally, in the generator, I implemented using random position fill out
% this failed a LOT - it turns out, filling in with systematically coordinates, but
% randomizing the values from the allowed list, was a much more stable approach.
% Therefore, it's possible the "fill in from blank for a while" puzzle technique may fail.

% punch_holes takes a full[er] grid, and a number of holes to punch, and punches that many holes
punch_holes(Grid, 0, Grid) :- unique(solve(Grid, _)).  % ensure a single unique solution once all holes punched.
punch_holes(Grid, N, HGrid) :- 
    all_XY(XYS),
    random_permutation(XYS, RXYS),  % randomally consider all positions
    punch_holes(Grid, N, HGrid, RXYS).  % punch holes in that order.

punch_holes(Grid, N, HGrid, [(X,Y)|XYS]) :-
    punch_hole(Grid, X, Y, GridP) ->  % punch the hole if safe; did we safely punch this hole?
    ( 
        NP is N - 1,  % if so, begin the recursive case - one less hole to punch
        punch_holes(GridP, NP, HGrid)
    ) ; punch_holes(Grid, N, HGrid, XYS).  % if not, discard this position from consideration and try again.
    

punch_hole(Grid, X, Y, HGrid) :-    % punch the hole if stay
    \+ read_grid_element(Grid, X, Y, empty_position),  % ensure the location is not already empty
    set_grid_element(Grid, X, Y, empty_position, HGrid), % empty the location
    unique(propose_value(HGrid, X, Y, _)).  % check that exactly one value can go into the location.

    % note: you may notice we check uniqueness twice - 
    % once when punching the hole (ensure only one possible value coudl go back and fill it)
    % and again, the whole puzzle, at the end with a solve command.
    % reason: if we wait until the end,  there might be a lot of backtracking if we made a mistake early
    % this leads to poor performance.  Although it does work.
    % we still retain the end check, though, because the single proposed value check is insufficient
    % example: we punch location A, only one value can go there; later we punch location B, only one
    % value can go there, but, in doing so, now two values could go in A.  We don't "go back and check"
    % I haven't verified this problem can actually happen, but it feels plausible.
 

% make_puzzle instantiates a Grid with as many holes as specified by Difficulty
% A good rule seems to be Difficulty between 20 and 50.
% this can fail due to getting stuck. 
% Puzzle is the version with holes, FullSolution is the completed solution.
make_puzzle(Difficulty, Puzzle, FullSolution) :-        
    empty_grid(NX),
    solve(NX, FullSolution),
    !, % do not backtrack into the grid - if we got a grid, stick with it
    punch_holes(FullSolution, Difficulty, Puzzle).
    


%%%%%%%%%%%%%%%%%%%%%%
%% REPL debug display
%%%%%%%%%%%%%%%%%%%%%%    
write_g(empty_position) :- write("_"), !.
write_g(X) :- write(X), !.

print_grid([]) :- write("\n").
print_grid([A, B, C, D, E, F, G, H, I|XS]) :-
    write_g(A), write_g(B), write_g(C), write(" "),
    write_g(D), write_g(E), write_g(F), write(" "),
    write_g(G), write_g(H), write_g(I), write(" "),
    write("\n"), print_grid(XS).
    
    
    
    