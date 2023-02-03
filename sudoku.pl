%% Suduko puzzle and solver
:- use_module(balvisit).
%%% Utility


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
    aggregate_all(count, limit(2, X), 1).

% any(XS) is true if the list XS has one or more elements
any([_|_]).


%%%%%%%%%%%%%%%%%%%%%%
%% Grid data structure
%%%%%%%%%%%%%%%%%%%%%%

% empty_position is a constant atom indicating an empty position
% considered several grid options - such as nested loops or assert'd terms
% ended up with a linear list with the square representation abstracted on top.

% an empty grid is 9*9 = 81 empty positions.

% originally used a list to represent the grid, but read and set were very slow in aggregate
% using a term allows constant access instead of a linked list, more like an array.
empty_grid(Grid) :- 
    replicate(empty_position, 81, G), 
    Grid =.. [grid | G].

% all_XY is, in list of tuple form, all allowed (X, Y) positions in the grid
all_XY(XYS) :-
    bagof((X, Y), (
        between(0, 8, X),
        between(0, 8, Y)
     ), XYS).

% because "is" does not work both ways (we could use constraint programming here actually)
% for now, we'll implement it both directions by hand.
% arg is 1 based
c_argpos(X, Y, Arg) :-
    ground((X, Y)) -> 
        Arg is Y * 9 + X + 1  
    ;    (Y is div(Arg - 1, 9), X is mod(Arg - 1, 9)).

:- dynamic argpos/3.    
set_argpos :-
    all_XY(XYS),
    retractall(argpos(_, _, _)),
    forall(member((X, Y), XYS), (
        Arg is Y * 9 + X + 1,
        assertz(argpos(X, Y, Arg))
    )).
  

% read_grid_element(Grid, X, Y, Element) maps the X and Y into the linear list
% and matches the element there.
read_grid_element(Grid, X, Y, E) :-
    between(0, 8, X), between(0, 8, Y),
    argpos(X, Y, Eid), 
    arg(Eid, Grid, E).
    
% set_grid_element(Grid, X, Y, Element, NewGrid) replaces the grid element at X, Y
% and binds it to new grid.
set_grid_element(Grid, X, Y, E, Grid2) :-
    between(0, 8, X), between(0, 8, Y),
    argpos(X, Y, Eid), 
    duplicate_term(Grid, Grid2),
    nb_setarg(Eid, Grid2, E).
    
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

% the block mask, for the duration of the puzzle, defines the puzzle blocks
% blockmask uses the same linear array notation as the grid, 
% but each element is an identifier for the block
% that is, all args of the same value in blockmask are part of the same block. 
% The identifier itself should be 0 thru 8
% NOTE: Solver will SORT BY block number, so you can use this as a hint
% for performance.  Put blocks to solve first with lower numbers.
:- dynamic blockmask/3.    

% default, think square blocks.
% blocks are 3x3 squares, such as (0,0) - (2,2); (3,0) - (5,2); (3,3) - (5,5); and (6,6) - (8,8)
square_blocks_grid(Grid) :-
    A = [0,0,0,1,1,1,2,2,2],
    B = [3,3,3,4,4,4,5,5,5],
    C = [6,6,6,7,7,7,8,8,8],

    append([A,A,A,B,B,B,C,C,C], G),
    Grid =.. [grid | G].


set_block_mask(Grid) :-
    all_XY(XYS),
    retractall(blockmask(_, _, _)),
    forall(member((X, Y), XYS), (
        read_grid_element(Grid, X, Y, Bid),
        assertz(blockmask(X, Y, Bid))
    )).
  

% block_adjacent(X, Y, Direction) is true if the position X, Y is on the edge of a block
% and the edge is in the direction left, right, top, bottom
% e.g. 1,2 is on the right edge
% 3,3 is on both the top and left edge of its block
block_adjacent(X, Y, Direction) :-
    blockmask(X, Y, Block),
    MX is X - 1, MY is Y - 1, PX is X + 1, PY is Y + 1,
    (
        % general pattern - if the adjacent blockmask identifier does NOT match this blockmask identifyer, its an edge
        % note this will identify the far edges too, e.g. 0,0 will give top and left - since the lack of mask beyond the edge is a non match
        % unclear if this is desired behavior
        \+ blockmask(X, MY, Block), Direction = top 
    ;   \+ blockmask(X, PY, Block), Direction = bottom 
    ;   \+ blockmask(MX, Y, Block), Direction = left 
    ;   \+ blockmask(PX, Y, Block), Direction = right
    ).

% block_value(Grid, X, Y, Value) is true if Value is an element in the block which contains X, Y
% for example, if coordinate (3, 1) is given, then Value binds to all values in the block (3,0) - (5,2)
% excludes empty positions.    
block_value(Grid, X, Y, Val) :-
    % get the block mask value for the position
    blockmask(X, Y, Block),
    % now get all positions with that block mask
    blockmask(BX, BY, Block),
    read_grid_element(Grid, BX, BY, Val),
    Val \= empty_position.

%%%%%%%%%%%%%%%%%%%%%%
%% Jigsaw Blocks generator
%%%%%%%%%%%%%%%%%%%%%%

diamond_blocks_grid(Grid) :-
    G =
    [0,0,0,0,1,2,2,2,2,
     0,0,0,1,1,1,2,2,2,
     0,0,1,1,1,1,1,2,2,
     3,3,3,4,4,4,5,5,5,
     3,3,3,4,4,4,5,5,5,
     3,3,3,4,4,4,5,5,5,
     6,6,7,7,7,7,7,8,8,
     6,6,6,7,7,7,8,8,8,
     6,6,6,6,7,8,8,8,8],
     Grid =.. [grid | G].

manhattan_distance(X1, Y1, X2, Y2, Dist) :-
    Dist is abs(X1 - X2) + abs(Y1 - Y2).

grow_into(Grid, OriginBN, TargetBN, TX, TY) :-
    arg(OEid, Grid, OriginBN),
    argpos(OX, OY, OEid),

    arg(TEid, Grid, TargetBN),
    argpos(TX, TY, TEid),

    % Manhattan Distance between these two locations must be exactly 1
    manhattan_distance(OX, OY, TX, TY, 1).

random_grow_into(Grid, OriginBN, TargetBN, Grid2) :-
    setof((TX, TY), grow_into(Grid, OriginBN, TargetBN, TX, TY), TXYS),
    random_permutation( RTXYS, TXYS),
    member((STX, STY), RTXYS),
    set_grid_element(Grid, STX, STY, OriginBN, Grid2).

grow_select(S) :-
    GrowPair = [
        (0-1), (1-2),         % first row horizontal
        (0-3), (1-4), (2-5), % first-second row vertical
        (3-4), (4-5),         % second row horizontal
        (3-6), (4-7), (5-8), % second-third row vertical
        (6-7), (7-8)          % third row horizontal
    ],
    transpose_pairs(GrowPair, IGP),
    append([GrowPair, IGP], GPS),
    so_balanced_visitor(GPS, VS),
    random_permutation(VS, [S|_]).

jigsaw(Grid, Grid2) :-
    grow_select(GrowPair),
    write(GrowPair), write("\n"),
    foldl([OB-TB, StartGrid, EndGrid] >> (
            random_grow_into(StartGrid, OB, TB, EndGrid)
            ),
        GrowPair, Grid, Grid2),
    validate(Grid2).

% validate ensures that a grid is a valid mask
% that all the values are contigous
validate(Grid) :-
    maplist(validate(Grid), [0, 1, 2, 3, 4, 5, 6, 7, 8]).

validate(Grid, BlockNum) :-
    findall((X, Y), (    
            arg(OEid, Grid, BlockNum),
            argpos(X, Y, OEid)
        ), [X|XS]),
    length(XS, 8),
    contigous([X], XS).


% a list of X, Y positions
% they must be contigous
% so no subgroup is isolated
% in other words, if we flood fill out from the first one
% we must eventually find them all!
contigous(_, []) :- !.
contigous(Accepted, Pending) :-
    partition({Accepted}/[(PX, PY)] >> (
        member((X, Y), Accepted),
        manhattan_distance(X, Y, PX, PY, 1) 
    ), Pending, NewlyAccepted, StillPending),
    any(NewlyAccepted),
    append([Accepted, NewlyAccepted], CAccepted),
    contigous(CAccepted, StillPending).


complexity(Grid, Complexity) :-
    aggregate_all(sum(L), (
        setof(V, col_value(Grid, XY, V), CS), length(CS, LCS),
        setof(V, row_value(Grid, XY, V), RS), length(RS, LRS),
        L is LCS + LRS
    ), Complexity).



%%%%%%%%%%%%%%%%%%%%%%
%% Sudoku rules/solver
%%%%%%%%%%%%%%%%%%%%%%

% value_consumed(Grid, X, Y, Val) is true if a particular Value
% has already been consumed and is not available for the given X, Y
value_consumed(Grid, X, Y, Val) :-
    % note: disjunctions
        col_value(Grid, X, Val)   % a value in the column would consume it for this location.
    ;   row_value(Grid, Y, Val)  % a value in the row would consume it for this location
    ;   block_value(Grid, X, Y, Val).  % a value in the block would consume it for this location

% propose_value(Grid, X, Y, Val) is designed to be used when X, Y is empty_position in the grid.
% binds Val to all possible values, compliant with Sudoku rules/solver
% considering row, column, and block uniqueness given existing values in the Grid.
% note: Values are bound in random order to facilitate puzzle construction, this does not adversely impact solving.
% propose_value can also be queried with all params instantiated to determine if a potential value is "plausible"
% this doesn't mean its right, just that it can fit in the current grid situation.
propose_value(Grid, X, Y, Val) :-            
    exclude(value_consumed(Grid, X, Y), [1, 2, 3, 4, 5, 6, 7, 8, 9], ValidItems),
    random_permutation(ValidItems, RX),
    member(Val, RX).
    

% a wrapper around propose_value, designed to be used in an iterative loop/foldl
% fills in all empty positions with valid values
iterably_assign_proposed_value(Eid, StartGrid, EndGrid) :-
    argpos(X, Y, Eid),
    propose_value(StartGrid, X, Y, Val),  % propose a new value for the position
    set_grid_element(StartGrid, X, Y, Val, EndGrid).  % update the grid with the proposed value
        

count_options(Grid, Eid, N) :-
    argpos(X, Y, Eid),
    aggregate_all(count, propose_value(Grid, X, Y, _), N).

% given a possibly empty or partially solved grid
% complete a solution!
% using randomness, suitable for generating from blank, new puzzles; also suitable for puzzles with multiple solutions
solve(StartGrid, EndGrid)  :-
    % find all empty positions    
    % along with how many options each position has    
    bagof(Cnt-Eid, (arg(Eid, StartGrid, empty_position), count_options(StartGrid, Eid, Cnt)), EidS) -> (    
        % sort to find the position with fewest options                
        keysort(EidS, [MCnt-Eid|_]),
        % fill in that position, if possible        
        iterably_assign_proposed_value(Eid, StartGrid, IG),        
        solve(IG, EndGrid))
    
    % if no empty positions, the solution is complete
    ; EndGrid = StartGrid.



% a wrapper around propose_value, designed to be used in an iterative loop/foldl
% fills in empty positions with UNIQUE valid value
% will only fill in a value if a UNIQUE solution is available, otherwise, it ignores (leaves it blank)
% thus, a single fold pass will likely not fill in all values
iterably_assign_unique_proposed_value(Eid, StartGrid, EndGrid) :-
    (argpos(X, Y, Eid),
     read_grid_element(StartGrid, X, Y, empty_position), % see if the current value at the position is empty
     bagof(Val, propose_value(StartGrid, X, Y, Val), [V]),  % if so, find all proposed values for this location and ensure there is exactly ONE
     set_grid_element(StartGrid, X, Y, V, EndGrid)  % update the grid with the proposed value
    ) ; EndGrid = StartGrid.  % otherwise, if the current value is non-empty, retain the grid unchanged.


% if we believe there is a unique solution, then we can use this predicate.
% it will fail to solve if there is no unique solution
% therefore, it cannot be used generatively
% howevr, it is much faster than solve.
solve_unique_solution(StartGrid, EndGrid) :-
    % find all empty positions by index id
    bagof(Eid, arg(Eid, StartGrid, empty_position), EidS) -> % arg fails if no matching empty_positions
    (
        % if succeeded, means at least 1 empty position, apply the unique assigner
        foldl(iterably_assign_unique_proposed_value, EidS, StartGrid, IntermediateGrid),
        % and see if we made any progress?  If so, we'll continue
        % if not, switch to the generic solver.
        StartGrid \= IntermediateGrid -> solve_unique_solution(IntermediateGrid, EndGrid) 
        
    ) ; (EndGrid = StartGrid).  % if theres no empty position, well then, we are done   

    




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
% UPDATE: in a concurrent attempt, I used clpfd and incremental labeling - this did not go well. 

% punch_holes takes a full[er] grid, and a number of holes to punch, and punches that many holes
punch_holes(Grid, N, HGrid) :- 
    all_XY(XYS),
    random_permutation(XYS, RXYS),  % randomally consider all positions
    punch_holes(Grid, N, HGrid, RXYS).  % punch holes in that order.

punch_holes(Grid, 0, Grid, _) :- !.
punch_holes(Grid, N, HGrid, [(X,Y)|XYS]) :-    
    punch_hole(Grid, X, Y, GridP) ->  % punch the hole if safe; did we safely punch this hole?
    ( 
        NP is N - 1,  % if so, begin the recursive case - one less hole to punch                
        punch_holes(GridP, NP, HGrid, XYS)
    ) ; punch_holes(Grid, N, HGrid, XYS).  % if not, discard this position from consideration and try again.
    

punch_hole(Grid, X, Y, HGrid) :-    % punch the hole if safe
    \+ read_grid_element(Grid, X, Y, empty_position),  % ensure the location is not already empty
    set_grid_element(Grid, X, Y, empty_position, HGrid), % empty the location    
    % unique(solve(HGrid, _)).
    solve_unique_solution(HGrid, _).  % check that exactly one value can go into the location.


% make_puzzle instantiates a Grid with as many holes as specified by Difficulty
% A good rule seems to be Difficulty between 20 and 50.
% this can fail due to getting stuck. 
% Puzzle is the version with holes, FullSolution is the completed solution.
make_puzzle(Difficulty, Puzzle, FullSolution) :-    
    square_blocks_grid(G),   
    set_block_mask(G),  
    empty_grid(NX),
    time(solve(NX, FullSolution)),
    !, % do not backtrack into the grid - if we got a grid, stick with it
    time(punch_holes(FullSolution, Difficulty, Puzzle)).    
    


retry_solve_time(Goal) :-
    repeat, % infinite choice points
        time(call_with_inference_limit(Goal, 5_000_000, R)),
        % if it failed, it won't bind the vars
        % if so, when we fail here, repeat will "move on" to the next choice point
        % otherwise, ! means we stop processing successfully
        (R == inference_limit_exceeded -> fail ; !).
    
make_jigsaw_puzzle(Difficulty, BlockMask, Puzzle, FullSolution) :-
    % some jigsaws are more ... solvable ... than others
    empty_grid(NX),
    square_blocks_grid(G),
    retry_solve_time( ( 
        jigsaw(G, BlockMask), 
        set_block_mask(BlockMask),
        !,
        solve(NX, FullSolution))),
    !,
    time(punch_holes(FullSolution, Difficulty, Puzzle)).


%%%%%%%%%%%%%%%%%%%%%%
%% REPL debug display
%%%%%%%%%%%%%%%%%%%%%%    
write_g(empty_position) :- write("_"), !.
write_g(X) :- write(X), !.

print_grid(G) :- G =.. [grid | XS], print_grid(XS).
print_grid([]) :- write("\n").
print_grid([A, B, C, D, E, F, G, H, I|XS]) :-
    write_g(A), write_g(B), write_g(C), write(" "),
    write_g(D), write_g(E), write_g(F), write(" "),
    write_g(G), write_g(H), write_g(I), write(" "),
    write("\n"), print_grid(XS).
    
    
    
    