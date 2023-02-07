:- module(grid, [empty_grid/1, all_XY/1, 
  read_grid_element/3, set_grid_element/4, 
  argpos/2, set_argpos/0,
  col_value/3, row_value/3,
  square_blocks_grid/1]).

%%%%%%%%%%%%%%%%%%%%%%
%% Grid data structure
%%%%%%%%%%%%%%%%%%%%%%

% **** BUILTIN in Tau-Prolog but not in SWI-Prolog    
% replicate(Element, Count, FullList)

replicate(_, 0, []) :- !.
replicate(Elem, Cnt, [Elem|XS]) :-
    Ncnt is Cnt - 1,
    replicate(Elem, Ncnt, XS).


% empty_position is a constant atom indicating an empty position
% considered several grid options - such as nested loops or assert'd terms
% ended up with a linear list with the square representation abstracted on top.

% an empty grid is 9*9 = 81 empty positions.

% originally used a list to represent the grid, but read and set were very slow in aggregate
% using a term allows constant access instead of a linked list, more like an array.
empty_grid(Grid) :- 
    replicate(empty_position, 81, G), 
    Grid =.. [grid | G].

% all_XY is, in list of tuple form, all allowed (X-Y) positions in the grid
all_XY(XYS) :-
    bagof(X-Y, (
        between(0, 8, X),
        between(0, 8, Y)
     ), XYS).

:- dynamic argpos/2.    
set_argpos :-
    all_XY(XYS),
    retractall(argpos(_, _, _)),
    forall(member(X-Y, XYS), (
        Arg is Y * 9 + X + 1,
        assertz(argpos(X-Y, Arg))
    )).
  

% read_grid_element(Grid, X-Y, Element) maps the X and Y into the linear list
% and matches the element there.
read_grid_element(Grid, X-Y, E) :-
    between(0, 8, X), between(0, 8, Y),
    argpos(X-Y, Eid), 
    arg(Eid, Grid, E).
    
% set_grid_element(Grid, X-Y, Element, NewGrid) replaces the grid element at X-Y
% and binds it to new grid.
set_grid_element(Grid, X-Y, E, Grid2) :-
    between(0, 8, X), between(0, 8, Y),
    argpos(X-Y, Eid), 
    duplicate_term(Grid, Grid2),
    nb_setarg(Eid, Grid2, E).
    
% row_value(Grid, Y, Value)  is true if Value is found in row Y of grid.
% excludes empty positions.
row_value(Grid, Y, Val) :-    
    between(0, 8, Id),
    read_grid_element(Grid, Id-Y, Val),
    Val \= empty_position.

% col_value(Grid, X, Value)  is true if Value is found in column X of grid.
% excludes empty positions.    
col_value(Grid, X, Val) :-
    between(0, 8, Id),
    read_grid_element(Grid, X-Id, Val),
    Val \= empty_position.

% default, think square blocks.
% blocks are 3x3 squares, such as (0,0) - (2,2); (3,0) - (5,2); (3,3) - (5,5); and (6,6) - (8,8)
square_blocks_grid(Grid) :-
    A = [0,0,0,1,1,1,2,2,2],
    B = [3,3,3,4,4,4,5,5,5],
    C = [6,6,6,7,7,7,8,8,8],

    append([A,A,A,B,B,B,C,C,C], G),
    Grid =.. [grid | G].

