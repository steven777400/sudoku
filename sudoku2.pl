:- use_module(library(clpfd)).

listpos(X, Y, Eid) :-
  [X, Y] ins 0..8,
  Eid #= Y * 9 + X + 1.


% read_grid_element(Grid, X, Y, Element) maps the X and Y into the linear list
% and matches the element there.
read_grid_element(Grid, X, Y, E) :-
    listpos(X, Y, Eid), 
    element(Eid, Grid, E).
    

% cannot use findall with constraints. See https://stackoverflow.com/questions/44728306/findall-3-creates-new-unrelated-variables-in-its-resulting-list/44728942#44728942
% and https://stackoverflow.com/questions/74839224/preserve-clpfd-constraint-variable-in-findall
% but we could use them to generate location Ids
% so we need a "constraint safe" predicate that turns multiple location Ids into a list
%read_grid_elements([], _, []).
%read_grid_elements([G|Grid], Eids, Elements) :-
  %maplist([E,Ep]>>(Ep #= E - 1), Eids, EidsP),  
  %read_grid_elements(Grid, EidsP, ElementsP),
%  (member(1, Eids) -> Elements = [G|ElementsP] ; Elements = ElementsP).

read_grid_elements(_, [], []).
read_grid_elements(Grid, [Eid|Eids], [Elem|Elements]) :- 
  element(Eid, Grid, Elem), 
  read_grid_elements(Grid, Eids, Elements).

read_grid_elements_mask([G|Grid], [MaskElement|Mask], MaskElement, [G|Elements]) :-
  read_grid_elements_mask(Grid, Mask, MaskElement, Elements).
read_grid_elements_mask([_|Grid], [X|Mask], MaskElement, Elements) :-
  X #\= MaskElement,
  read_grid_elements_mask(Grid, Mask, MaskElement, Elements).
read_grid_elements_mask(_, [], _, []).

square_block_mask(X) :-
  A = [0,0,0,1,1,1,2,2,2],
  maplist(plus(3), A, B),
  maplist(plus(6), A, C),
  append([A,A,A,B,B,B,C,C,C], X).
  

sudoku(BlockMask, S) :-
  length(S, 81),
  S ins 1..9,    
  maplist({S,BlockMask}/[Id]>> (      
      findall(Eid, (between(0, 8, V), listpos(Id, V, Eid)), RowEids),                 
      read_grid_elements(S, RowEids, RowElements),
      all_distinct(RowElements),

      findall(Eid, (between(0, 8, V), listpos(V, Id, Eid)), ColumnEids),                       
      read_grid_elements(S, ColumnEids, ColumnElements),
      all_distinct(ColumnElements),

      read_grid_elements_mask(S, BlockMask, Id, BlockElements),
      all_distinct(BlockElements)


    ), [0,1,2,3,4,5,6,7,8]
  ).


prelabel(_, 0, _).
prelabel(S, N, [R|REids]) :-
  element(R, S, RElem), 
  label([RElem]),
  Np #= N - 1,
  prelabel(S, Np, REids).


puzzle(S) :-
  square_block_mask(BlockMask),
  sudoku(BlockMask, S),  
  findall(Eid, between(1, 81, Eid), Eids),
  random_permutation(Eids, REids),
  % apply a minimum number of labels to start with to ensure search space tractable
  prelabel(S, 35, REids).
  % TODO continue labeling until solution unique?

  
%  offset(1, (puzzle(S), label(S))). ensure this fails = one solution
% https://eu.swi-prolog.org/pldoc/man?section=solutionsequences