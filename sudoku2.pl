:- use_module(library(lists)).
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


take(_, 0, []) :- !.
take([A|AX], N, [A|BX]) :-
  N > 0,
  Np #= N - 1,
  take(AX, Np, BX).





% a couple jigsaws
% G2 = grid(0, 0, 0, 0, 1, 1, 2, 2, 2, 0, 0, 1, 1, 1, 1, 1, 2, 2, 0, 0, 3, 1, 4, 2, 2, 2, 5, 0, 3, 3, 1, 4, 4, 4, 2, 5, 3, 3, 3, 3, 4, 5, 5, 5, 5, 6, 3, 4, 4, 4, 7, 5, 5, 8, 6, 3, 7, 7, 4, 7, 7, 5, 8, 6, 6, 6, 7, 7, 7, 8, 8, 8, 6, 6, 6, 6, 7, 8, 8, 8, 8)
% G2 = grid(0, 0, 1, 1, 1, 2, 2, 2, 2, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 0, 0, 0, 4, 1, 5, 2, 2, 3, 0, 3, 3, 4, 1, 5, 5, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 3, 6, 4, 4, 7, 5, 5, 5, 8, 3, 6, 6, 4, 7, 7, 7, 5, 8, 6, 6, 7, 7, 7, 7, 8, 8, 8, 6, 6, 6, 6, 7, 8, 8, 8, 8

jigsaw([0, 0, 1, 1, 1, 2, 2, 2, 2, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 0, 0, 0, 4, 1, 5, 2, 2, 3, 0, 3, 3, 4, 1, 5, 5, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 3, 6, 4, 4, 7, 5, 5, 5, 8, 3, 6, 6, 4, 7, 7, 7, 5, 8, 6, 6, 7, 7, 7, 7, 8, 8, 8, 6, 6, 6, 6, 7, 8, 8, 8, 8]).


rdm_solve(G, S, S2) :-
 square_block_mask(G),
  %jigsaw(G),
  sudoku(G, S),
  copy_term(S, S2),
  random_permutation(S2, SX),
  labeling([ff], SX).

label_next_member(Puzzle, Solution, Idx) :-
  % need to find the min fd_size index in puzzle,
  % then apply the exact value from solution (just labeling is not sufficient)
  maplist(fd_size, Puzzle, PuzzleS),  
  max_list(PuzzleS, MinPFD),  
  nth1(Idx, PuzzleS, MinPFD),
  write(Idx), write(": "), write(MinPFD), write("\n"),
  nth1(Idx, Solution, SolvedValue),
  nth1(Idx, Puzzle, PuzzleValue),
  PuzzleValue #= SolvedValue.


label_to_uniqueness(Puzzle, Solution, LabeledIdxs) :-
  % cannot  use repeat!  reason: choice points backtrack, which erases the labels
    label_next_member(Puzzle, Solution, Idx),
    copy_term(Puzzle, S3),
    (offset(1, label(S3)) -> (label_to_uniqueness(Puzzle, Solution, IdxS), LabeledIdxs = [Idx|IdxS]) ; !).


%%%%% OLD STUFFF


%%%%%%%%%%%%%%%%%%%%%%
%% REPL debug display
%%%%%%%%%%%%%%%%%%%%%%    
write_g(X) :- (ground(X) -> write(X) ; write("_")), !.

print_grid(G) :- G =.. [grid | XS], print_grid(XS).
print_grid([]) :- write("\n").
print_grid([A, B, C, D, E, F, G, H, I|XS]) :-
    write_g(A), write_g(B), write_g(C), write(" "),
    write_g(D), write_g(E), write_g(F), write(" "),
    write_g(G), write_g(H), write_g(I), write(" "),
    write("\n"), print_grid(XS).
    
    
    
