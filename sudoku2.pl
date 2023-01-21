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


%%% can we make constraint based hole punching?
% rdm_solve(G, S, S2), !,  hole_mask(S, S2, 80, M).
hole_mask(Empty, Solved, NumHoles, Mask) :-
  length(Mask, 81),
  Mask ins 0..1,
  global_cardinality(Mask, [0-NumHoles, 1-_]),
  apply_hole_mask_step(Empty, Solved, Mask),
label(Empty).
  %label(Mask), ground(Empty).
  

% this could be mapped and made non-recursive
apply_hole_mask_step([], [] ,[]).
apply_hole_mask_step([E|Empty], [S|Solved], [M|Mask]) :-
  M #= 1 #==> E #= S,
  apply_hole_mask_step(Empty, Solved, Mask).
  
%% boosted this from docs
% fdset_size(+Set, -Size)
% X in 1..10, Z in 1..10, Z #> X, isground(Z, 1), label([X,Z]).

:- multifile clpfd:run_propagator/2.

isground(X, Z) :-
        clpfd:make_propagator(isground(X, Z), Prop),
        clpfd:init_propagator(X, Prop),
        clpfd:trigger_once(Prop).

clpfd:run_propagator(isground(X, Z), MState) :-
        (   fd_set(X, XS), fdset_size(XS, 1) -> clpfd:kill(MState), Z = 1
        ;   true
        ).

%%%%% HOLE PUNCHING
% rdm_solve(G, S, S2), !, punch_holes(S, S2, 80, H).


punch_holes(Empty, Solved, N, Holes) :- 
  findall(Eid, between(1, 81, Eid), Eids),  
  random_permutation(Eids, REids),
  punch_holes(Empty, Solved, N, [], REids, Holes).  % punch holes in that order.

punch_holes(_, _, 0, OutHoles, _, OutHoles) :- !.
punch_holes(Empty, Solved, N, Holes, [Eid|Eids], OH) :-   
  length(Holes, HN),
  write(HN), write(": "), write(Eid), write("\n"), 
  punch_hole(Empty, Solved, Eid, Holes) ->  % punch the hole if safe; did we safely punch this hole?
  ( 
      Np #= N - 1,  % if so, begin the recursive case - one less hole to punch                
      punch_holes(Empty, Solved, Np, [Eid|Holes], Eids, OH)
  ) ; punch_holes(Empty, Solved, N, Holes, Eids, OH).  % if not, discard this position from consideration and try again.
  

mesh(_, _, []).
mesh([E|EX], [S|SX], Eids) :-
  maplist(plus(-1), Eids, MEids),
  [Eid|REids] = MEids,
  (Eid == 0 -> (
    E #= S,
    mesh(EX, SX, REids)
  ) ; (
    mesh(EX, SX, MEids)
  )).


punch_hole(Empty, Solved, Eid, Holes) :-    % punch the hole if safe
  copy_term(Empty, E2),
  findall(XEid, (
    between(1, 81, XEid),
    XEid \= Eid,
    \+ member(XEid, Holes)), Eids),  
  mesh(E2, Solved, Eids),
  ground(E2).
 % \+ offset(1, label(E2)).



%%%%% OLD STUFFF


whole_puzzle(LabeledS, S, NumSols) :-
  square_block_mask(BlockMask),
  sudoku(BlockMask, S),
  write("a\n"),
  copy_term(S, LabeledS),
  %random_permutation(S, LabeledS),
  findall(Eid, between(1, 81, Eid), Eids),  
  random_permutation(Eids, REids),
  maplist({LabeledS}/[Eid]>>(element(Eid, LabeledS, E), label([E])), REids),
  write("b\n"),
  label(LabeledS),
  write("c\n"),
  punch_holes(LabeledS, S, 30),
  aggregate_all(count, label(S), NumSols).

ground_elem(LabeledS, PuzzleS, N) :-
  write("x\n"),
  element(N, LabeledS, Value),
  element(N, PuzzleS, PuzzleElem),
  PuzzleElem #= Value.

punch_holes(LabeledS, S, N) :-
  findall(Eid, between(1, 81, Eid), Eids),  
  random_permutation(Eids, REids),
  take(REids, N, TEids),
  maplist({LabeledS, S}/[E]>>(ground_elem(LabeledS, S, E)), TEids).
  







prelabel(_, 0, _).
prelabel(S, N, [R|REids]) :-
  element(R, S, RElem), 
  label([RElem]),
  Np #= N - 1,
  prelabel(S, Np, REids).



puzzle(S2, TEids) :-
  square_block_mask(BlockMask),
  sudoku(BlockMask, S),
  findall(Eid, between(1, 81, Eid), Eids),  
  repeat, 
    copy_term(S, S2),
    random_permutation(Eids, REids),
    % apply a minimum number of labels to start with to ensure search space tractable
    take(REids, 35, TEids),
    maplist({S2}/[N, E]>>(element(N, S2, E)), TEids, Elems),
    write(TEids),
    write(S2),
    once(labeling([ff], Elems)),
%    prelabel(S2, 35, REids),
    % TODO continue labeling until solution unique?
    write(Elems),
    write("x\n"),
    copy_term(S2, S3),
    (offset(1, label(S3)) -> fail 
    ; !).

%puzzle(S) :-
%  square_block_mask(BlockMask),
%  sudoku(BlockMask, S),  
%  random_permutation(S, RS),
%  label(RS).
  
%  offset(1, (puzzle(S), label(S))). ensure this fails = one solution
% https://eu.swi-prolog.org/pldoc/man?section=solutionsequences

