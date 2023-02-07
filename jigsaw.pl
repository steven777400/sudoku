:- module(jigsaw, [jigsaw/1]).

:- use_module(balvisit).
:- use_module(grid).

%%%%%%%%%%%%%%%%%%%%%%
%% Jigsaw Blocks generator
%%%%%%%%%%%%%%%%%%%%%%

% any(XS) is true if the list XS has one or more elements
any([_|_]).

manhattan_distance(X1-Y1, X2-Y2, Dist) :-
    Dist is abs(X1 - X2) + abs(Y1 - Y2).

grow_into(Grid, OriginBN, TargetBN, TX-TY) :-
    arg(OEid, Grid, OriginBN),
    argpos(OX-OY, OEid),
    arg(TEid, Grid, TargetBN),
    argpos(TX-TY, TEid),

    % Manhattan Distance between these two locations must be exactly 1
    manhattan_distance(OX-OY, TX-TY, 1),

    % it's not a "grow into" if it restores the original layout
    square_blocks_grid(G),
    % it must not be the case that the target id has the origin block number
    % in the original square grid
    % if it were the case, it means the growth is actually restoring the original
    % squares.
    \+ arg(TEid, G, OriginBN).

random_grow_into(Grid, OriginBN, TargetBN, Grid2) :-
    setof(TX-TY, grow_into(Grid, OriginBN, TargetBN, TX-TY), TXYS),
    random_permutation( RTXYS, TXYS),
    member(STX-STY, RTXYS),
    set_grid_element(Grid, STX-STY, OriginBN, Grid2).

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

jigsaw(Grid2) :-
    square_blocks_grid(Grid),
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
    findall(X-Y, (    
            arg(OEid, Grid, BlockNum),
            argpos(X-Y, OEid)
        ), [X|XS]),
    length(XS, 8),
    contigous([X], XS).


% a list of X-Y positions
% they must be contigous
% so no subgroup is isolated
% in other words, if we flood fill out from the first one
% we must eventually find them all!
contigous(_, []) :- !.
contigous(Accepted, Pending) :-
    partition({Accepted}/[PX-PY] >> (
        member(X-Y, Accepted),
        manhattan_distance(X-Y, PX-PY, 1) 
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