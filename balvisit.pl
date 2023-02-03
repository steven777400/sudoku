:- module(balvisit, [balanced_visitor/2, so_balanced_visitor/2]).
% Balanced Visitor
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).

% A balanced visitor, given a directed graph as a series of node pairs, visits as many nodes
% as possible, but each node exactly once with a single entry and a single exit.
% a limitation is placed that "as many nodes as possible" means a solution is only valid
% if it could not add more nodes to the portion given.
% e.g. its not the literal longest possible solution
% but doesn't leave anything "on the table"
% so, for  A-B, B-A, C-D, D-C, you would have to include both pairs to be a solution.
% it is thus "balanced", in the sense that for any visited node A, there exists a transit _-A and A-_ which balances node A
% and node A will not be "revisited" at any time.
% the path is not by any means constrained to be contiguous, e.g. A-B, D-E, B-C, E-D, C-A would be a valid balanced visit 
% even though A and D have no connection in the selected visit (they may or may not have been connected in the origin graph)
%% balanced_visitor(++Edges, -Visit)
balanced_visitor(Edges, Visit) :-
  once_visitor(Edges, Visit),
  % now we are guaranteed a visit that visits each node at most once
  % however - it may not be balanced
  % e.g. we could have a visit A-B, B-C.  This is balanced in B but not in A nor C.

  % however, it doesn't mean the visit can't be partially salved
  % consider A-B, D-C, B-A.  By remove D-C, we can salvage this visit and make it balanced.
  % In this case, we require simply that for this proposed solution, all the nodes are balanced.
  include({Visit}/[A-B]>>(
    memberchk(_-A, Visit), 
    memberchk(B-_, Visit)
  ), Visit, Visit).  % mandate the inclusion is complete - result is same as input


filter_out(List, Elems, List2) :-
  % can you believe it? member and memberchk will bind unification on partial variables.
  % so it doesn't work without copying - really wild stuff
  exclude({Elems}/[X]>>(
    duplicate_term(Elems, Elems2), 
    memberchk(X, Elems2)
  ), List, List2).

once_visitor([], []). % trivial base case
once_visitor(Edges, [A-B|Visit]) :-
  member(A-B, Edges),
  % once we originate from A and arrive at B, we want no other consider with
  % this start and end point
  filter_out(Edges, [A-_, _-B], REdges),
  % it would be better to remove conceptual duplicates,
  % A-B,B-A and B-A,A-B are the same thing in this sense, and should only count as one option.
  maplist(compare(<, A-B), REdges),
  once_visitor(REdges, Visit).
  

% balanced visitor produces all permutations
% but if we want to select one, say, at random,
% this should only be used for generation, not for checking
% since it removes valid but 
%% so_balanced_visitor(++Edges, --VisitSet)
so_balanced_visitor(Edges, VisitSet) :-
  setof(SVisit, Visit^(
    balanced_visitor(Edges, Visit),
    sort(Visit, SVisit)
  ), VisitSet).

