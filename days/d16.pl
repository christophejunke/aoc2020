% -*- mode: prolog -*-

% finite domains
:- lib(fd).

% load candiate/2 predicates
?- ["../inputs/16-cstr.pl"].

%
% V is unified with a list of variables [V0, ... Vn] related to the
% constraint satisfaction problem.

% S is unified with [C0 : V0, C1 : V1, ...], a list of pairs mapping
% columns to the rule name; S represents the solution.
%
build_constraints(V, S) :-
    % collect all (C,D) pairs knowing candidate(C,D).
    findall((C,D), candidate(C, D), L),
    % build V and S
    build(L, V, S),
    % add the constraint that all variables must have different values
    alldifferent(V).

%% base case of recursion.
build([], [], []).

build([(C,D)|L], [V|LV], [C:V|LS]) :-
    %% associate V with domain D
    V :: D,
    %% recurse
    build(L,LV,LS).

solve(Solutions) :-
    build_constraints(Vars, Solutions),
    %% there is no need to search for a solution by trial and error,
    %% since in our case, constraint propagation directly unifies all
    %% variables. But otherwise, we would uncomment the next line to
    %% try all possible combinations of values for each variables.
    % labeling(Vars),
    true.

%% Expected behaviour:
%%
%% [eclipse 15]: solve(S).
%%
%% S = [2 : arrival_location,
%%      17 : class,
%%      18 : arrival_platform,
%%      0 : row, 16 : arrival_station,
%%      13 : type,
%%      19 : train,
%%      ... : ...,
%%      ...]
