% -*- mode: prolog -*-

% finite domains
:- lib(fd).

?- ["../inputs/16-cstr.pl"].

%% A predicate is identified by both its name and its arity (number of
%% parameters): build_constraints/2 and build_constraints/3 are two
%% different predicates.

build_constraints(V, S) :-
    findall((C,D), candidate(C, D), L),
    build_constraints(L, V, S),
    alldifferent(V).

build_constraints([], [], []).
build_constraints([(C,D)|L], [V|LV], [C:V|LS]) :-
    V :: D,
    build_constraints(L,LV,LS).

solve(Solutions) :-
    build_constraints(Vars, Solutions),
    %% there is no need to search for a solution by trial and error,
    %% since in out case constraint propagation directly unifies all
    %% variables. But otherwise, you would uncomment the next line:
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
%%      15 : arrival_track,
%%      14 : departure_time,
%%      7 : departure_station,
%%      10 : departure_platform,
%%      4 : departure_date,
%%      11 : departure_track,
%%      9 : departure_location,
%%      5 : route,
%%      1 : seat,
%%      6 : wagon,
%%      3 : duration,
%%      ... : ...,
%%      ...]
