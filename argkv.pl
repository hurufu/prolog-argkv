% Inspiered by argnames from Ciao Prolog

:- module(argkv, [
    argkv_def/2,
    argkv_apply_pairs/3,
    argkv_call/1,
    argkv_arrange/3,
    argkv_sublist/2
]).

:- use_module(library(pairs)).

% Comment if using SWI
:- use_module(library(loader)).
:- use_module(library(si)).
:- use_module(library(lists)).
:- use_module(library(debug)).
is_list(L) :- catch(list_si(L), _, false).

%% argkv_def(+PredicateIndicator, +Keys).
%
% Define new record to be used with this library
%
% IMPORTANT: Record definitions should be placed before first usage of a
% predicate being defined.
:- multifile([argkv_def/2]).
:- discontiguous([argkv_def/2]).

argkv_def(house/1, [color,nation,pet,drink,car]).
argkv_def(house/2, [color,nation,pet,drink,car]).

%% argkv_call(+Goal)
%
% Call `Goal` expanded in the runtime with the same rules as during
% `goal_expansion/2`.
argkv_call(Goal) :-
    loader:goal_expansion(Goal, user, XGoal),
%   goal_expansion(Goal, XGoal),
    call(user:XGoal).
%   call(XGoal).

argkv_apply_pairs(Goal, Keys, Pairs) :-
    argkv_arrange(Pairs, Keys, Values),
    apply(Goal, Values).

argkv_arrange(Pairs, Keys, Values) :-
    pairs_keys_values(AllPairs, Keys, Values),
    argkv_sublist(Pairs, AllPairs).

%% argkv_sublist(?A, ?B).
%
% B contains every element of A in any order, repetitions allowed. Non-monotinic.
% TODO: It is useful general-purpose predicate – move to a separate list utils.
argkv_sublist([], _).
argkv_sublist([H|T], X) :-
    memberchk(H, X) ->
        argkv_sublist(T, X);
        pairs_keys_values(X, Expected, _),
        H = K-_ ->
        (
           \+ memberchk(K, Expected),
           throw(error(domain_error(one_of(Expected),K),_))
        )
        ;  throw(error(type_error((-)/2,H),_)).

%% ugoal_expansion(?Goal, -ExpandedGoal).
%
% Given predicate `X/Arity` that has `Specification` defined by `argkv_def/2`.
% Transform `Pairs` to `Values` according to that specification and apply the
% following sequence of expansions when possible:
%
% ```
% X(Pairs, ...) =>
%   argkv_apply_pairs(X(...), Specification, Pairs) =>
%     apply(X(...), Values) =>
%       X(..., Values...).
%     argkv_sublist(Pairs, P), argkv_apply_pairs(X(...), Specification, P) =>
%        argkv_sublist(Pairs, P), apply(X(...), Values) =>
%          argkv_sublist(Pairs, P), X(..., Values...).
% ```
%
% NOTE: If expansion isn't possible (eg. if goal is created dynamically) you
% need to call it via `argkv_call/1`, otherwise it won't function properly.
ugoal_expansion(G, argkv_apply_pairs(X, Keys, Pairs)) :-
    functor(G, Functor, Arity),
    argkv_def(Functor/Arity, Keys),
    G =.. [Functor,Pairs|Rest],
    X =.. [Functor|Rest].
ugoal_expansion(argkv_apply_pairs(Goal, Keys, Pairs), apply(Goal, Values)) :-
    is_list(Pairs),
    argkv_arrange(Pairs, Keys, Values).
ugoal_expansion(
        argkv_apply_pairs(Goal, Keys, Pairs),
        (argkv_sublist(Pairs, P), argkv_apply_pairs(Goal, Keys, P))) :-
    var(Pairs),
    pairs_keys_values(P, Keys, _).
ugoal_expansion(apply(Goal, L), XGoal) :-
    callable(Goal),
    is_list(L),
    Goal =.. [Functor|Args],
    append(Args, L, AllArgs),
    XGoal =.. [Functor|AllArgs].

user:goal_expansion(G, X) :-
    nonvar(G),
    ugoal_expansion(G, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic([example/1]).
example(X) :-
    house([color-green,drink-tea], 123),
    house([]),
    house([nation-X]),
    X = [drink-water,drink-_],
    house(X, 321),
    G = house(X, 122),
    argkv_call(G),
    call(house([pet-snake]), 120),
    call(house, [pet-rat], 121).

test :-
    clause(argkv:example(J), Actual),
    Expected = (
        house(123,green,_,_,tea,_),
        house(_,_,_,_,_),
        house(_,J,_,_,_),
        J = [drink-water,drink-_],
        (
            argkv_sublist(J,[color-O,nation-P,pet-Q,drink-R,car-S]),
            house(321,O,P,Q,R,S)
        ),
        T = house(J,122),
        argkv_call(T),
        call(argkv:'$aux',120),
        call(argkv:'$aux',[pet-rat],121)
    ),
    Actual = Expected.
