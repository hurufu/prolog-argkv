:- module(ext_list, [contained/2]).

% Every element of A is in B.
contained([], _, true).
contained([H|T], L, TF) :-
    memberchk(H, L) -> (TF = true, contained(T, L)); TF = false(H).
