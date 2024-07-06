:- use_module(main).
:- use_module(library(format)).

:- multifile([argkv:argkv_def/2]).
argkv:argkv_def(house/1, [color,nation,pet,drink,car]).
argkv:argkv_def(house/2, [color,nation,pet,drink,car]).

house(Color, Nation, Pet, Drink, Car) :-
    write(5:[Color,Nation,Pet,Drink,Car]),
    nl.
house(A1, Color, Nation, Pet, Drink, Car) :-
    write(6:[A1,Color,Nation,Pet,Drink,Car]),
    nl.

:- dynamic([t/1]).

t(G) :-
    house([color-green,drink-tea], 123),
    house([]),
    house([nation-X]),
    X = [drink-water,drink-_],
    house(X, 321),
    G = house(X, 122),
    argkv_call(G),
    call(house([pet-snake]), 120),
    call(house, [pet-rat], 121).
