:- use_module(argkv).

/*
  There are five consecutive houses, each of a different
color and inhabited by men of different nationalities. They each
own a different pet, have a different favorite drink, and drive a
different car.

 1. The Englishman lives in the red house.
 2. The Spaniard owns the dog.
 3. Coffee is drunk in the green house.
 4. The Ukrainian drinks tea.
 5. The green house is immediately to the right of the ivory
    house.
 6. The Porsche driver owns snails.
 7. The Masserati is driven by the man who lives in the yellow
    house.
 8. Milk is drunk in the middle house.
 9. The Norwegian lives in the first house on the left.
10. The man who drives a Saab lives in the house next to the man
    with the fox.
11. The Masserati is driven by the man in the house next to the
    house where the horse is kept.
12. The Honda driver drinks orange juice.
13. The Japanese drives a Jaguar.
14. The Norwegian lives next to the blue house.

The problem is: Who owns the Zebra?  Who drinks water?
*/

:- multifile([argkv:argkv_def/2]).
argkv:argkv_def(house/1, [color,nation,pet,drink,car]).

house(Street, Properties) :- member(house(Properties), Street).

zebra(Owns_zebra, Drinks_water) :-
    length(Street, 5),
    house(Street, [nation-Owns_zebra, pet-zebra]),
    house(Street, [nation-Drinks_water, drink-water]),
    house(Street, [nation-englishman, color-red]),
    house(Street, [nation-spaniard, pet-dog]),
    house(Street, [drink-coffee, color-green]),
    house(Street, [nation-ukrainian, drink-tea]),
    left_right(house([color-ivory]), house([color-green]), Street),
    house(Street, [car-porsche, pet-snails]),
    house(Street, [car-masserati, color-yellow]),
    Street = [_, _, house([drink-milk]), _, _],
    Street = [house([nation-norwegian])|_],
    next_to(Street, house([car-saab]), house([pet-fox])),
    next_to(Street, house([car-masserati]), house([pet,horse])),
    house(Street, [car-honda, drink-orange_juice]),
    house(Street, [nation-japanese, car-jaguar]),
    next_to(Street, house([nation-norwegian]), house([color-blue])).

left_right(L,R,[L,R|_]).
left_right(L,R,[_|T]) :- left_right(L,R,T).

next_to(L,X,Y) :- left_right(L,X,Y).
next_to(L,X,Y) :- left_right(L,Y,X).
