after(result, [[action1, [g1, g2]], [action2, [g3, g4]]]).
after(result, [[action1, [g1, g3]], [action2, [g2]]]).
after(beta, [[action1, [g1]]]):- !.


observable_after(result, [[action1, [g1]], [action2, [g2]]]).


initially(beta).


by_causes_if(action1, [g1], result, pi).

by_causes(action1, [g1], result).

causes_if(action1, result, pi).


impossible_by_if(action1, [g1], pi).

impossible_by(action1, [g1]).

impossible_if(action1, pi).
