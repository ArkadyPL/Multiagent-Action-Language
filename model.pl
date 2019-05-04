after(alpha, [[action1, [g1, g2]], [action2, [g3, g4]]]).
after(alpha, [[action1, [g1, g3]], [action2, [g2]]]).
after(beta, [[action1, [g1]]]):- true, !.


observable_after(alpha, [[action1, [g1]], [action2, [g2]]]).


initially(beta).


causes_by_if(action1, [g1], alpha, pi).

causes_by(action1, [g1], alpha).

causes_if(action1, alpha, pi).


impossible_by_if(action1, [g1], pi).

impossible_if(action1, pi).