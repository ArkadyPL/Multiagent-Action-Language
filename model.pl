after(alpha, [[a1, [g1, g2]], [a2, [g3, g4]]]).
after(alpha, [[a1, [g1, g3]], [a2, [g2]]]).
after(beta, [[a1, [g1]]]):- true, !.

observable_after(alpha, [[a1, [g1]], [a2, [g2]]]).

initially(beta).