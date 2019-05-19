% Test case 1 a.
by_causes_if(action1, [g1, g2], result, [pi]).
by_causes_if(action2, [g1, g2], another, [result]).
after(final,[[action1,[g1, g2]],[action2,[g1, g2]]]).
