% Test case 1 a. - should be good
by_causes_if(action1, [g1, g2], result, [pi]).
by_causes_if(action2, [g1, g2], another, [result]).
after(final,[[action1,[g1, g2]],[action2,[g1, g2]]]).

% Test case 1 b. - should fail
by_causes_if(action1_b, [g1_b, g2_b], result_b, [pi_b]).
by_causes_if(action2_b, [g1_b, g2_b], another_b, [other_b]).
after(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]]).

% Test case 1 a. - should fail
by_causes_if(action1_c, [g1_c], result_c, [pi_c]).









