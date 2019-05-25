% Test case 1 a. - should be good
by_causes_if(action1, [g1, g2], result, [pi]).
by_causes_if(action2, [g1, g2], another, [result]).
after(final,[[action1,[g1, g2]],[action2,[g1, g2]]]).

% Test case 1 b. - should fail
by_causes_if(action1_b, [g1_b, g2_b], result_b, [pi_b]).
by_causes_if(action2_b, [g1_b, g2_b], another_b, [other_b]).
after(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]]).

% Test case 1 c. - should fail
by_causes_if(action1_c, [g1_c], result_c, [pi_c]).

% Test case 1 d. - should be good
by_causes_if(a1_d, [g1_d, g2_d], result_d, [pi_d]).
by_causes_if(a2_d, [g1_d, g2_d], another_d, [result_d]).
after(final_d,[[a1_d,[g1_d, g2_d]],[a2_d,[g1_d, g2_d]]]).
always(final_d).







