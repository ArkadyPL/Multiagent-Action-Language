% Test case 1 a. - should be good
by_releases_if(action1, [g1], result, [pi]).
after(result,[[action1,[g1]]]).

% Test case 1 b. - should be good
by_releases_if(action1_b, [g1_b, g2_b], result_b, [pi_b]).
by_causes_if(action2_b, [g1_b, g2_b], another_b, [result_b]).
after(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]]).

% Test case 1 c. - should fail, no after clause
by_releases_if(action1_c, [g1_c, g2_c], result_c, [pi_c]).

% Test case 1 d. - should fail
by_releases_if(action1_d, [g1_d, g2_d], result_d, [pi_d]).
by_causes_if(action2_d, [g1_d, g2_d], another_d, [other_d]).
after(final_d,[[action1_d,[g1_d, g2_d]],[action2_d,[g1_d, g2_d]]]).







