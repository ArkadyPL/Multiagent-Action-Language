% Test case 0 a. - Should be true - just a query
by_causes_if(pa_0_action, [pa_0_g1, pa_0_g2], [pa_0_result], [pa_0_cond]).

% Test case 1 a. - Should be false - action not defined
by_releases_if(pa_1_action1, [pa_1_g1, pa_1_g2], [pa_1_fluent], [pa_1_cond]).

% Test case 2 a. - should be true, action defined and has the same condition
by_releases_if(pa_2_action, [pa_2_g1, pa_2_g2], [pa_2_fluent], [pa_2_cond]).
by_causes_if(pa_2_action, [pa_2_g1, pa_2_g2], [pa_2_something], [pa_2_cond]).

% Test case 3 a. - should be false, action defined BUT has different condition - check for releases condition
% Test case 3 b. - should be false, action defined BUT has different condition - check for causes condition
by_releases_if(pa_3_action, [pa_3_g1, pa_3_g2], [pa_3_fluent], [pa_3_cond1]).
by_causes_if(pa_3_action, [pa_3_g1, pa_3_g2], [pa_3_something], [pa_3_cond2]).

% Test case 4 a. - should be true, action defined and has the same condition (but also defined for another condition)
by_releases_if(pa_4_action, [pa_4_g1, pa_4_g2], [pa_4_fluent], [pa_4_cond1]).
by_causes_if(pa_4_action, [pa_4_g1, pa_4_g2], [pa_4_something], [pa_4_cond1]).
by_causes_if(pa_4_action, [pa_4_g1, pa_4_g2], [pa_4_something_else], [pa_4_cond2]).

% Test case 5 a. - should be true, action defined and both require initial state
by_releases_if(pa_5_action, [pa_5_g1, pa_5_g2], [pa_5_fluent], []).
by_causes(pa_5_action, [pa_5_g1, pa_5_g2], [pa_5_something]).

% Test case 6 a. - should be true, action defined and its condition contains releases condition - check for releases condition
% Test case 6 b. - should be true, action defined and its condition contains releases condition - check for causes condition
by_releases_if(pa_6_action, [pa_6_g1, pa_6_g2], [pa_6_fluent], [pa_6_cond1]).
by_causes_if(pa_6_action, [pa_6_g1, pa_6_g2], [pa_6_something], [pa_6_cond1, pa_6_cond2]).

% Test case 7 a. - should be false, action defined BUT its condition is smaller than releases condition - check for releases condition
% Test case 7 b. - should be false, action defined BUT its condition is smaller than releases condition - check for causes condition
by_releases_if(pa_7_action, [pa_7_g1, pa_7_g2], [pa_7_fluent], [pa_7_cond1, pa_7_cond2]).
by_causes_if(pa_7_action, [pa_7_g1, pa_7_g2], [pa_7_something], [pa_7_cond1]).

% Test case 8 a. - should be true, actions defined and 1st action has the same condition and 2nd is achieved from 1st
by_releases_if(pa_8_action2, [pa_8_g1, pa_8_g2], [pa_8_fluent], [pa_8_cond]).
by_causes_if(pa_8_action1, [pa_8_g1, pa_8_g2], [pa_8_result1], [pa_8_cond]).
by_causes_if(pa_8_action2, [pa_8_g1, pa_8_g2], [pa_8_something], [pa_8_result1]).

% Test case 9 a. - should be true, actions defined and 2nd action has the same condition and 2nd is achieved from 1st
by_releases_if(pa_9_action2, [pa_9_g1, pa_9_g2], [pa_9_fluent], [pa_9_result1]).
by_causes_if(pa_9_action1, [pa_9_g1, pa_9_g2], [pa_9_result1], [pa_9_cond]).
by_causes_if(pa_9_action2, [pa_9_g1, pa_9_g2], [pa_9_something], [pa_9_result1]).

% Test case 10 a. - should be false, actions defined and 1st action has the same condition and 2nd is NOT achieved from 1st
by_releases_if(pa_10_action2, [pa_10_g1, pa_10_g2], [pa_10_fluent], [pa_10_cond1]).
by_causes_if(pa_10_action1, [pa_10_g1, pa_10_g2], [pa_10_something], [pa_10_cond1]).
by_causes_if(pa_10_action2, [pa_10_g1, pa_10_g2], [pa_10_another_result], [pa_10_something_else]).