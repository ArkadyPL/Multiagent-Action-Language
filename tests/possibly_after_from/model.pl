% Test case 0 a. - Should be true - just a query
by_causes_if(pa_0_action, [pa_0_g1, pa_0_g2], [pa_0_result], [pa_0_cond]).

% Test case 1 a. - Should be true - just releases
by_releases_if(pa_1_action1, [pa_1_g1, pa_1_g2], [pa_1_fluent], [pa_1_cond]).

% Test case 2 a. - should be true, action from "causes" cause "pa_9_result" which is required by releases
by_releases_if(pa_9_action2, [pa_9_g1, pa_9_g2], [pa_9_fluent], [pa_9_result]).
by_causes_if(pa_9_action1, [pa_9_g1, pa_9_g2], [pa_9_result], [pa_9_cond]).

% Test case 3 a. - should be false, actions defined BUT 2nd is NOT achieved from 1st
by_releases_if(pa_10_action2, [pa_10_g1, pa_10_g2], [pa_10_fluent], [pa_10_another_result]).
by_causes_if(pa_10_action1, [pa_10_g1, pa_10_g2], [pa_10_something], [pa_10_cond1]).
by_causes_if(pa_10_action2, [pa_10_g1, pa_10_g2], [pa_10_another_result], [pa_10_something_else]).