% Test case 0 - Should be true - just causes
by_causes_if(pa_0_action, [pa_0_g1, pa_0_g2], [pa_0_result], [pa_0_cond]).

% Test case 1 - Should be true - just releases
by_releases_if(pa_1_action1, [pa_1_g1, pa_1_g2], [pa_1_fluent], [pa_1_cond]).

% Test case 2 - should be true, action from "causes" cause "pa_2_result" which is required by releases
by_releases_if(pa_2_action2, [pa_2_g1, pa_2_g2], [pa_2_fluent], [pa_2_result]).
by_causes_if(pa_2_action1, [pa_2_g1, pa_2_g2], [pa_2_result], [pa_2_cond]).

% Test case 3 - should be false, actions defined BUT 2nd is NOT achieved from 1st
by_releases_if(pa_3_action2, [pa_3_g1, pa_3_g2], [pa_3_fluent], [pa_3_another_result]).
by_causes_if(pa_3_action1, [pa_3_g1, pa_3_g2], [pa_3_something], [pa_3_cond1]).
by_causes_if(pa_3_action2, [pa_3_g1, pa_3_g2], [pa_3_another_result], [pa_3_something_else]).

% Test case 4 - Should be true - conditions fulfilled when considering query input + always statement
by_causes_if(pa_4_action, [pa_4_g1, pa_4_g2], [pa_4_fluent], [pa_4_cond1, pa_4_cond2, pa_4_cond3]).
always([pa_4_cond2, pa_4_cond3]).

% Test case 5 - Should be true - "pa_5_fluentX" is always true so it is possible
by_causes_if(pa_5_action, [pa_5_g1, pa_5_g2], [pa_5_fluent], [pa_5_cond1]).
always([pa_5_fluent1, pa_5_fluent2]).
always([pa_5_fluent3, pa_5_fluentX]).
always([pa_5_fluent4, pa_5_fluent5]).

% Test case 6 - Should be true - "pa_6_fluentX" is always true so it is possible
by_releases_if(pa_6_action, [pa_6_g1, pa_6_g2], [pa_6_fluent], [pa_6_cond1]).
always([pa_6_fluent1, pa_6_fluent2]).
always([pa_6_fluent3, pa_6_fluentX]).
always([pa_6_fluent4, pa_6_fluent5]).
