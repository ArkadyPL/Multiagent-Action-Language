:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly After From - TESTS STARTED'),nl,


write('Test case 0 - Should be true - just causes\n'),
(write('possibly_after_from([pa_0_result], [[pa_0_action,[pa_0_g1, pa_0_g2]]], [pa_0_cond]).'),
possibly_after_from([pa_0_result], [[pa_0_action,[pa_0_g1, pa_0_g2]]], [pa_0_cond]), passed; failed), nl,

write('Test case 1 - Should be true - just releases\n'),
(write('possibly_after_from([pa_1_fluent], [[pa_1_action1,[pa_1_g1, pa_1_g2]]], [pa_1_cond]).'),
possibly_after_from([pa_1_fluent], [[pa_1_action1,[pa_1_g1, pa_1_g2]]], [pa_1_cond]), passed; failed), nl,

write('Test case 2 - should be true, action from "causes" cause "pa_2_result" which is required by releases\n'),
(write('possibly_after_from([pa_2_fluent], [[pa_2_action1,[pa_2_g1, pa_2_g2]], [pa_2_action2,[pa_2_g1, pa_2_g2]]], [pa_2_cond]).'),
possibly_after_from([pa_2_fluent], [[pa_2_action1,[pa_2_g1, pa_2_g2]], [pa_2_action2,[pa_2_g1, pa_2_g2]]], [pa_2_cond]), passed; failed), nl,

write('Test case 3 - should be false, actions defined BUT 2nd is NOT achieved from 1st\n'),
(write('not(possibly_after_from([pa_3_fluent], [[pa_3_action1,[pa_3_g1, pa_3_g2]], [pa_3_action2,[pa_3_g1, pa_3_g2]]], [pa_3_cond1])).'),
not(possibly_after_from([pa_3_fluent], [[pa_3_action1,[pa_3_g1, pa_3_g2]], [pa_3_action2,[pa_3_g1, pa_3_g2]]], [pa_3_cond1])), passed; failed), nl,

write('Test case 4 - should be false, actions defined BUT 2nd is NOT achieved from 1st\n'),
(write('possibly_after_from([pa_4_fluent], [[pa_4_action,[pa_4_g1, pa_4_g2]]], [pa_4_cond1]).'),
possibly_after_from([pa_4_fluent], [[pa_4_action,[pa_4_g1, pa_4_g2]]], [pa_4_cond1]), passed; failed), nl,

write('Test case 5 - Should be true - "pa_5_fluentX" is always true so it is possible\n'),
(write('possibly_after_from([pa_5_fluentX], [[pa_5_action,[pa_5_g1, pa_5_g2]]], [pa_5_cond1]).'),
possibly_after_from([pa_5_fluentX], [[pa_5_action,[pa_5_g1, pa_5_g2]]], [pa_5_cond1]), passed; failed), nl,

write('Test case 6 - Should be true - "pa_6_fluentX" is always true so it is possible\n'),
(write('possibly_after_from([pa_6_fluentX], [[pa_6_action,[pa_6_g1, pa_6_g2]]], [pa_6_cond1]).'),
possibly_after_from([pa_6_fluentX], [[pa_6_action,[pa_6_g1, pa_6_g2]]], [pa_6_cond1]), passed; failed), nl,

write('Possibly After From - TESTS FINISHED'),nl.
