:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly After From - TESTS STARTED'),nl,


write('Test case 0 a. - Should be true - just a query\n'),
(write('possibly_after_from([pa_0_result], [[pa_0_action,[pa_0_g1, pa_0_g2]]], [pa_0_cond]).'),
possibly_after_from([pa_0_result], [[pa_0_action,[pa_0_g1, pa_0_g2]]], [pa_0_cond]), passed; failed), nl,

write('Test case 1 a. - Should be true - just releases\n'),
(write('possibly_after_from([pa_1_fluent], [[pa_1_action1,[pa_1_g1, pa_1_g2]]], [pa_1_cond]).'),
possibly_after_from([pa_1_fluent], [[pa_1_action1,[pa_1_g1, pa_1_g2]]], [pa_1_cond]), passed; failed), nl,

write('Test case 2 a. - should be true, action from "causes" cause "pa_9_result" which is required by releases\n'),
(write('possibly_after_from([pa_9_fluent], [[pa_9_action1,[pa_9_g1, pa_9_g2]], [pa_9_action2,[pa_9_g1, pa_9_g2]]], [pa_9_cond]).'),
possibly_after_from([pa_9_fluent], [[pa_9_action1,[pa_9_g1, pa_9_g2]], [pa_9_action2,[pa_9_g1, pa_9_g2]]], [pa_9_cond]), passed; failed), nl,

write('Test case 3 a. - should be false, actions defined BUT 2nd is NOT achieved from 1st\n'),
(write('not(possibly_after_from([pa_10_fluent], [[pa_10_action1,[pa_10_g1, pa_10_g2]], [pa_10_action2,[pa_10_g1, pa_10_g2]]], [pa_10_cond1])).'),
not(possibly_after_from([pa_10_fluent], [[pa_10_action1,[pa_10_g1, pa_10_g2]], [pa_10_action2,[pa_10_g1, pa_10_g2]]], [pa_10_cond1])), passed; failed), nl,


write('Possibly After From - TESTS FINISHED'),nl.
