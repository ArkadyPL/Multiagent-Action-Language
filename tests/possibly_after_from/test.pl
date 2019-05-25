:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly After From - TESTS STARTED'),nl,


write('Test case 1 a. - Should be false - action not defined\n'),
(write('not(possibly_after_from(pa_1_fluent, [[pa_1_action1,[pa_1_g1, pa_1_g2]]], [pa_1a_pi])).'),
not(possibly_after_from(pa_1_fluent, [[pa_1_action1,[pa_1_g1, pa_1_g2]]], [pa_1_pi])), passed; failed), nl,

write('Test case 2 a. - should be true, action defined and has the same condition\n'),
(write('possibly_after_from(pa_2_fluent, [[pa_2_action,[pa_2_g1, pa_2_g2]]], [pa_2_pi]).'),
possibly_after_from(pa_2_fluent, [[pa_2_action,[pa_2_g1, pa_2_g2]]], [pa_2_pi]), passed; failed), nl,

write('Test case 3 a. - should be false, action defined BUT has different condition\n'),
(write('not(possibly_after_from(pa_3_fluent, [[pa_3_action, [pa_3_g1, pa_3_g2]]], [pa_3_cond1])).'),
not(possibly_after_from(pa_3_fluent, [[pa_3_action, [pa_3_g1, pa_3_g2]]], [pa_3_cond1])), passed; failed), nl,

write('Test case 3 b. - should be false, action defined BUT has different condition\n'),
(write('not(possibly_after_from(pa_3_fluent, [[pa_3_action, [pa_3_g1, pa_3_g2]]], [pa_3_cond2])).'),
not(possibly_after_from(pa_3_fluent, [[pa_3_action, [pa_3_g1, pa_3_g2]]], [pa_3_cond2])), passed; failed), nl,

write('Test case 4 a. - should be false, action defined and has the same condition (but also defined for another condition)\n'),
(write('possibly_after_from(pa_4_fluent, [[pa_4_action,[pa_4_g1, pa_4_g2]]], [pa_4_cond1]).'),
possibly_after_from(pa_4_fluent, [[pa_4_action,[pa_4_g1, pa_4_g2]]], [pa_4_cond1]), passed; failed), nl,

write('Test case 5 a. - should be true, action defined and both require initial state\n'),
(write('possibly_after(pa_5_fluent, [[pa_5_action,[pa_5_g1, pa_5_g2]]]).'),
possibly_after(pa_5_fluent, [[pa_5_action,[pa_5_g1, pa_5_g2]]]), passed; failed), nl,

write('Test case 6 a. - should be false, action defined and its condition contains releases condition - check for releases condition\n'),
(write('not(possibly_after_from(pa_6_fluent, [[pa_6_action,[pa_6_g1, pa_6_g2]]], [pa_6_cond1])).'),
not(possibly_after_from(pa_6_fluent, [[pa_6_action,[pa_6_g1, pa_6_g2]]], [pa_6_cond1])), passed; failed), nl,

write('Test case 6 b. - should be true, action defined and its condition contains releases condition - check for releases condition\n'),
(write('possibly_after_from(pa_6_fluent, [[pa_6_action,[pa_6_g1, pa_6_g2]]], [pa_6_cond1, pa_6_cond2]).'),
possibly_after_from(pa_6_fluent, [[pa_6_action,[pa_6_g1, pa_6_g2]]], [pa_6_cond1, pa_6_cond2]), passed; failed), nl,

write('Test case 7 a. - should be true, action defined BUT its condition is smaller than releases condition - check for releases condition\n'),
(write('possibly_after_from(pa_7_fluent, [[pa_7_action,[pa_7_g1, pa_7_g2]]], [pa_7_cond1]).'),
possibly_after_from(pa_7_fluent, [[pa_7_action,[pa_7_g1, pa_7_g2]]], [pa_7_cond1, pa_7_cond2]), passed; failed), nl,

write('Test case 7 b. - should be false, action defined BUT its condition is smaller than releases condition - check for causes condition\n'),
(write('not(possibly_after_from(pa_7_fluent, [[pa_7_action,[pa_7_g1, pa_7_g2]]], [pa_7_cond1])).'),
not(possibly_after_from(pa_7_fluent, [[pa_7_action,[pa_7_g1, pa_7_g2]]], [pa_7_cond1])), passed; failed), nl,

write('Test case 8 a. - should be true, actions defined and 1st action has the same condition and 2nd is achieved from 1st\n'),
(write('possibly_after_from(pa_8_fluent, [[pa_8_action1,[pa_8_g1, pa_8_g2]], [pa_8_action2,[pa_8_g1, pa_8_g2]]], [pa_8_cond]).'),
possibly_after_from(pa_8_fluent, [[pa_8_action1,[pa_8_g1, pa_8_g2]], [pa_8_action2,[pa_8_g1, pa_8_g2]]], [pa_8_cond]), passed; failed), nl,

write('Test case 9 a. - should be true, actions defined and 2nd action has the same condition and 2nd is achieved from 1st\n'),
(write('possibly_after_from(pa_9_fluent, [[pa_9_action1,[pa_9_g1, pa_9_g2]], [pa_9_action2,[pa_9_g1, pa_9_g2]]], [pa_9_cond]).'),
possibly_after_from(pa_9_fluent, [[pa_9_action1,[pa_9_g1, pa_9_g2]], [pa_9_action2,[pa_9_g1, pa_9_g2]]], [pa_9_cond]), passed; failed), nl,

write('Test case 10 a. - should be false, actions defined and 1st action has the same condition and 2nd is NOT achieved from 1st\n'),
(write('not(possibly_after_from(pa_10_fluent, [[pa_10_action1,[pa_10_g1, pa_10_g2]], [pa_10_action2,[pa_10_g1, pa_10_g2]]], [pa_10_cond1]))).'),
not(possibly_after_from(pa_10_fluent, [[pa_10_action1,[pa_10_g1, pa_10_g2]], [pa_10_action2,[pa_10_g1, pa_10_g2]]], [pa_10_cond1])), passed; failed), nl,


write('Possibly After From - TESTS FINISHED'),nl.
