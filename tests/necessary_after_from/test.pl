:- consult('../../logic.pl'), consult('model.pl'),

write('Necessary After From - TESTS STARTED'),nl,


write('Test case 1 a. - should be false because 1nd action is not executable from initial state\n'),
(write('not(necessary_after([na_1a_expected], [[na_1a_action1,[na_1a_g1, na_1a_g2]], [na_1a_action2, [na_1a_g1, na_1a_g2]]])).'),
not(necessary_after([na_1a_expected], [[na_1a_action1,[na_1a_g1, na_1a_g2]], [na_1a_action2, [na_1a_g1, na_1a_g2]]])), passed; failed), nl,

write('Test case 2 a. - should be false because 2nd action is not executable after 1st one\n'),
(write('not(necessary_after([na_2a_expected], [[na_2a_action1,[na_2a_g1, na_2a_g2]], [na_2a_action2, [na_2a_g1, na_2a_g2]]])).'),
not(necessary_after([na_2a_expected], [[na_2a_action1,[na_2a_g1, na_2a_g2]], [na_2a_action2, [na_2a_g1, na_2a_g2]]])), passed; failed), nl,

write('Test case 3 a. - should be true because program is executable from initial state, and it is true that "na_3a_expected" after this program\n'),
(write('necessary_after_from([na_3a_expected], [[na_3a_action1,[na_3a_g1, na_3a_g2]], [na_3a_action2, [na_3a_g1, na_3a_g2]]], []).'),
necessary_after_from([na_3a_expected], [[na_3a_action1,[na_3a_g1, na_3a_g2]], [na_3a_action2, [na_3a_g1, na_3a_g2]]], []), passed; failed), nl,

write('Test case 4 a. - should be true because program is executable from initial state, and it is true that "na_4a_expected" after this program\n'),
(write('necessary_after_from([na_4a_expected], [[na_4a_action1,[na_4a_g1, na_4a_g2]], [na_4a_action2, [na_4a_g1, na_4a_g2]]], []).'),
necessary_after_from([na_4a_expected], [[na_4a_action1,[na_4a_g1, na_4a_g2]], [na_4a_action2, [na_4a_g1, na_4a_g2]]], []), passed; failed), nl,

write('Test case 5 a. - should be false because 2nd action is not executable from requested state\n'),
(write('not(necessary_after_from([na_5a_expected], [[na_5a_action1,[na_5a_g1, na_5a_g2]], [na_5a_action2, [na_5a_g1, na_5a_g2]]], [na_5a_pi])).'),
not(necessary_after_from([na_5a_expected], [[na_5a_action1,[na_5a_g1, na_5a_g2]], [na_5a_action2, [na_5a_g1, na_5a_g2]]], [na_5a_pi])), passed; failed), nl,

write('Test case 6 a. - should be true because both actions are executable from requested state, and it is true that "na_6a_expected" after this program\n'),
(write('necessary_after_from([na_6a_expected], [[na_6a_action1,[na_6a_g1, na_6a_g2]], [na_6a_action2, [na_6a_g1, na_6a_g2]]], [na_6a_pi]).'),
necessary_after_from([na_6a_expected], [[na_6a_action1,[na_6a_g1, na_6a_g2]], [na_6a_action2, [na_6a_g1, na_6a_g2]]], [na_6a_pi]), passed; failed), nl,

write('Test case 7 a. - should be true because both actions are executable from requested state, and it is true that "na_7a_expected" after this program\n'),
(write('necessary_after_from([na_7a_expected], [[na_7a_action1,[na_7a_g1, na_7a_g2]], [na_7a_action2, [na_7a_g1, na_7a_g2]]], [na_7a_pi]).'),
necessary_after_from([na_7a_expected], [[na_7a_action1,[na_7a_g1, na_7a_g2]], [na_7a_action2, [na_7a_g1, na_7a_g2]]], [na_7a_pi]), passed; failed), nl,

write('Test case 8 a. - should be false - trivial, actions undefined\n'),
(write('not(necessary_after([na_8a_expected], [[na_8a_action1,[na_8a_g1, na_8a_g2]], [na_8a_action2, [na_8a_g1, na_8a_g2]]])).'),
not(necessary_after([na_8a_expected], [[na_8a_action1,[na_8a_g1, na_8a_g2]], [na_8a_action2, [na_8a_g1, na_8a_g2]]])), passed; failed), nl,

write('Test case 9 a. - should be true because expected term is always true\n'),
(write('necessary_after_from([na_9a_expected], [[na_9a_action1,[na_9a_g1, na_9a_g2]], [na_9a_action2, [na_9a_g1, na_9a_g2]]], [na_9a_pi]).'),
necessary_after_from([na_9a_expected], [[na_9a_action1,[na_9a_g1, na_9a_g2]], [na_9a_action2, [na_9a_g1, na_9a_g2]]], [na_9a_pi]), passed; failed), nl,

write('Test case 10 a. - should be false because expected term is never true\n'),
(write('not(necessary_after_from([na_10a_expected], [[na_10a_action1,[na_10a_g1, na_10a_g2]], [na_10a_action2, [na_10a_g1, na_10a_g2]]], [na_10a_pi])).'),
not(necessary_after_from([na_10a_expected], [[na_10a_action1,[na_10a_g1, na_10a_g2]], [na_10a_action2, [na_10a_g1, na_10a_g2]]], [na_10a_pi])), passed; failed), nl,

write('Necessary After From - TESTS FINISHED'),nl.
