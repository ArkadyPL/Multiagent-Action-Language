:- consult('../../logic.pl'), consult('model.pl'),

write('Necessary Executable - TESTS STARTED'),nl,



write('Test case 1 a. - action2 is possible because after action1, result is true\n'),
(write('necessary_executable_from([[action1,[g1, g2]],[action2,[g1, g2]]], [pi]). => '),
necessary_executable_from([[action1,[g1, g2]],[action2,[g1, g2]]], [pi]), passed; failed), nl,

write('Test case 1 b. - action2 is not possible because those agents cannot perform given action\n'),
(write('not(necessary_executable_from([[action1,[gX, gY]],[action2,[gX, gY]]], [pi])). => '),
not(necessary_executable_from([[action1,[gX, gY]],[action2,[gX, gY]]], [pi])), passed; failed), nl,



write('Test case 2 a. - action8 is possible because after action7, not(sigma) is true\n'),
(write('necessary_executable_from([[action7, [g7, g8]],[action8, [g7, g8]]], [sigma]). => '),
necessary_executable_from([[action7, [g7, g8]],[action8, [g7, g8]]], [sigma]), passed; failed), nl,

write('Test case 2 b. - action9 is not possible because after action7, not(sigma) is true and then after action8, sigma is true\n'),
(write('necessary_executable_from([[action7, [g7, g8]],[action8, [g7, g8]],[action9, [g7, g8]]], [sigma]). => '),
necessary_executable_from([[action7, [g7, g8]],[action8, [g7, g8]],[action9, [g7, g8]]], [sigma]), passed; failed), nl,

write('Test case 2 c. - action10 is not possible because after action8, sigma is true\n'),
(write('not(necessary_executable_from([[action7, [g7, g8]],[action8, [g7, g8]],[action10, [g7, g8]]], [sigma])). => '),
not(necessary_executable_from([[action7, [g7, g8]],[action8, [g7, g8]],[action10, [g7, g8]]], [sigma])), passed; failed), nl,

write('Test case 2 d. - action8 is not possible because after action9, sigma is true\n'),
(write('not(necessary_executable_from([[action10, [g7, g8]],[action9, [g7, g8]],[action8, [g7, g8]]], [\sigma])). => '),
not(necessary_executable_from([[action10, [g7, g8]],[action9, [g7, g8]],[action8, [g7, g8]]], [\sigma])), passed; failed), nl,



write('Test case 3 - action4 is possible because after action3 the following is true: [alpha, beta]\n'),
(write('necessary_executable_from([[action3,[g3, g4]],[action4,[g3, g4]]], [alpha]). => '),
necessary_executable_from([[action3,[g3, g4]],[action4,[g3, g4]]], [alpha]), passed; failed), nl,



write('Test case 4 - action6 impossible, because we never achieve delta\n'),
(write('not(necessary_executable_from([[action5,[g5, g6]],[action6,[g5, g6]]], [psi])). => '),
not(necessary_executable_from([[action5,[g5, g6]],[action6,[g5, g6]]], [psi])), passed; failed), nl,

write('Test case 5 a. - action_5_1 impossible by g_5_1\n'),
(write('not(necessary_executable_from([[action_5_1,[g_5_1]]],[psi_5])). => '),
not(necessary_executable_from([[action_5_1,[g_5_1]]],[psi_5])), passed; failed), nl,

write('Test case 5 b. - action_5_1 possible by g_5_2\n'),
(write('necessary_executable_from([[action_5_1,[g_5_2]]],[psi_5]). => '),
necessary_executable_from([[action_5_1,[g_5_2]]],[psi_5]), passed; failed), nl,

write('Test case 6. - action_6_1 not necessary_executable by g_6_1\n'),
(write('not(necessary_executable_from([[action_6_1,[g_6_1]]],[psi_6])). => '),
not(necessary_executable_from([[action_6_1,[g_6_1]]],[psi_6])), passed; failed), nl,

write('Test case 7 a. - should be false because 2nd action is not executable after 1st one (becuase it requires initial state)\n'),
(write('not(necessary_executable_from([[nex_7a_action1, [nex_7a_g1, nex_7a_g2]],[nex_7a_action2, [nex_7a_g1, nex_7a_g2]]],[nex_7a_pi])). => '),
not(necessary_executable_from([[nex_7a_action1, [nex_7a_g1, nex_7a_g2]],[nex_7a_action2, [nex_7a_g1, nex_7a_g2]]],[nex_7a_pi])), passed; failed), nl,

write('Necessary Executable - TESTS FINISHED'),nl.
