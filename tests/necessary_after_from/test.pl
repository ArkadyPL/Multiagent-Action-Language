passed:- nl, ansi_format([bold,fg(green)], 'Passed', []).
failed:- nl, ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Necessary After From - TESTS STARTED'),nl,



write('Test case 1 a.\n'),
(write('necessary_after_from(final,[[action1,[g1, g2]],[action2,[g1, g2]]], [pi]). => '),
necessary_after_from(final,[[action1,[g1, g2]],[action2,[g1, g2]]], [pi]), passed; failed), nl,

write('Test case 1 b.\n'),
(write('not(necessary_after_from(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]], [pi_b])). => '),
not(necessary_after_from(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]], [pi_b])), passed; failed), nl,

write('Test case 1 c.\n'),
(write('not(necessary_after_from(final_c,[[action1_c,[g1_c]]], [pi_c])). => '),
not(necessary_after_from(final_c,[[action1_c,[g1_c]]], [pi_c])), passed; failed), nl,

write('Necessary After From - TESTS FINISHED'),nl.
