passed:- nl, ansi_format([bold,fg(green)], 'Passed', []).
failed:- nl, ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly After From - TESTS STARTED'),nl,



write('Test case 1 a.\n'),
(write('possibly_after_from(result,[[action1,[g1]]],[pi]). => '),
possibly_after_from(result,[[action1,[g1]]],[pi]), passed; failed), nl,

write('Test case 1 b.\n'),
(write('possibly_after_from(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]], [pi_b]). => '),
possibly_after_from(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]], [pi_b]), passed; failed), nl,

write('Test case 1 c.\n'),
(write('not(possibly_after_from(final_b,[[action1_b,[g1_b, g2_b]],[action2_b,[g1_b, g2_b]]], [pi_b])). => '),
not(possibly_after_from(final_c,[[action1_c,[g1_c, g2_c]]], [pi_c])), passed; failed), nl,

write('Test case 1 d.\n'),
(write('not(possibly_after_from(final_d,[[action1_d,[g1_d, g2_d]],[action2_d,[g1_d, g2_d]]], [pi_d])). => '),
not(possibly_after_from(final_d,[[action1_d,[g1_d, g2_d]],[action2_d,[g1_d, g2_d]]], [pi_d])), passed; failed), nl,


write('Possibly After From - TESTS FINISHED'),nl.
