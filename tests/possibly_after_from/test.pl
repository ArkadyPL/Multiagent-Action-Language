passed:- nl, ansi_format([bold,fg(green)], 'Passed', []).
failed:- nl, ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly After From - TESTS STARTED'),nl,



write('Test case 1 a.\n'),
(write('possibly_after_from(final,[[action1,[g1, g2]],[action2,[g1, g2]]], [pi]). => '),
possibly_after_from(final,[[action1,[g1, g2]],[action2,[g1, g2]]], [pi]), passed; failed), nl,


write('Possibly After From - TESTS FINISHED'),nl.
