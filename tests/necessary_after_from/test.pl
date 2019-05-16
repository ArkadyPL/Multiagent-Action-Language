passed:- nl, ansi_format([bold,fg(green)], 'Passed', []).
failed:- nl, ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Necessary After From - TESTS STARTED'),nl,



write('Test case 1 a.\n'),
(write('necessary_after_from(final,[[action1,[g1, g2]],[action2,[g1, g2]]], [pi]). => '),
necessary_after_from(final,[[action1,[g1, g2]],[action2,[g1, g2]]], [pi]), passed; failed), nl,


write('Necessary After From - TESTS FINISHED'),nl.
