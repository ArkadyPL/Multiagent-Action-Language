passed:- ansi_format([bold,fg(green)], 'Passed', []).
failed:- ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly Engaged - TESTS STARTED'),nl,



write('Test case 1\n'),
(write('possibly_engaged([a], [push]) => '),
possibly_engaged([a], [push]), nl, passed; failed), nl,

(write('possibly_engaged([b], [push]) => '),
possibly_engaged([b], [push]), nl, passed; failed), nl,

(write('possibly_engaged([a], [run, push]) => '),
possibly_engaged([a], [run, push]), nl, passed; failed), nl,

(write('possibly_engaged([a], [push, run]) => '),
possibly_engaged([b], [push, run]), nl, passed; failed), nl,



write('Test case 2\n'),
(write('possibly_engaged([c], [fly, sing]) => '),
possibly_engaged([c], [fly, sing]), nl, passed; failed), nl,

(write('possibly_engaged([c], [sing, fly]) => '),
possibly_engaged([c], [sing, fly]), nl, passed; failed), nl,

(write('not(possibly_engaged([d], [fly, sing])) => '),
not(possibly_engaged([d], [fly, sing])), nl, passed; failed), nl,

(write('not(possibly_engaged([d], [sing, fly])) => '),
not(possibly_engaged([d], [sing, fly])), nl, passed; failed), nl,



write('Test case 3\n'),
(write('not(possibly_engaged([x], [swim])) => '),
not(possibly_engaged([x], [swim])), nl, passed; failed), nl,

(write('possibly_engaged([x, y], [swim]) => '),
possibly_engaged([x, y], [swim]), nl, passed; failed), nl,

(write('possibly_engaged([x, y, z], [swim]) => '),
possibly_engaged([x, y, z], [swim]), nl, passed; failed), nl,


write('Possibly Engaged - TESTS FINISHED'),nl.
