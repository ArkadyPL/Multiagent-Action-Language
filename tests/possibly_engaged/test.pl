passed:- ansi_format([bold,fg(green)], 'Passed', []).
failed:- ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../program.pl'), consult('model.pl'),

write('Possibly Engaged - TESTS STARTED'),nl,


write('Test case 1\n'),
(write('possibly_engaged([a], [push]) => '),
possibly_engaged([a], [push]), passed; failed), nl,

(write('possibly_engaged([b], [push]) => '),
possibly_engaged([b], [push]), passed; failed), nl,

(write('possibly_engaged([a], [run, push]) => '),
possibly_engaged([a], [run, push]), passed; failed), nl,

(write('possibly_engaged([a], [push, run]) => '),
possibly_engaged([b], [push, run]), passed; failed), nl,


write('Test case 2\n'),
(write('possibly_engaged([c], [fly, sing]) => '),
possibly_engaged([c], [fly, sing]), passed; failed), nl,

(write('possibly_engaged([c], [sing, fly]) => '),
possibly_engaged([c], [sing, fly]), passed; failed), nl,

(write('not(possibly_engaged([d], [fly, sing])) => '),
not(possibly_engaged([d], [fly, sing])), passed; failed), nl,

(write('not(possibly_engaged([d], [sing, fly])) => '),
not(possibly_engaged([d], [sing, fly])), passed; failed), nl,


write('Possibly Engaged - TESTS FINISHED'),nl.
