passed:- ansi_format([bold,fg(green)], 'Passed', []).
failed:- ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../program.pl'), consult('model.pl'),

write('TESTS STARTED'),nl,


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
(write('possibly_engaged([c], [run, push]) => '),
possibly_engaged([c], [run, push]), passed; failed), nl,

(write('possibly_engaged([c], [push, run]) => '),
possibly_engaged([c], [push, run]), passed; failed), nl,

(write('not(possibly_engaged([d], [run, push])) => '),
not(possibly_engaged([d], [run, push])), passed; failed), nl,

(write('not(possibly_engaged([d], [push, run])) => '),
not(possibly_engaged([d], [push, run])), passed; failed), nl,


write('TESTS FINISHED'),nl.
