passed:- nl, ansi_format([bold,fg(green)], 'Passed', []).
failed:- nl, ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Necessary Engaged - TESTS STARTED'),nl,



write('Test case 1\n'),
(write('not(necessary_engaged([a], [push])) => '),
not(necessary_engaged([a], [push])), passed; failed), nl,

(write('not(necessary_engaged([b], [push])) => '),
not(necessary_engaged([b], [push])), passed; failed), nl,

(write('not(necessary_engaged([a], [run, push])) => '),
not(necessary_engaged([a], [run, push])), passed; failed), nl,

(write('not(necessary_engaged([a], [push, run])) => '),
not(necessary_engaged([b], [push, run])), passed; failed), nl,



write('Test case 2\n'),
(write('necessary_engaged([c], [fly, sing]) => '),
necessary_engaged([c], [fly, sing]), passed; failed), nl,

(write('necessary_engaged([c], [sing, fly]) => '),
necessary_engaged([c], [sing, fly]), passed; failed), nl,

(write('not(necessary_engaged([d], [fly, sing])) => '),
not(necessary_engaged([d], [fly, sing])), passed; failed), nl,

(write('not(necessary_engaged([d], [sing, fly])) => '),
not(necessary_engaged([d], [sing, fly])), passed; failed), nl,



write('Test case 3\n'),
(write('not(necessary_engaged([x], [swim])) => '),
not(necessary_engaged([x], [swim])), passed; failed), nl,

(write('necessary_engaged([x, y], [swim]) => '),
necessary_engaged([x, y], [swim]), passed; failed), nl,

(write('necessary_engaged([x, y, z], [swim]) => '),
necessary_engaged([x, y, z], [swim]), passed; failed), nl,


write('Necessary Engaged - TESTS FINISHED'),nl.
