passed:- ansi_format([bold,fg(green)], 'Passed', []).
failed:- ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Necessary Engaged - TESTS STARTED'),nl,



write('Test case 1\n'),
(write('not(necessary_engaged([a], [push])) => '),
not(necessary_engaged([a], [push])), nl, passed; failed), nl,

(write('not(necessary_engaged([b], [push])) => '),
not(necessary_engaged([b], [push])), nl, passed; failed), nl,

(write('not(necessary_engaged([a], [run, push])) => '),
not(necessary_engaged([a], [run, push])), nl, passed; failed), nl,

(write('not(necessary_engaged([a], [push, run])) => '),
not(necessary_engaged([b], [push, run])), nl, passed; failed), nl,



write('Test case 2\n'),
(write('necessary_engaged([c], [fly, sing]) => '),
necessary_engaged([c], [fly, sing]), nl, passed; failed), nl,

(write('necessary_engaged([c], [sing, fly]) => '),
necessary_engaged([c], [sing, fly]), nl, passed; failed), nl,

(write('not(necessary_engaged([d], [fly, sing])) => '),
not(necessary_engaged([d], [fly, sing])), nl, passed; failed), nl,

(write('not(necessary_engaged([d], [sing, fly])) => '),
not(necessary_engaged([d], [sing, fly])), nl, passed; failed), nl,



write('Test case 3\n'),
(write('not(necessary_engaged([x], [swim])) => '),
not(necessary_engaged([x], [swim])), nl, passed; failed), nl,

(write('necessary_engaged([x, y], [swim]) => '),
necessary_engaged([x, y], [swim]), nl, passed; failed), nl,

(write('necessary_engaged([x, y, z], [swim]) => '),
necessary_engaged([x, y, z], [swim]), nl, passed; failed), nl,


write('Necessary Engaged - TESTS FINISHED'),nl.
