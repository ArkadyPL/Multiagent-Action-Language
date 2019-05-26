:- consult('../../logic.pl'), consult('story.pl'),

write('Story 1 - TESTS STARTED'),nl,

write('Test case 1\n'),
(write('necessary_executable_from([[push,[a,b]]],[\\isRunning]). => '),
necessary_executable_from([[push,[a,b]]],[\isRunning]), passed; failed), nl,

write('Test case 2\n'),
(write('necessary_executable_from([[push,[a,c]]],[\\isRunning]). => '),
necessary_executable_from([[push,[a,c]]],[\isRunning]), passed; failed), nl,

write('Test case 3\n'),
(write('necessary_after_from([isRunning], [[push,[A,B]]], [\\isRunning]). => '),
necessary_after_from([isRunning], [[push,[A,B]]], [\isRunning]), passed; failed), nl,

write('Test case 4\n'),
(write('necessary_after_from([isRunning], [[push,[A,C]]], [\\isRunning]). => '),
necessary_after_from([isRunning], [[push,[A,C]]], [\isRunning]), passed; failed), nl,

write('Test case 5\n'),
(write('necessary_engaged([a], [push]). => '),
necessary_engaged([a], [push]), passed; failed), nl,

write('Test case 6\n'),
(write('necessary_engaged([b], [push]). => '),
possibly_engaged([b], [push]), passed; failed), nl,

write('Test case 7\n'),
(write('necessary_engaged([c], [push]). => '),
possibly_engaged([c], [push]), passed; failed), nl,

write('Story 1 - TESTS FINISHED'),nl.