:- consult('../../logic.pl'), consult('story.pl'),
write('---------------------------------------------------------------------------------'),nl,
write('Story 2 - TESTS STARTED'),nl,

write('Test case 1 - should be true \n'),
(write('necessary_executable_from([[lift,[a,b]]],[alive]). => '),
necessary_executable_from([[lift,[a,b]]],[alive]), passed; failed), nl,

write('Test case 2 - should be true \n'),
(write('necessary_executable_from([[protect,[a]]],[up, alive]). => '),
necessary_executable_from([[protect,[a]]],[up, alive]), passed; failed), nl,

write('Test case 3 - should be true \n'),
(write('necessary_executable_from([[protect,[b]]],[up, alive]). => '),
necessary_executable_from([[protect,[b]]],[up, alive]), passed; failed), nl,

write('Test case 4 - should be true \n'),
(write('necessary_after_from([up], [[lift,[a,b]]], [alive]). => '),
necessary_after_from([up], [[lift,[a,b]]], [alive]), passed; failed), nl,

write('Test case 5 - should be true \n'),
(write('necessary_after_from([protected], [[protect,[a]]], [up]). => '),
necessary_after_from([protected], [[protect,[a]]], [up]), passed; failed), nl,

write('Test case 6 - should be true \n'),
(write('necessary_after_from([protected], [[protect,[b]]], [up]). => '),
necessary_after_from([protected], [[protect,[b]]], [up]), passed; failed), nl,

write('Test case 7 - should be true \n'),
(write('possibly_engaged([a], [protect]). => '),
possibly_engaged([a], [protect]), passed; failed), nl,

write('Test case 8 - should be true \n'),
(write('possibly_engaged([b], [protect]). => '),
possibly_engaged([b], [protect]), passed; failed), nl,

write('Story 2 - TESTS FINISHED'),nl,
write('---------------------------------------------------------------------------------'),nl.
