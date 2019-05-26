:- consult('../../logic.pl'), consult('model.pl'),

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



write('Test case 3\n'),
(write('not(possibly_engaged([x], [swim])) => '),
not(possibly_engaged([x], [swim])), passed; failed), nl,

(write('possibly_engaged([x, y], [swim]) => '),
possibly_engaged([x, y], [swim]), passed; failed), nl,

(write('possibly_engaged([x, y, z], [swim]) => '),
possibly_engaged([x, y, z], [swim]), passed; failed), nl,

write('Test case 4a - should fail, wrong state\n'),
(write('not(possibly_engaged_from([john], [eat],[is_tired])) => '),
not(possibly_engaged_from([john], [eat],[is_tired])), passed; failed), nl,
write('Test case 4b - should be good, from state is correct\n'),
(write('possibly_engaged_from([john], [eat],[is_hungry]) => '),
possibly_engaged_from([john], [eat],[is_hungry]), passed; failed), nl,

write('Test case 5 - should fail, needs always statement\n'),
(write('not(possibly_engaged_from([anna],[talks],[is_bored])) => '),
not(possibly_engaged_from([anna],[talks],[is_bored])), passed; failed), nl,

write('Test case 6 - should be fine, there is always statement \n'),
(write('possibly_engaged_from([george],[goes_to_sleep],[on_monday]) => '),
possibly_engaged_from([george],[goes_to_sleep],[on_monday]), passed; failed), nl,


write('Possibly Engaged - TESTS FINISHED'),nl.
