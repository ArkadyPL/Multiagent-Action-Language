:- consult('../../logic.pl'), consult('model.pl'),

write('----------- COIN TOSS TESTS ---\n'),
write('Test case 0\n'),
(write('possibly_after_from([heads],[[toss,[]]],[])'),
possibly_after_from([heads],[[toss,[]]],[]), passed; failed), nl,

write('Test case 1\n'),
(write('possibly_after_from([not heads],[[toss,[]]],[])'),
possibly_after_from([\heads],[[toss,[]]],[]), passed; failed), nl,


write('Test case 2\n'),
(write('not(necessary_after_from([not heads],[[toss,[]]],[]))'),
not(necessary_after_from([\heads],[[toss,[]]],[])), passed; failed), nl,

write('Test case 3\n'),
(write('not(necessary_after_from([heads],[[toss,[]]],[]))'),
not(necessary_after_from([heads],[[toss,[]]],[])), passed; failed), nl,


write('TESTS FINISHED'),nl.
