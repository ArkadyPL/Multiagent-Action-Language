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


write('Test case 41\n'),
(write('necessary_after_from([~alive],[[load,[]],[shoot,[]]],[~loaded])'),
necessary_after_from([\alive],[[load,[]],[shoot,[]]],[\loaded]), passed; failed), nl,


write('Test case 4b\n'),
(write('possibly_after_from([~alive],[[load,[]],[shoot,[]]],[~loaded])'),
possibly_after_from([\alive],[[load,[]],[shoot,[]]],[\loaded]), passed; failed), nl,

write('Test case 5a\n'),
(write('necessary_after_from([happy],[[studing,[]],[finding_job,[]],[living,[]]],[])'),
necessary_after_from([happy],[[studing,[]],[finding_job,[]],[living,[]]],[]), passed; failed), nl,

write('Test case 5b\n'),
(write('possibly_after_from([happy],[[studing,[]],[finding_job,[]],[living,[]]],[])'),
possibly_after_from([happy],[[studing,[]],[finding_job,[]],[living,[]]],[]), passed; failed), nl,

write('Test case 6a\n'),
(write('necessary_after_from([beeing_human],[[beeing,[]]],[])'),
necessary_after_from([beeing_human],[[beeing,[]]],[]), passed; failed), nl,

write('Test case 6b\n'),
(write('possibly_after_from([beeing_human],[[beeing,[]]],[])'),
possibly_after_from([beeing_human],[[beeing,[]]],[]), passed; failed), nl,

write('TESTS FINISHED'),nl.
