:- consult('../../logic.pl'), consult('model.pl'),

write('TESTS STARTED'),nl,

write('Test case 1\n'),
(write('necessary_after_from([f1,f2],[[action1,[actor1]],[action2,[actor2]]],[])'),
necessary_after_from([f1,f2],[[action1,[actor1]],[action2,[actor2]]],[]), passed; failed), nl,

write('Test case 2\n'),
(write('necessary_after_from([f,g],[[a,[x]],[b,[y]]],[])'),
necessary_after_from([f,g],[[a,[x]],[b,[y]]],[]), passed; failed), nl,

write('Test case 3\n'),
(write('necessary_after_from([loaded],[[load,[Bill]]],[])'),
necessary_after_from([loaded],[[load,[Bill]]],[]), passed; failed), nl,

write('Test case 4\n'),
(write('necessary_after_from([\alive],[[fire,[Bill]]],[])'),
necessary_after_from([\alive],[[fire,[Bill]]],[]), passed; failed), nl,

write('Test case 5\n'),
(write('necessary_after_from([alive1],[],[])'),
necessary_after_from([alive1],[],[]), passed; failed), nl,


write('TESTS FINISHED'),nl.
