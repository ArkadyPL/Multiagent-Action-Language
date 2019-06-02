:- consult('../../logic.pl'), consult('model.pl'),


write('----------MEAN TESTS ---\n'),
write('Test case 0\n'),
(write('necessary_after([a],[[work,[John,Tom]]])'),
necessary_after([a],[[work,[John,Tom]]]), passed; failed), nl,
(write('possibly_after([a],[[work,[John,Tom]]])'),
possibly_after([a],[[work,[John,Tom]]]), passed; failed), nl,

write('Test case 0\n'),
(write('necessary_after([d,e],[[eat,[Magda,Anna]],[talk,[Irena,Alice]]])'),
necessary_after([d,e],[[eat,[Magda,Anna]],[talk,[Irena,Alice]]]), passed; failed), nl,
(write('possibly_after([d,e],[[eat,[Magda,Anna]],[talk,[Irena,Alice]]])'),
possibly_after([d,e],[[eat,[Magda,Anna]],[talk,[Irena,Alice]]]), passed; failed), nl,

write('Test case 0\n'),
(write('necessary_after([f],[[do,[Milena]]])'),
necessary_after([f],[[do,[Milena]]]), passed; failed), nl,
(write('possibly_after([f],[[do,[Milena]]])'),
possibly_after([f],[[do,[Milena]]]), passed; failed), nl,


write('Test case 0\n'),
(write('necessary_after([~g],[[make,[Iwona]]])'),
necessary_after([\g],[[make,[Iwona]]]), passed; failed), nl,

write('Test case 0\n'),
(write('necessary_after([i],[[swim,[Ula]]])'),
necessary_after([i],[[swim,[Ula]]]), passed; failed), nl,

write('TESTS FINISHED'),nl.

