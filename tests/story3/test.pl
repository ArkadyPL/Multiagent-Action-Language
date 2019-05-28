:- consult('../../logic.pl'), consult('story.pl'),
write('---------------------------------------------------------------------------------'),nl,
write('Story 3 - TESTS STARTED'),nl,


(write('necessary_executable_from([[brew,[]]],[\brewed]). => '),
necessary_executable_from([[brew,[]]],[\brewed]), passed; failed), nl,

(write('possibly_engaged([Filemon], [brew]), passed; failed). => '),
possibly_engaged([Filemon], [brew]), passed; failed), nl,

(write('possibly_engaged([Bercik], [brew]), passed; failed). => '),
possibly_engaged([Bercik], [brew]), passed; failed), nl,

(write('possibly_after_from([brewed],[[brew,[Bercik, Filemon]]], [\brewed,not destroyed]). => '),
possibly_after_from([brewed],[[brew,[Bercik, Filemon]]], [\brewed,\destroyed]), passed; failed), nl,

(write('possibly_after_from([brewed],[[brew,[Filemon]]], [\brewed,not destroyed]). => '),
possibly_after_from([brewed],[[brew,[Filemon]]], [\brewed,\destroyed]), passed; failed), nl,

(write('possibly_after_from([brewed],[[brew,[Bercik]]], [\brewed,not destroyed]). => '),
possibly_after_from([brewed],[[brew,[Bercik]]], [\brewed,\destroyed]), passed; failed), nl,

(write('possibly_after_from([destroyed],[[brew,[Bercik]]], [\brewed,not destroyed]). => '),
possibly_after_from([destroyed],[[brew,[Bercik]]], [\brewed,\destroyed]), passed; failed), nl,


write('Story 2 - TESTS FINISHED'),nl,
write('---------------------------------------------------------------------------------'),nl.
