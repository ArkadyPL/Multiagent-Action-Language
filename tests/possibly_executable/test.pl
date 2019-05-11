passed:- ansi_format([bold,fg(green)], 'Passed', []).
failed:- ansi_format([bold,fg(red)], 'Failed', []).

:- consult('../../logic.pl'), consult('model.pl'),

write('Possibly Executable - TESTS STARTED'),nl,

write('Test case 1 a. - action_1_1 possibly executable by g_1_1 because of release\n'),
(write('possibly_executable_from([[action_1_1,[g_1_1]]],[psi_1]). => '),
possibly_executable_from([[action_1_1,[g_1_1]]],[psi_1]), nl, passed; failed), nl,

write('Test case 1 b. - action_1_2 possibly executable by g_1_2 because of causes\n'),
(write('possibly_executable_from([[action_1_2,[g_1_2]]],[psi_1]). => '),
possibly_executable_from([[action_1_2,[g_1_2]]],[psi_1]), nl, passed; failed), nl,

write('Test case 2. - action_2_1 possibly executable by g_2_1 because of impossible\n'),
(write('not(possibly_executable_from([[action_2_1,[g_2_1]]],[psi_2])). => '),
not(possibly_executable_from([[action_2_1,[g_2_1]]],[psi_2])), nl, passed; failed), nl,

write('Possibly Executable - TESTS FINISHED'),nl.