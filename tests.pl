:- write('ALL TESTS STARTED'),nl,
	consult('tests/necessary_executable/test.pl'),
	consult('tests/possibly_executable/test.pl'),
	consult('tests/necessary_engaged/test.pl'),
	consult('tests/possibly_engaged/test.pl'),
	consult('tests/necessary_after_from/test.pl'),
	consult('tests/possibly_after_from/test.pl'),
	write('ALL TESTS FINISHED').
