:- dynamic after/2.
:- consult('model.pl').

% Make sure, "initially" also propagates to after(Result, []).
after(Result, []):-
	initially(Result).

