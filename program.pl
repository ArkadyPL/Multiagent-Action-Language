:- dynamic after/2, observable_after/2, initially/1,
	by_causes_if/4, by_causes/3, causes_if/3,
	impossible_by_if/3, impossible_by/2, impossible_if/2.
:- consult('models/story1.pl').

% Make sure, "initially" also propagates to after(Result, []).
after(Result, []):-
	initially(Result), !.

impossible_by_if(Action, Group, []):-
	impossible_by(Action, Group), !.
	
impossible_by_if(Action, [], State):-
	impossible_if(Action, State), !.


by_causes_if(Action, Group, Result, []):-
	by_causes(Action, Group, Result), !.
	
by_causes_if(Action, [], Result, State):-
	causes_if(Action, Result, State), !.


necessary_engaged_from(Group, Actions, State):-
	possibly_engaged_from(Group, Actions, State),
	%TODO: not possible by any other group
	!.

necessary_engaged(Group, Actions):-
	necessary_engaged_from(Group, Actions, []), !.	


possibly_engaged_from(Group, [Action|List], State):-
	not(impossible_by_if(Action, Group, State)),
	by_causes_if(Action, Group, _, State),
	%TODO: check for remaining actions in the list
	!.

possibly_engaged(Group, [Action|List]):-
	possibly_engaged_from(Group, [Action|List], []), !.
