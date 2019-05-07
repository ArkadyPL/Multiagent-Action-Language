:- dynamic after/2, observable_after/2, initially/1,
	by_causes_if/4, by_causes/3, causes_if/3,
	impossible_by_if/3, impossible_by/2, impossible_if/2.


% Make sure, "initially" also propagates to after(Result, []).
after(Result, []):-
	initially(Result).



impossible_by_if(Action, Group, []):-
	impossible_by(Action, Group), !.
	
impossible_by_if(Action, [], State):-
	impossible_if(Action, State), !.



by_causes_if(Action, Group, Result, []):-
	by_causes(Action, Group, Result).
	
by_causes_if(Action, [], Result, State):-
	causes_if(Action, Result, State).



necessary_engaged_from(Group, [Action|List], State):-
	not(impossible_by_if(Action, Group, State)),
	findall(X, by_causes_if(Action, X, _, State), Engaged),
	necessary_engaged_from(Group, List, State, Engaged), !.

necessary_engaged_from(Group, [Action|List], State, Engaged):-
	not(impossible_by_if(Action, Group, State)),
	findall(X, by_causes_if(Action, X, _, State), NextEngaged),
	findall(X, (member(X, Engaged), member(X, NextEngaged)), EngagedInBoth),
	necessary_engaged_from(Group, List, State, EngagedInBoth), !.

necessary_engaged_from(_, [], _, []):- fail.
necessary_engaged_from(Group, [], _, [Item|[]]):- Group = Item, !.
necessary_engaged_from(_, [], _, [Item|_]):- fail.

necessary_engaged(Group, Actions):-
	necessary_engaged_from(Group, Actions, []).	



possibly_engaged_from(Group, [Action|List], State):-
	not(impossible_by_if(Action, Group, State)),
	by_causes_if(Action, Group, _, State),
	possibly_engaged_from(Group, List, State), !.
	
possibly_engaged_from(_, [], _):- !.
	
possibly_engaged(Group, [Action|List]):-
	possibly_engaged_from(Group, [Action|List], []), !.
