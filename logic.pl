:- dynamic impossible_by/2.
:- dynamic impossible_if/2.
:- dynamic by_causes/3.
:- dynamic causes_if/3.

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Executability queries - TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

necessary_executable_from(Program, InitialState):- true.

necessary_executable(Program):-
	necessary_executable_from(Program, []).



possibly_executable_from(Program, InitialState):- true.

possibly_executable(Program):-
	possibly_executable_from(Program, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Value queries - TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

necessary_after_from(State, Program, InitialState):- true.

necessary_after(State, Program):-
	necessary_after_from(State, Program, []).



possibly_after_from(State, Program, InitialState):- true.

possibly_after(State, Program):-
	possibly_after_from(State, Program, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Engagement queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
necessary_engaged_from(Group, [], _, [Item|[]]):- subset(Item, Group), !.
necessary_engaged_from(_, [], _, [_|_]):- fail.

necessary_engaged(Group, Actions):-
	necessary_engaged_from(Group, Actions, []).	



possibly_engaged_from(Group, [Action|List], State):-
	not(impossible_by_if(Action, Group, State)),
	by_causes_if(Action, X, _, State),
	subset(X, Group),
	possibly_engaged_from(Group, List, State), !.
	
possibly_engaged_from(_, [], _):- !.
	
possibly_engaged(Group, [Action|List]):-
	possibly_engaged_from(Group, [Action|List], []), !.
