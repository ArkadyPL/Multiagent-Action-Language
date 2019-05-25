:- dynamic impossible_by/2.
:- dynamic impossible_if/2.
:- dynamic by_causes/3.
:- dynamic causes_if/3.
:- dynamic by_releases_if/4.
:- dynamic by_releases/3.
:- dynamic releases_if/3.


% Support for tests:
:- multifile impossible_by/2, by_causes_if/4, by_causes/3, after/2, by_releases_if/4.
:- style_check(-discontiguous).
passed:- nl, ansi_format([bold,fg(green)], 'Passed', []).
failed:- nl, ansi_format([bold,fg(red)], 'Failed', []).


% Make sure, "initially" also propagates to after(Result, []).
after(Result, []):-
	initially(Result).

private_impossible_by_if(Action, Group, State):-
	impossible_by(Action, Group);
	impossible_if(Action, State);
	impossible_by_if(Action, Group, State).

impossible_by_if(Action, Group, []):-
	impossible_by(Action, Group), !.
	
impossible_by_if(Action, [], State):-
	impossible_if(Action, State), !.



by_causes_if(Action, Group, Result, []):-
	by_causes(Action, Group, Result).
	
by_causes_if(Action, [], Result, State):-
	causes_if(Action, Result, State).


by_releases_if(Action, Group, Result, _):-
	by_releases(Action, Group, Result).

by_releases_if(Action, _, Result, State):-
	releases_if(Action, Result, State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Executability queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CurrentStates means "initialState" in the first call, and then set of all changes states
necessary_executable_from([[Action, Group] | Program], CurrentStates):-
	by_causes_if(Action, Group, ResultingState, X),
	subset(X, CurrentStates),
	not(private_impossible_by_if(Action, Group, X)),
	delete(CurrentStates, ResultingState, ListWithoutResultingState),
	delete(ListWithoutResultingState, \ResultingState, ListWithoutNotResultingState),
	append(ListWithoutNotResultingState, [ResultingState], NewCurrentStates),
	necessary_executable_from(Program, NewCurrentStates).

necessary_executable_from([], _).

necessary_executable(Program):-
	necessary_executable_from(Program, []).



possibly_executable_from([[Action, Group] | Program], CurrentStates):- 
	(by_releases_if(Action, Group, ResultingState, X) ; by_causes_if(Action, Group, ResultingState, X)),
	subset(X, CurrentStates),
	not(private_impossible_by_if(Action, Group, X)),
	delete(CurrentStates, ResultingState, ListWithoutResultingState),
	delete(ListWithoutResultingState, \ResultingState, ListWithoutNotResultingState),
	append(ListWithoutNotResultingState, [ResultingState], NewCurrentStates),
	possibly_executable_from(Program, NewCurrentStates).

possibly_executable_from([],_).

possibly_executable(Program):-
	possibly_executable_from(Program, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Value queries - TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

necessary_after_from(State, Program, CurrentStates):-
	after(State,Program),
	necessary_after_from_main(State, Program, CurrentStates).

necessary_after_from_main(State, [[Action, Group] | Program], CurrentStates):-
	by_causes_if(Action, Group, ResultingState, X),
	subset(X, CurrentStates),
	not(private_impossible_by_if(Action, Group, X)),
	delete(CurrentStates, ResultingState, ListWithoutResultingState),
	delete(ListWithoutResultingState, \ResultingState, ListWithoutNotResultingState),
	append(ListWithoutNotResultingState, [ResultingState], NewCurrentStates),
	necessary_after_from_main(State,Program, NewCurrentStates).
	
necessary_after_from_main(_,[], _).

necessary_after(State, Program):-
	necessary_after_from(State, Program, []).
	
possibly_after_from(State, Program, CurrentStates):-
	after(State,Program),
	possibly_after_from_main(State, Program, CurrentStates).

possibly_after_from_main(State, [[Action, Group] | Program], CurrentStates):-
	(by_releases_if(Action, Group, ResultingState, X) ; by_causes_if(Action, Group, ResultingState, X)),
	subset(X, CurrentStates),
	not(private_impossible_by_if(Action, Group, X)),
	delete(CurrentStates, ResultingState, ListWithoutResultingState),
	delete(ListWithoutResultingState, \ResultingState, ListWithoutNotResultingState),
	append(ListWithoutNotResultingState, [ResultingState], NewCurrentStates),
	possibly_after_from_main(State,Program, NewCurrentStates).
	
possibly_after_from_main(_,[], _).

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
