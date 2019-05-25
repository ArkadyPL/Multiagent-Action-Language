:- dynamic impossible_by/2.
:- dynamic impossible_if/2.
:- dynamic by_causes/3.
:- dynamic causes_if/3.
:- dynamic by_releases_if/4.
:- dynamic by_releases/3.
:- dynamic releases_if/3.
:- dynamic always/1.
:- dynamic initially/1.

% Support for tests:
:- multifile impossible_by/2, by_causes_if/4, by_causes/3, after/2, by_releases_if/4, always/1.
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

check_always([Element | List]):-
	(always(Element) ; check_always(List)), !.
check_always([]):- fail.

necessary_executable_from([[Action, Group] | Program], CurrentState):-
	private_necessary_executable_from([[Action, Group] | Program], CurrentState, _), !.

% CurrentState means "initialState" in the first call, and then set of all changes states
private_necessary_executable_from([[Action, Group] | Program], CurrentState, FinalState):-
	by_causes_if(Action, Group, ResultingState, RequiredState),
	(
		(
			not(is_empty(RequiredState)),
			(
				check_always(RequiredState)
					;
				subset(RequiredState, CurrentState)
			)
		)
			;
		(is_empty(RequiredState),is_empty(CurrentState))
	),
	not(private_impossible_by_if(Action, Group, RequiredState)),
	subtract(CurrentState, ResultingState, ListWithoutResultingState),
	negate_list(ResultingState, NotResultingState),
	subtract(ListWithoutResultingState, NotResultingState, ListWithoutNotResultingState),
	append(ListWithoutNotResultingState, ResultingState, NewCurrentState),
	private_necessary_executable_from(Program, NewCurrentState, FinalState), !.

private_necessary_executable_from([], NewCurrentState, FinalState):-
	FinalState = NewCurrentState, !.

necessary_executable(Program):-
	necessary_executable_from(Program, []), !.



% CurrentState means "initialState" in the first call, and then set of all changes states
possibly_executable_from([[Action, Group] | Program], CurrentState).
	% TODO - implement

possibly_executable_from([],_).

possibly_executable(Program):-
	possibly_executable_from(Program, []), !.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Value queries - TODO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

necessary_after_from(State, [[Action, Group] | Program], CurrentState):-
	(
		(
			findall(X, always(X), AlwaysStates),
			subset(State, AlwaysStates)
		)
			;
		(
			private_necessary_executable_from([[Action, Group] | Program], CurrentState, StateAfterProgram),
			(after(State, [[Action, Group] | Program]) ; subset(State, StateAfterProgram))
		)
	), !.

necessary_after(State, Program):-
	necessary_after_from(State, Program, []), !.


possibly_after_from(State, Program, CurrentState):-
	after(State,Program),
	possibly_after_from_main(State, Program, CurrentState).

possibly_after_from_main(State, [[Action, Group] | Program], CurrentState):-
	(by_releases_if(Action, Group, ResultingState, X) ; by_causes_if(Action, Group, ResultingState, X)),
	subset(X, CurrentState),
	not(private_impossible_by_if(Action, Group, X)),
	subtract(CurrentState, ResultingState, ListWithoutResultingState),
	negate_list(ResultingState, NotResultingState),
	subtract(ListWithoutResultingState, NotResultingState, ListWithoutNotResultingState),
	append(ListWithoutNotResultingState, ResultingState, NewCurrentState),
	possibly_after_from_main(State,Program, NewCurrentState), !.

possibly_after_from_main(_,[], _).

possibly_after(State, Program):-
	possibly_after_from(State, Program, []), !.



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
	necessary_engaged_from(Group, Actions, []), !.	


possibly_engaged_from(Group, [Action|List], State):-
	not(impossible_by_if(Action, Group, State)),
	by_causes_if(Action, X, _, State),
	subset(X, Group),
	possibly_engaged_from(Group, List, State), !.
	
possibly_engaged_from(_, [], _):- !.
	
possibly_engaged(Group, [Action|List]):-
	possibly_engaged_from(Group, [Action|List], []), !.
	
	
% Utils
negate_list([Item|List], NegatedList):-
	NegatedList = [\Item, NegatedList],
	negate_list(List, NegatedList).
negate_list([Item|[]], NegatedList):-
	NegatedList = [\Item, NegatedList].

is_empty([]).