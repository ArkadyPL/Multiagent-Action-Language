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
% Executability queries - TODO: fix possibly_executable_from query
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
		(is_empty(RequiredState),initially(CurrentState))		
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


% TODO: create more tests and fix that query
% Remember that action must be defined to be executable!!!

% CurrentState means "initialState" in the first call, and then set of all changes states
possibly_executable_from([[Action, Group] | Program], CurrentState):-
	(by_releases_if(Action, Group, ResultingState, X) ; by_causes_if(Action, Group, ResultingState, X)),
	((not(is_empty(X)),subset(X, CurrentState)) ; (is_empty(X),is_empty(CurrentState))),	
	not(private_impossible_by_if(Action, Group, X)),	
	subtract(CurrentState, ResultingState, ListWithoutResultingState),	
	negate_list(ResultingState, NotResultingState),	
	subtract(ListWithoutResultingState, NotResultingState, ListWithoutNotResultingState),	
	append(ListWithoutNotResultingState, ResultingState, NewCurrentState),	
	possibly_executable_from(Program, NewCurrentState), !.

possibly_executable_from([],_).

possibly_executable(Program):-
	possibly_executable_from(Program, []), !.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Value queries - TODO: finish possibly_after_from implementation
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

possibly_after_from(State, [[Action, Group] | Program], CurrentState):-	
%TODO rozważyć odpalenie possibly_executable_from
	(
		%Bierzemy stan do którego musimy przejść i idziemy do następnej instrukcji. Jeżeli to się nie powiedzie to zwracamy false bo musimy przejść do ResultingState(!)
		by_causes_if(Action, Group, ResultingState, X),
		((not(is_empty(X)),subset(X, CurrentState)) ; (is_empty(X),is_empty(CurrentState))),
		apply_resulting_state(ResultingState, CurrentState, NewCurrentState),
		possibly_after_from(State, Program, NewCurrentState), !
	)
		;
	(	
		% Nie mamy stanu, do którego musimy przejść, generujemy stany usuwając poszczególne fluenty i sprawdzamy
		possibly_after_from_without_causes(State, [[Action, Group] | Program], CurrentState)
	).
	
possibly_after_from_without_causes(State, [[Action, Group] | Program], CurrentState):-
	%Ta reguła generuje nowe stany na podstawie reguł releases i weryfikuje dla nich działanie	
	%by_releases_if(Action, Group, ReleasedFluent, CurrentState), % sprawdzamy, czy możemy w ogóle wykonać releases
	(
		%Sprawdzamy dalszy przebieg dla obecnego stanu		
		by_releases_if(Action, Group, ReleasedFluent, X), %czy akcja w ogóle możliwa
		((not(is_empty(X)),subset(X, CurrentState)) ; (is_empty(X),is_empty(CurrentState))),
		possibly_after_from(State, Program, CurrentState)
	)
		;
	(
		%Generujemy nowy stan usuwając jeden z uwolnionych fluentów i sprawdzamy dalszy przebieg rekurencyjnie		
		by_releases_if(Action, Group, ReleasedFluent, X),
		((not(is_empty(X)),subset(X, CurrentState)) ; (is_empty(X),is_empty(CurrentState))),	
		subtract(CurrentState, [ReleasedFluent], CurrentStateWithoutPositiveReleased),
		subtract(CurrentStateWithoutPositiveReleased, [\ReleasedFluent], CurrentStateWithoutAnyReleased),
		(
			(
				append([ReleasedFluent], CurrentStateWithoutAnyReleased, NewCurrentStateWithPositiveReleased),
				possibly_after_from_without_causes(State, [[Action, Group] | Program], NewCurrentStateWithPositiveReleased)
			)
				;
			(
				append([\ReleasedFluent], CurrentStateWithoutAnyReleased, NewCurrentStateWithNegativeReleased),
				possibly_after_from_without_causes(State, [[Action, Group] | Program], NewCurrentStateWithNegativeReleased)		
			)
		)
	).

possibly_after_from_without_causes(State, [], CurrentState):-
	possibly_after_from(State, [], CurrentState).
	
possibly_after_from(State,[], CurrentState):-
	negate_list(State, NegatedRequiredState),
	intersection(NegatedRequiredState, CurrentState, Common),
	is_empty(Common), !.	

possibly_after(State, Program):-
	possibly_after_from(State, Program, []), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Engagement queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_state(Action,[FirstEngaged | _ ], States):-
	by_causes_if(Action, FirstEngaged, _, RequiredState),
	(subset(RequiredState, States); always(RequiredState)).

necessary_engaged_from(Group, [Action|List], States):-
	not(impossible_by_if(Action, Group, RequiredState)),
	findall(X, by_causes_if(Action, X, _, RequiredState), Engaged),	
	check_state(Action, Engaged,States),
	necessary_engaged_from(Group, List, RequiredState, Engaged), !.

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
	(	by_causes_if(Action, X, _, State);
		by_causes_if(Action, X, _, CheckedState),
		always(CheckedState)
	),
	subset(X, Group),
	possibly_engaged_from(Group, List, State), !.
	
possibly_engaged_from(_, [], _):- !.
	
possibly_engaged(Group, [Action|List]):-
	possibly_engaged_from(Group, [Action|List], []), !.
	
	
% Utils
apply_resulting_state(ResultingState, CurrentState, NewState):-
	subtract(CurrentState, ResultingState, ListWithoutResultingState),
	negate_list(ResultingState, NotResultingState),	
	subtract(ListWithoutResultingState, NotResultingState, ListWithoutNotResultingState),	
	append(ListWithoutNotResultingState, ResultingState, NewState).

negate_list([Item|List], NegatedList):-
	negate_list(List, NegatedSublist),
	append([\Item], NegatedSublist, NegatedList).	
negate_list([Item|[]], NegatedList):-
	NegatedList = [\Item].	
negate_list([],[]).

is_empty([]).