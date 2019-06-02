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

necessary_executable_from([[Action, Group] | Program], CurrentState):-
	private_necessary_executable_from([[Action, Group] | Program], CurrentState, _), !.

% CurrentState means "initialState" in the first call, and then set of all changes states
private_necessary_executable_from([[Action, Group] | Program], CurrentState, FinalState):-
	(	
		(by_causes_if(Action, G, ResultingState, RequiredState);by_releases_if(Action, G, ResultingState, RequiredState)),
		(	
			subset(G,Group),			
			(		
				not(is_empty(RequiredState)),
				(				
					is_always(RequiredState)
						;
					subset(RequiredState, CurrentState)
				)
			)
			;				
			initially(Y),
			(is_empty(RequiredState), subset(CurrentState,Y))		
			;				
			(is_empty(RequiredState),is_empty(CurrentState))
		)
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
necessary_after_from(State, Program, CurrentState):-	
	(	
		(after(State,ResultingProgram), 		
		subset(ResultingProgram,Program),
		are_necessary_connected(Program,CurrentState))	
	;
		(private_necessary_after_from(State,Program,CurrentState))	
	).


are_necessary_connected([[Action, Group] | Program], CurrentState):-
	(
		(				
			by_causes_if(Action,G,ResultingState, S),		
			subset(S,CurrentState),				
			subset(G,Group)
		)
	;
		(	
			by_causes(Action,G,ResultingState),	
			subset(G,Group)
		)
	),		
	are_necessary_connected(Program, ResultingState).

are_necessary_connected([],_):-
	true.
	
private_necessary_after_from( State, [[Action, Group] | Program], CurrentState):-
	(
		(is_always(State))		
		;
		(
			(
				by_causes_if(Action,G,ResultingState, S),
				(subset(S,CurrentState);initially(S);always(S)),			
				subset(G,Group),								
				(	
					(
					subset(ResultingState,State),	
					subtract(State,ResultingState,SetWithoutState),					
					private_necessary_after_from(SetWithoutState, Program, ResultingState)
					)
				;
					(
					private_necessary_after_from(State, Program, ResultingState)
					)
				)
			)
				
			;
			
			(
				by_causes(Action,G,ResultingState),						
				subset(G,Group),	
				(	
					(subset(ResultingState,State),					
					subtract(State,ResultingState,SetWithoutState),					
					private_necessary_after_from(SetWithoutState, Program, ResultingState))
				;
					(private_necessary_after_from(State, Program, ResultingState))
				)
			)
			
		)
	), !.
	

private_necessary_after_from([], [],_):-
	true, !.
	
private_necessary_after_from(State, [] ,_):-
	initially(State).

necessary_after(State, Program):-
	necessary_after_from(State, Program, []), !.

possibly_after_from(State, [[Action, Group] | Program], CurrentState):-	
	is_always(State)
	;
	(apply_alwayses(CurrentState, FullCurrentState) ; true),	
	(
		(
			%Bierzemy stan do którego musimy przejść i idziemy do następnej instrukcji. Jeżeli to się nie powiedzie to zwracamy false bo musimy przejść do ResultingState(!)
			by_causes_if(Action, Group, ResultingState, X),
			((not(is_empty(X)),subset(X, FullCurrentState)) ; (is_empty(X),is_empty(FullCurrentState))),
			apply_resulting_state(ResultingState, FullCurrentState, NewCurrentState),
			possibly_after_from(State, Program, NewCurrentState), !
		)
			;
		(	
			% Nie mamy stanu, do którego musimy przejść, generujemy stany usuwając poszczególne fluenty i sprawdzamy
			possibly_after_from_without_causes(State, [[Action, Group] | Program], FullCurrentState)
		)
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
	(subset(RequiredState, States); is_always(RequiredState)).

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
	(	
		by_causes_if(Action, X, _, State)
		;
		by_causes_if(Action, X, _, CheckedState),
		always(CheckedState)
		;
		by_causes_if(Action, X, _, CheckedState),		
		initially(Y),
		subset(CheckedState,Y)
		;
		causes_if(Action, _, CheckedState),	
		subset(State,CheckedState)
	),
	subset(X, Group),
	possibly_engaged_from(Group, List, State), !.
	
possibly_engaged_from(_, [], _):- !.
	
possibly_engaged(Group, [Action|List]):-
	possibly_engaged_from(Group, [Action|List], []), !.
	
	
% Utils
is_always(State):-
	findall(X, always(X), AlwaysList),
	is_subset_of_any_always(State, AlwaysList).

is_subset_of_any_always(State, [Always | AlwaysList]):-
	subset(State, Always);
	is_subset_of_any_always(State, AlwaysList).
is_subset_of_any_always(_, [[] | []]):- fail.

apply_alwayses(CurrentState, NewState):-
	not(always(X));
	(
		not(subset([X],CurrentState)),
		append([X], CurrentState, NewStateWithX),
		apply_alwayses(NewStateWithX, NewState)
	).

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