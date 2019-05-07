initially(not(isRunning)).

impossible_if(push, isRunning).
impossible_by(push, [d]).

by_causes(push, [a], not(isRunning)).
by_causes(push, [b], not(isRunning)).
by_causes(push, [c], not(isRunning)).
by_causes(push, [b,c], not(isRunning)).
by_causes(push, [a,b], isRunning).
by_causes(push, [a,c], isRunning).
by_causes(push, [a,b,c], not(isRunning)).

by_causes(run, [a], not(isRunning)).
%by_causes(run, [b], not(isRunning)).
