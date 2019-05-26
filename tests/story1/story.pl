initially([\isRunning]).
impossible_if(push, [isRunning]).
impossible_by(push, [d]).
by_causes(push, [a], [\isRunning]).
by_causes(push, [b], [\isRunning]).
by_causes(push, [c], [\isRunning]).
by_causes(push, [b, c], [\isRunning]).
by_causes(push, [a, b], [isRunning]).
by_causes(push, [a, c], [isRunning]).
by_causes(push, [a, b, c], [\isRunning]).