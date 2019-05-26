initially([alive, \protected, \up]).
by_causes(lift, [a, b], [up]).
by_causes_if(lift, [a], [\alive], [\protected, up]).
by_causes_if(lift, [b], [\alive], [\protected, up]).
causes_if(protect, [protected], [up]).
impossible_if(lift, [protected, \alive]).
impossible_if(lift, [protected, alive]).
impossible_if(lift, [\protected, \alive]).
impossible_if(protect, [protected, \alive, up]).
impossible_if(protect, [\protected, alive, up]).
impossible_if(protect, [protected, alive, up]).
impossible_if(protect, [protected, \alive, \up]).
impossible_if(protect, [\protected, alive, \up]).
impossible_if(protect, [protected, alive, \up]).
impossible_if(protect, [\protected, \alive, \up]).
