by_causes(action1,[],[f1]).
by_causes(action2,[],[f2]).
initially([\f1,\f2]).

by_causes(a,[x],[f]).
by_causes(b,[y],[g]).
initially([\f,\g]).

by_causes(load,[],[loaded]).
by_causes(fire,[],[\loaded]).
by_causes_if(fire,[],[\alive],[loaded]).
initially([alive]).
after([\alive],[[fire,[Bill]]]).

by_causes(fire1,[],[\loaded1]).
causes_if(fire1,[\alive1],[loaded1]).
initially([alive1]).