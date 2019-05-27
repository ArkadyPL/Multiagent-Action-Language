% Test case 1
by_causes(push, [a], []).
by_causes(push, [b], []).

by_causes(run, [a], []).
by_causes(run, [b], []).

% Test case 2
by_causes(sing, [c], []).
by_causes(sing, [d], []).
by_causes(fly, [c], []).

% Test case 3
by_causes(swim, [x, y], []).

% Test case 4
by_causes_if(eat, [john],[], [is_hungry]).

% Test case 5
by_causes_if(talks, [anna], [],[is_ready]).

% Test case 6
by_causes_if(new_6_goes_to_sleep, [nex_6_george], [], [nex_6_is_tired]).
always([nex_6_some_noise1, nex_6_some_noise2]).
always([nex_6_is_tired, nex_6_some_noise]).
always([nex_6_some_noise3]).
