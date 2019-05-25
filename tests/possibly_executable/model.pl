% Test case 1 a. - action_1_1 possibly executable by g_1_1 because of release
by_releases_if(action_1_1, [g_1_1], [delta_1], [psi_1]).

% Test case 1 b. - action_1_2 possibly executable by g_1_2 because of causes
by_causes_if(action_1_2, [g_1_2], [delta_1], [psi_1]).

% Test case 2. - action_2_1 possibly executable by g_2_1 because of impossible
impossible_by(action_2_1, [g_2_1]).
by_releases_if(action_2_1, [g_2_1], [delta_2], [psi_2]).