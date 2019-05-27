% Test case 1 a. - should be false because 1st action is not executable from initial state
by_causes_if(na_1a_action1, [na_1a_g1, na_1a_g2], [na_1a_result], [na_1a_pi]).
by_causes(na_1a_action2, [na_1a_g1, na_1a_g2], [na_1a_another]).
after([na_1a_expected], [[na_1a_action1,[na_1a_g1, na_1a_g2]], [na_1a_action2, [na_1a_g1, na_1a_g2]]]).

% Test case 2 a. - should be false because 2nd action is not executable after 1st one
by_causes_if(na_2a_action1, [na_2a_g1, na_2a_g2], [na_2a_result], []).
by_causes_if(na_2a_action2, [na_2a_g1, na_2a_g2], [na_2a_another], [na_2a_pi]).
after([na_2a_expected], [[na_2a_action1,[na_2a_g1, na_2a_g2]], [na_2a_action2, [na_2a_g1, na_2a_g2]]]).

% Test case 3 a. - should be true because program is executable from initial state, and it is true that "na_3a_expected" after this program
by_causes(na_3a_action1, [na_3a_g1, na_3a_g2], [na_3a_result]).
by_causes_if(na_3a_action2, [na_3a_g1, na_3a_g2], [na_3a_another], [na_3a_result]).
after([na_3a_expected], [[na_3a_action1,[na_3a_g1, na_3a_g2]], [na_3a_action2, [na_3a_g1, na_3a_g2]]]).

% Test case 4 a. - should be true because program is executable from initial state, and it is true that "na_4a_expected" after this program
by_causes(na_4a_action1, [na_4a_g1, na_4a_g2], [na_4a_result]).
by_causes_if(na_4a_action2, [na_4a_g1, na_4a_g2], [na_4a_expected], [na_4a_result]).

% Test case 5 a. - should be false because 2nd action is not executable after 1st one
by_causes_if(na_5a_action1, [na_5a_g1, na_5a_g2], [na_5a_result], [na_5a_pi]).
by_causes(na_5a_action2, [na_5a_g1, na_5a_g2], [na_5a_another]). % this is not executable in 'na_5a_result', it is in initial state but it doesn't matter
after([na_5a_expected], [[na_5a_action1,[na_5a_g1, na_5a_g2]], [na_5a_action2, [na_5a_g1, na_5a_g2]]]).

% Test case 6 a. - should be true because both actions are executable from requested state, and it is true that "na_6a_expected" after this program
by_causes_if(na_6a_action1, [na_6a_g1, na_6a_g2], [na_6a_result], [na_6a_pi]).
by_causes_if(na_6a_action2, [na_6a_g1, na_6a_g2], [na_6a_another], [na_6a_result]).
after([na_6a_expected], [[na_6a_action1,[na_6a_g1, na_6a_g2]], [na_6a_action2, [na_6a_g1, na_6a_g2]]]).

% Test case 7 a. - should be true because both actions are executable from requested state, and it is true that "na_7a_expected" after this program
by_causes_if(na_7a_action1, [na_7a_g1, na_7a_g2], [na_7a_result], [na_7a_pi]).
by_causes_if(na_7a_action2, [na_7a_g1, na_7a_g2], [na_7a_expected], [na_7a_result]).

% Test case 8 a. - should be false - trivial, actions undefined
after([na_8a_expected], [[na_8a_action1,[na_8a_g1, na_8a_g2]], [na_8a_action2, [na_8a_g1, na_8a_g2]]]).

% Test case 9 a. - should be true because expected term is always true
by_causes_if(na_9a_action1, [na_9a_g1, na_9a_g2], [na_9a_result], [na_9a_pi]).
by_causes_if(na_9a_action2, [na_9a_g1, na_9a_g2], [na_9a_another], [na_9a_result]).
always([na_9a_just_make_some_noise]).
always([na_9a_expected, na_9a_not_important]).
always([na_9a_just_make_some_noise2]).

% Test case 10 a. - should be false because expected term is never true
by_causes_if(na_10a_action1, [na_10a_g1, na_10a_g2], [na_10a_result], [na_10a_pi]).
by_causes_if(na_10a_action2, [na_10a_g1, na_10a_g2], [na_10a_another], [na_10a_result]).
always([na_10a_just_make_some_noise]).
always([na_10a_not_important, na_10a_other_not_important]).
always([na_10a_just_make_some_noise2]).
