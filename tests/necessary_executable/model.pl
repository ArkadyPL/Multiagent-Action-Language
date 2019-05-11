% Test case 1 a. - action2 is possible because after action1, result is true
% Test case 1 b. - action2 is not possible because those agents cannot perform given action
by_causes_if(action1, [g1, g2], result, [pi]).
by_causes_if(action2, [g1, g2], another, [result]).


% Test case 2 a. - action8 is possible because after action7, not(sigma) is true
% Test case 2 b. - action9 is not possible because after action7, not(sigma) is true and then after action8, sigma is true
% Test case 2 c. - action10 is not possible because after action8, sigma is true
% Test case 2 d. - action8 is not possible because after action9, sigma is true
by_causes_if(action7, [g7, g8], \sigma, [sigma]).
by_causes_if(action8, [g7, g8], sigma, [\sigma]).
by_causes_if(action9, [g7, g8], sigma, [sigma]).
by_causes_if(action10, [g7, g8], sigma, [\sigma]).


% Test case 3 - action4 is possible because after action3 the following is true: [alpha, beta]
by_causes_if(action3, [g3, g4], beta, [alpha]).
by_causes_if(action4, [g3, g4], other, [alpha, beta]).


% Test case 4 - action6 impossible, because we never achieve delta
by_causes_if(action5, [g5, g6], delta, [psi]).
by_causes_if(action6, [g5, g6], result3, [delta, gamma]).

<<<<<<< Updated upstream
% Test case 5 a. - action_5_1 impossible by g_5_1
% Test case 5 b. - action_5_1 possible by g_5_2
impossible_by(action_5_1, [g_5_1]).
by_causes_if(action_5_1, [g_5_1], delta_5,[psi_5]).
by_causes_if(action_5_1, [g_5_2], delta_5,[psi_5]).
=======


% Test case 4 - action6 impossible, because we never achieve delta
by_causes_if(action5, [g5, g6], delta, [psi]).
by_causes_if(action6, [g5, g6], result3, [delta, gamma]).


% Test case 6. - action_6_1 not necessary_executable by g_6_1
by_releases_if(action_6_1, [g_6_1], delta_6, [psi_6]).
>>>>>>> Stashed changes
