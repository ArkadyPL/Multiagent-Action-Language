initially([heads]).
by_releases(toss,[],[heads]).

initially([alive, \loaded]).
by_causes(load,[],[loaded]).
causes_if(shoot,[\alive],[loaded]).

initially([is_student]).
causes_if(studing,[has_diploma],[is_student]).
causes_if(finding_job,[rich],[has_diploma]).
causes_if(living,[happy],[rich]).

always([thinking]).
initially([not_human]).
causes_if(beeing,[beeing_human],[thinking]).

initially([\open]).
by_causes(insert,[],[open]).
impossible_if(insert,[\hasCard]).


