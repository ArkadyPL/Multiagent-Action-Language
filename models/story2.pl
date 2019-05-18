initially(alive).
initially(not(protected)).
initially(not(up)).

by_causes(lift,[a,b],up).
by_causes_if(lift,[a],not(alive),(not(protected),up)).
by_causes_if(lift,[b],not(alive),(not(protected),up)).
causes_if(protect,protected,up).

impossible_if(lift,(protected;not(alive))).
impossible_if(protected,(protected;not(alive);not(up))).

