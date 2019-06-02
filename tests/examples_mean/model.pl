after([a],[[work,[John,Tom]]]).
by_causes(work,[],[b]).


after([d,e],[[eat,[Magda,Anna]]]).
after([e],[[talk,[Irena,Alice]]]).
by_causes(eat,[],[z]).
by_causes(talk,[],[z]).

always([f]).
after([\f],[[do,[Milena]]]).


after([\g],[[make,[Iwona]]]).
by_causes(make, [Iwona], [g]).

always([h]).
by_causes_if(swim, [Ula],[i],[h]).