initially([\brewed]).
initially([\destroyed]).
impossible_if(brew,[brewed_destroyed]).
by_causes(brew, [Bercik, Filemon], [brewed]).
by_releases(brew, [Bercik], [brewed]).
by_releases(brew, [Bercik], [destroyed]).
by_releases(brew, [Filemon], [brewed]).

