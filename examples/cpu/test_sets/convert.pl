
:- lib(os_lib).
:- lib(mtx).
:- lib( portray_clauses/2 ).

:- debug(convert).

convert :-
	os_file( Os ),
	os_ext( csv, Stem, Os ),
	os_ext( pl, Stem, PlF ),
	debug( convert, 'Doing: ~w', Os ),
	mtx( Os, Csv ),
	findall( protein(EnsP), member(row(EnsP),Csv), Facts ),
	portray_clauses( Facts, file(PlF) ),
	debug( convert, 'Wrote on: ~w', PlF ),
	fail.
convert :-
	debug( convert, 'Done', true ).
