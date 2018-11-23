
:- lib(stoics_lib:message_report/3).

/**  std_graphs_strg_auto_version( Vers )

Description

==
?- std_graphs_strg_auto_version( Vers ).
Vers = '10.5'.
==

@author nicos angelopoulos
@version  0.1 2018/11/09

*/
std_graphs_strg_auto_version( VerAtm ) :-
    load_html( 'https://string-db.org/', Idx, [] ),
    findall( Li, xpath(Idx,//(li),Li), Lis ),
    Lis = [_,element(li,_,[element(a,_,[VerAtm])])|_],
    % element(li, [class=last], [element(a, _, ['10.5'])])
    atomic_list_concat( IntAtms, '.', VerAtm ),
    maplist( atom_number, IntAtms, Ints ),
    maplist( integer, Ints ),
    debug( std_graphs_strg, 'Auto detected version: ~a', VerAtm ),
    !.
std_graphs_strg_auto_version( unkonwn ) :-
	Mess1 = 'Please check number of latest version at http://string-db.org',
	Mess2 = '... and run, for example, std_graphs_string(\'10.5\').',
	message_report( Mess1, [], information ),
	message_report( Mess2, [], information ),
    fail.
