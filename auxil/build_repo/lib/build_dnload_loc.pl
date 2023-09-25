
:- ensure_loaded(bio_db_build_aliases).     % defines build_db_build_downlads(Db) file alias

:- lib(os_lib).
:- lib(options).
:- lib(debug_call).

/** build_dnload_loc(+Self, -Loc, +Opts).

Get the location for download for db() and make sure it is created.

Opts 
  * db(DB)
    source database
  * debug(Dbg)
    debug passed to os_make_path/2

Pred also prints debug of the dir location on Self channel. 

Note these are expected to be fully formed Opts list, as this is a helper predicate.

==
?- bio_db_build_aliases([]).
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-23.09.25'
true.

?- debug(bdl_ex).
Warning: bdl_ex: no matching debug topic (yet)
true.

?- build_dnloads_loc( bdl_ex, Loc, [db(ncbi),debug(true)] ).
% Db: ncbi, dnload location: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-23.09.25/dnloads/ncbi'
Loc = '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-23.09.25/dnloads/ncbi'.
==

@author nicos angelopoulos
@version  0:1 2023/9/25

*/
build_dnloads_loc( Self, Loc, Opts ) :-
     options( [db(Db),debug(Dbg)], Opts ),
     absolute_file_name( bio_db_build_downloads(Db), Loc ),
     debuc( Self, 'Db: ~w, dnload location: ~p', [Db,Loc] ),
     os_make_path( Loc, debug(Dbg) ).
