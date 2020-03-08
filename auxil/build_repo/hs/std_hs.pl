
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).


organism(hs).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:at_con/3).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(debug_colours/1).  % /1.

:- organism(Org), debug(Org).

std_hs_defaults(debug(true)).

/** std_hs(+Opts).

Build all the standard datasets for bio_db.

@author nicos angelopoulos
@version  0.1 2017/10/12
@version  0.2 2018/03/30
@version  0.3 2018/11/05, everything tested, and working
@tbd add timings (in file)

*/
std_hs( Args ) :-
    Self = std_hs,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    organism( Org ),
    std_org( Org, Opts ).

std_org( Org, _Opts ) :-
    % debug_call( std_hs, start, true ),
    findall( Succ-Type-Db, ( std(Org,Type,Db),
                            std_upsh(Org,Db,Type,Succ)
                        ),
                                Trips ),
    % atomic_list_concat( [std,maps,Db], '_', Upsh ),
    Mess = '... finished ~w, standards: ~w',
    debug_consec( Org, [black,black],  Mess, [Org, Trips] ).

std_upsh( Org, Db, Type, Succ ) :-
    debug_colours( Dlrs ),
    ( Org \== hs ->
        atomic_list_concat( [std,Org,Type,Db], '_', Upsh )
        ;
        atomic_list_concat( [std,Type,Db], '_', Upsh )
    ),
    debug_consec( Org, Dlrs, 'Starting ~w\'s type: ~w, db: ~w ... ', [Org,Type,Db] ),
    at_con( [Org,Type,Db], :, Task ),
    debug_call( Org, task(start), Task ),
    catch( @ pupsh(f,Upsh), Err, true ),
    ( \+ var(Err) ->
        debug_consec( Org, [red,red], 'Caught while running: ~w:~w:~w, error: ~w', [Org,Type,Db,Err] ),
        Succ = error,
        abort
        ;
        debug_consec( Org, Dlrs, '... finished organism: ~w, type: ~w Db: ~w.', [Org,Type,Db] ),
        Succ = ok
    ),
    debug_call( Org, task(start), Task ).   % fixme: change colour.. time to optionise debug_call ?

std(hs, maps, hgnc).
std(hs, maps, ense).
std(hs, maps, unip).
std(hs, maps, unip_seqs).
std(hs, maps, ncbi).
% needs "unip_seqs" above to have ran
std(hs, maps, pros).
% requires, maps-hgnc
std(hs, maps, gont).

std(hs, graphs, gont).
std(hs, graphs, strg).   % needs hgnc and ncbi
