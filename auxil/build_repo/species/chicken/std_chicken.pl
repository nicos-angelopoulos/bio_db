
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).

organism(gallus).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(debug_colours/1).

:- organism(Org), debuc(Org).

std_chicken_defaults(debug(true)).

/** std_chicken.

Build all the standard datasets for bio_db.

There are no building dependencies between the datasets.

@author nicos angelopoulos
@version  0.1 2022/12/20

*/
std_chicken( Args ) :-
    Self = std_chicken,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    organism( Org ),
    std_org( Self, Org, Opts ).


std_org( Self, Org, _Opts ) :-
    % debug_call( std_hs, start, true ),
    findall( Succ-Type-Db, ( std(Org,Type,Db),
                            std_upsh(Org,Db,Type,Succ)
                        ),
                                Trips ),
    % atomic_list_concat( [std,maps,Db], '_', Upsh ),
    Mess = '... finished ~w, standards: ~w',
    debug_consec( Self, [black,black],  Mess, [Org, Trips] ).

std_upsh( Org, Db, Type, Succ ) :-
    debug_colours( Dlrs ),
    ( Org \== hs ->
        atomic_list_concat( [std,Org,Type,Db], '_', Upsh )
        ;
        atomic_list_concat( [std,Type,Db], '_', Upsh )
    ),
    debug_consec( Org, Dlrs, 'Starting ~w\'s type: ~w, db: ~w ... ', [Org,Type,Db] ),
    at_con( [Org,Type,Db], :, Task ),
    debuc( Org, task(start), Task ),
    catch( @ upsh(Upsh,f,p), Err, true ),
    ( \+ var(Err) ->
        debug_consec( Org, [red,red], 'Caught while running: ~w:~w:~w, error: ~w', [Org,Type,Db,Err] ),
        Succ = error,
        abort
        ;
        debug_consec( Org, Dlrs, '... finished organism: ~w, type: ~w Db: ~w.', [Org,Type,Db] ),
        Succ = ok
    ),
    debuc( Org, task(stop), Task ).   % fixme: change colour.. time to optionise debug_call ?

std(gallus, graphs, strg).
%
std(gallus, maps, cgnc).
std(gallus, maps, ense).
std(gallus, maps, gont).
std(gallus, maps, reac).
std(gallus, maps, unip).
