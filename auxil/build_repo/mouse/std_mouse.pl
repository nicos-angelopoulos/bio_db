
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).

organism(mouse).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(debug_colours/1).

:- organism(Org), debuc(Org).

std_mouse_defaults(debug(true)).

/** std_mouse.

Build all the standard datasets for bio_db.

Dependencies
 * on([])
   maps_mgim
 * on(maps_mgim)
   maps_ense

@author nicos angelopoulos
@version  0.1 2017/10/12
@version  0.2 2018/03/30
@version  0.3 2018/11/05, everything tested, and working
@version  0.4 2020/09/11, added ense
@tbd add timings (in file)

*/
std_mouse( Args ) :-
    Self = std_mouse,
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

std(mouse, maps, mgim).
std(mouse, maps, ense).
std(mouse, maps, ncbi).
std(mouse, maps, unip).
std(mouse, maps, gont).
std(mouse, maps, reac).

% std( mouse, graphs, gont ).
std(mouse, graphs, strg).
