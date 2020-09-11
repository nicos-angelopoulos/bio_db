
% if library(lib) is missing, install via pack_install(lib).
%
% :- use_module(library(debug) ).
:- use_module(library(lists) ).
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

:- organism(Org), debuc(Org).

std_hs_defaults(debug(true)).

/** std_hs(+Opts).

Build all the standard datasets for bio_db.

Opts
  * debug(Dbg=true)
    debug this level
  * pass(Term)
    Terms to pass to the upsh calls in lower level

==
% date
Thu 10 Sep 19:34:38 BST 2020
% upsh std_hs pass=assembly=38 release=101
==

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

std_org( Org, Opts ) :-
    % debug_call( std_hs, start, true ),
    findall( Succ-Type-Db, ( std(Org,Type,Db),
                             std_upsh(Org,Db,Type,Opts,Succ)
                        ),
                                Trips ),
    % atomic_list_concat( [std,maps,Db], '_', Upsh ),
    Mess = '... finished ~w, standards: ~w',
    debug_consec( Org, [black,black],  Mess, [Org, Trips] ).

std_upsh( Org, Db, Type, Opts, Succ ) :-
    debug_colours( Dlrs ),
    ( Org \== hs ->
        atomic_list_concat( [std,Org,Type,Db], '_', Ucmd )
        ;
        atomic_list_concat( [std,Type,Db], '_', Ucmd )
    ),
    debug_consec( Org, Dlrs, 'Starting ~w\'s type: ~w, db: ~w ... ', [Org,Type,Db] ),
    at_con( [Org,Type,Db], :, Task ),
    debuc( Org, task(start), Task ),
    findall( Urg, (member(pass(Trg),Opts),Trg=..Parts,at_con(Parts,'=',Urg)), Urgs ),
    % at_con( [Upsh|Urgs], ' ', Upshable ),
    Upsh =.. [upsh,p,f,Ucmd|Urgs],
    debuc( Org, 'std_hs is shelling ~w,', [Upsh] ),
    % catch( @ upsh(p,f,Upshable), Err, true ),
    catch( @ Upsh, Err, true ),
    ( \+ var(Err) ->
        debug_consec( Org, [red,red], 'Caught while running: ~w:~w:~w, error: ~w', [Org,Type,Db,Err] ),
        Succ = error,
        abort
        ;
        debug_consec( Org, Dlrs, '... finished organism: ~w, type: ~w Db: ~w.', [Org,Type,Db] ),
        Succ = ok
    ),
    debuc( Org, task(start), Task ).   % fixme: change colour.. time to optionise debug_call ?

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
