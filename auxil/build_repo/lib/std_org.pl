
:- use_module(library(lists)).  % member/2.

% if library(lib) is missing, install via pack_install(lib).
:- use_module(library(lib) ).

% stoics libs, lib/1 know how to install those, if they are missing
:- lib(by_unix).
:- lib(debug_call).

% local libs & sources
:- ensure_loaded('../../lib/debug_colours').  % /1.

/** std_org( +Org, +Opts ).

Run all the scripts for building bio_db data tables for species Org.

Org should match a species sub-dir .

The caller should define std/3 in the order in which 
the runs will be made.

See Species/std_Species for example usage.

This used to be within each species but it was factored out as we 
start support more species.

@author nicos angelopoulos
@version  0:2 2022/12/28, carved out of from human/std_human.pl
@tbd debug option, for turning underlying debugs off

*/

std_org( Org, Opts ) :-
    debuc( Org ),
    debug_colours( Dlrs ),
    findall( Succ-Type-Db, ( std(Org,Type,Db),
                             std_upsh(Org,Db,Type,Dlrs,Opts,Succ)
                        ),
                                Trips ),
    Mess = '... finished ~w, standards: ~w',
    debug_consec( Org, [black,black],  Mess, [Org, Trips] ).

std_upsh( Org, Db, Type, Dlrs, Opts, Succ ) :-
    ( Org \== human ->
        atomic_list_concat( [std,Org,Type,Db], '_', Ucmd )
        ;
        atomic_list_concat( [std,Type,Db], '_', Ucmd )
    ),
    debug_consec( Org, Dlrs, 'Starting ~w\'s type: ~w, db: ~w ... ', [Org,Type,Db] ),
    atomic_list_concat( [Org,Type,Db], ':', Task ),
    debuc( Org, task(start), Task ),
    findall( Urg, (member(pass(Trg),Opts),Trg=..Parts,atomic_list_concat(Parts,'=',Urg)), Urgs ),
    Upsh =.. [upsh,Ucmd,f,p|Urgs],
    debuc( Org, 'std_org is shelling ~w,', [Upsh] ),
    % catch( @ upsh(p,f,Upshable), Err, true ),
    ( catch(@ Upsh,Err,true) ->
          ( \+ var(Err) ->
               debug_consec( Org, [red,red], 'Caught while running: ~w:~w:~w, error: ~w', [Org,Type,Db,Err] ),
               Succ = error
               ;
               debug_consec( Org, Dlrs, '... finished organism: ~w, type: ~w Db: ~w.', [Org,Type,Db] ),
               Succ = ok
          )
          ;
          debug_consec( Org, [yellow,yellow], 'Ended with failure: ~w/~w/~w', [Org,Type,Db] )
     ),
     debuc( Org, task(stop), Task ).   % fixme: change colour.. time to optionise debug_call ?
