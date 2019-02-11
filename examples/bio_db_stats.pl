
% :- use_module(library(lib)).
% :- lib( bio_db ).
% :- lib(debug).

:- use_module(library(bio_db)).
:- debug(bio_db_stats).

/**  bio_db_stats.

Writes out a number of measures for the datasets served by bio_db.
You should have installed bio_db_repo, if you want to run this.

In the interest of limiting required memory, (the pred assumes you
are using the Prolog interface of bio_db (see bio_db_interface/2),
and then it abolishes the interrogated bio_db data predicates 
once it has counted its contents. 

==
%  /home/nicos/.rcpl compiled 0.00 sec, 8 clauses
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.1-6-ga2ddb05d8)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- bio_db_stats.
% v:2:3:0, d:date(2019,2,11) 
% Started at Mon Feb 11 10:04:34 2019
% 0.263 seconds cpu time for 1,808,250 inferences
% 13,426 atoms, 6,283 functors, 5,101 predicates, 112 modules, 224,793 VM-codes
% 
%                     Limit   Allocated      In use
% Local  stack:           -       20 Kb    2,024  b
% Global stack:           -       60 Kb       32 Kb
% Trail  stack:           -       30 Kb    4,888  b
%        Total:    1,024 Mb      110 Kb       39 Kb
% 
% 12 garbage collections gained 488,976 bytes in 0.001 seconds.
% 18 clause garbage collections gained 635 clauses in 0.000 seconds.
% Stack shifts: 2 local, 2 global, 0 trail in 0.000 seconds
% 2 threads, 0 finished threads used 0.000 seconds
% Timed at: time(10,4,37.66731286)
% map_hgnc_hgnc_entz/2 has 41556 records.
% map_ncbi_entz_ensp/2 has 42891 records.
% map_hgnc_hgnc_ensg/2 has 37731 records.
% map_ncbi_rnuc_symb/2 has 370527 records.
% map_mgim_mouse_mgim_chrl/5 has 300460 records.
% map_hgnc_hgnc_chrb/2 has 41625 records.
% map_ncbi_dnuc_symb/2 has 298973 records.
% map_hgnc_hgnc_ccds/2 has 19010 records.
% map_ncbi_unig_entz/2 has 30782 records.
% map_hgnc_entz_symb/2 has 41556 records.
% map_ense_enst_chrl/5 has 6854 records.
% map_hgnc_entz_hgnc/2 has 41556 records.
% edge_gont_includes/2 has 77198 records.
% map_ense_ensg_chrl/5 has 58233 records.
% map_hgnc_ensg_hgnc/2 has 37731 records.
% edge_gont_is_a/2 has 77198 records.
% map_hgnc_ccds_hgnc/2 has 19010 records.
% edge_gont_regulates/2 has 3577 records.
% edge_gont_positively_regulates/2 has 3105 records.
% edge_strg_mouse_symb/3 has 3497614 records.
% edge_gont_negatively_regulates/2 has 3128 records.
% edge_strg_mouse/3 has 11944806 records.
% edge_gont_consists_of/2 has 7942 records.
% edge_gont_part_of/2 has 7942 records.
% map_gont_mouse_mgim_gont/3 has 405751 records.
% map_gont_gont_symb/2 has 284882 records.
% map_gont_gont_gonm/2 has 47375 records.
% map_gont_symb_gont/2 has 284882 records.
% map_pros_pros_sprt/7 has 54128 records.
% map_hgnc_hgnc_name/2 has 42828 records.
% map_unip_ensp_unip/2 has 107830 records.
% map_unip_mouse_gyno_unip/2 has 23962 records.
% map_hgnc_hgnc_symb/2 has 42828 records.
% map_unip_hgnc_unip/2 has 73158 records.
% map_unip_mouse_unip_unig/2 has 68855 records.
% map_unip_unip_entz/2 has 33575 records.
% map_hgnc_prev_symb/2 has 14481 records.
% map_unip_mouse_unip_symb/2 has 81866 records.
% map_hgnc_symb_entz/2 has 41556 records.
% map_unip_trem_nucs/2 has 880954 records.
% map_unip_mouse_unip_entz/2 has 33375 records.
% map_unip_unip_unig/2 has 140150 records.
% map_hgnc_symb_hgnc/2 has 42828 records.
% map_unip_mouse_trem_nucs/2 has 193045 records.
% map_hgnc_syno_symb/2 has 41436 records.
% map_unip_unip_hgnc/2 has 73158 records.
% map_unip_mouse_mgim_unip/2 has 79265 records.
% map_unip_trem_seqn/2 has 148976 records.
% map_unip_mouse_ensp_unip/2 has 65612 records.
% map_ense_ensg_hgnc/2 has 38111 records.
% map_unip_sprt_seqn/2 has 20413 records.
% edge_strg_hs_symb/3 has 5226297 records.
% map_ense_ensg_symb/2 has 38111 records.
% map_pros_pros_prsn/2 has 1790 records.
% edge_strg_hs/3 has 11759454 records.
% map_mgim_mouse_syno_mgim/2 has 251504 records.
% map_mgim_mouse_symb_wdra/2 has 56007 records.
% map_ense_enst_ensg/2 has 6854 records.
% map_mgim_mouse_mgim_unip/2 has 16547 records.
% map_mgim_mouse_mgim_symb/2 has 300460 records.
% map_ncbi_ensg_entz/2 has 25875 records.
% map_mgim_mouse_mgim_genb/2 has 276324 records.
% map_ncbi_ensp_entz/2 has 42891 records.
% map_ncbi_entz_ensg/2 has 25875 records.
% 
% Total number of predicates: 64, and records: 38404274
% 
% Timed at: time(10,4,51.85700941)
% Started at Mon Feb 11 10:04:34 2019
% 14.453 seconds cpu time for 40,312,573 inferences
% 3,075,632 atoms, 6,411 functors, 5,269 predicates, 115 modules, 260,658,792 VM-codes
% 
%                     Limit   Allocated      In use
% Local  stack:           -       20 Kb    2,024  b
% Global stack:           -      512 Mb    9,720  b
% Trail  stack:           -      254 Kb    4,400  b
%        Total:    1,024 Mb      512 Mb   16,144  b
% 
% 13 garbage collections gained 543,904 bytes in 0.001 seconds.
% 19 clause garbage collections gained 651 clauses in 0.000 seconds.
% Stack shifts: 2 local, 6 global, 3 trail in 0.001 seconds
% 2 threads, 0 finished threads used 0.000 seconds
% ...halting as all predicates have been retracted.
==

@author nicos angelopoulos
@version  0.1 2018/11/23
@version  0.2 2019/2/11

*/
bio_db_stats :-
    bio_db_version( V, D ),
    debug( bio_db_stats, 'v:~w, d:~w ', [V,D] ),
    statistics,
    bio_db_time,
    findall( Pn/Pa-Len, bio_db_stats(Pn,Pa,Len), Trips ),
    length( Trips, NoPreds ),
    findall( Len, member(_-Len,Trips), Lens ),
    sumlist( Lens, NoRecs ),
    debug( bio_db_stats, '', [] ),
    debug( bio_db_stats, 'Total number of predicates: ~d, and records: ~d', [NoPreds,NoRecs] ),
    debug( bio_db_stats, '', [] ),
    bio_db_time,
    statistics,
    debug( bio_db_stats, '...halting as all predicates have been retracted.', [] ),
    sleep(3),  % give gc thread time to catch up... fixme:
    halt.

bio_db_stats( Pn, Pa, Len ) :-
    current_predicate( bio_db:Pn/Pa ),
    % ( Pn == map_unip_mouse_unip_symb -> trace; true ),
    member( Pfx, [map_,edge_] ),
    atom_concat( Pfx, _, Pn ),
    functor( G, Pn, Pa ),
    \+ atom_concat( _, info, Pn ),  % currently map_mgim_mouse_syno_mgim_info/2 is tried...
    findall( 1, G, Ones ),
    length( Ones, Len ),
    debug( bio_db_stats, '~w/~d has ~d records.', [Pn,Pa,Len] ).
    % abolish( bio_db:Pn/Pa ).
    % retractall( bio_db:G ).

bio_db_time :-
    get_time( Stamp ),
    stamp_date_time(Stamp, D, 0),
    date_time_value(time, D, Time ),
    debug( bio_db_stats, 'Timed at: ~w', [Time] ).
