
:- use_module(library(debug)).

:- use_module(library(lib)).

:- lib(bio_db).
:- lib(bio_db_repo).

:- lib(debug).
:- debug(bio_db_stats).

/**  bio_db_stats.
     bio_db_stats(Opts).

Writes out a number of measures for the datasets served by bio_db.
You should have installed bio_db_repo, if you want to run this.

In the interest of limiting required memory, the pred assumes you
are using the Prolog interface of bio_db (see bio_db_interface/2),
this predicate abolishes the interrogated bio_db data predicates 
once it has counted its contents.  

Opts
  * abolish(Abl=true)
    use false to turn abolish off
  * csv(Csv=false)
    use true to save as csv to bio_db_stats.csv
    or give atom of name to use (requires pack(mtx))

==
?- bio_db_stats.

% bio_db version: 3:4:0, date: date(2021,5,10)
% bio_db_repo version: 21:12:3, date: date(2021,12,3)
% 
% edge_gont_is_a/2 has 70755 records.
% edge_gont_negatively_regulates/2 has 3089 records.
% edge_gont_part_of/2 has 8126 records.
% edge_gont_positively_regulates/2 has 3062 records.
% edge_gont_regulates/2 has 3552 records.
% edge_strg_hs/3 has 11938498 records.
% edge_strg_hs_symb/3 has 5085802 records.
% edge_strg_mouse/3 has 14496358 records.
% edge_strg_mouse_symb/3 has 4099916 records.
% map_ense_ensg_chrl/5 has 60664 records.
% map_ense_ensg_hgnc/2 has 40047 records.
% map_ense_ensg_symb/2 has 40047 records.
% map_ense_enst_chrl/5 has 236816 records.
% map_ense_enst_ensg/2 has 236816 records.
% map_ense_mouse_ensg_chrl/5 has 55416 records.
% map_ense_mouse_ensg_mgim/2 has 52377 records.
% map_ense_mouse_ensg_symb/2 has 55416 records.
% map_ense_mouse_enst_chrl/5 has 0 records.
% map_ense_mouse_enst_ensg/2 has 142338 records.
% map_gont_gont_gonm/2 has 43791 records.
% map_gont_gont_symb/3 has 328427 records.
% map_gont_mouse_gont_symb/3 has 419947 records.
% map_gont_mouse_mgim_gont/3 has 419963 records.
% map_gont_symb_gont/3 has 328427 records.
% map_hgnc_ccds_hgnc/2 has 18927 records.
% map_hgnc_ensg_hgnc/2 has 39997 records.
% map_hgnc_entz_hgnc/2 has 42861 records.
% map_hgnc_entz_symb/2 has 42861 records.
% map_hgnc_hgnc_ccds/2 has 18927 records.
% map_hgnc_hgnc_chrb/2 has 42909 records.
% map_hgnc_hgnc_ensg/2 has 39997 records.
% map_hgnc_hgnc_entz/2 has 42861 records.
% map_hgnc_hgnc_name/2 has 42910 records.
% map_hgnc_hgnc_symb/2 has 42910 records.
% map_hgnc_prev_symb/2 has 15257 records.
% map_hgnc_symb_entz/2 has 42861 records.
% map_hgnc_symb_hgnc/2 has 42910 records.
% map_hgnc_syno_symb/2 has 42537 records.
% map_mgim_mouse_mgim_chrl/5 has 305940 records.
% map_mgim_mouse_mgim_entz/2 has 347266 records.
% map_mgim_mouse_mgim_genb/2 has 277165 records.
% map_mgim_mouse_mgim_symb/2 has 305940 records.
% map_mgim_mouse_mgim_unip/2 has 16921 records.
% map_mgim_mouse_symb_wdra/2 has 57106 records.
% map_mgim_mouse_syno_mgim/2 has 252980 records.
% map_ncbi_dnuc_symb/2 has 319353 records.
% map_ncbi_ensg_entz/2 has 35158 records.
% map_ncbi_ensp_entz/2 has 44697 records.
% map_ncbi_entz_ensg/2 has 35158 records.
% map_ncbi_entz_ensp/2 has 44697 records.
% map_ncbi_mouse_syno_symb/2 has 77610 records.
% map_ncbi_rnuc_symb/2 has 379411 records.
% map_pros_pros_prsn/2 has 1828 records.
% map_pros_pros_sprt/7 has 54369 records.
% map_unip_ensp_unip/2 has 115206 records.
% map_unip_hgnc_unip/2 has 78237 records.
% map_unip_mouse_ensp_unip/2 has 67082 records.
% map_unip_mouse_gyno_unip/2 has 26771 records.
% map_unip_mouse_mgim_unip/2 has 80470 records.
% map_unip_mouse_trem_nucs/2 has 129597 records.
% map_unip_mouse_unip_entz/2 has 32793 records.
% map_unip_mouse_unip_symb/2 has 83203 records.
% map_unip_sprt_seqn/2 has 20375 records.
% map_unip_trem_nucs/2 has 1023960 records.
% map_unip_trem_seqn/2 has 183336 records.
% map_unip_unip_entz/2 has 33249 records.
% map_unip_unip_hgnc/2 has 78237 records.
% Total number of predicates: 67, and records: 43196490

% You should better halt this session as bio_db predicates were retracted.
true.


?- bio_db_stats( [abolish(false),csv('/tmp/bio_db_stats.csv')] ).

% bio_db version: 3:4:0, date: date(2021,5,10)
% bio_db_repo version: 21:12:3, date: date(2021,12,3)
% 
% edge_gont_is_a/2 has 70755 records.
....

?- shell('head -2 /tmp/bio_db_stats.csv').
edge_gont_is_a,2,70755
edge_gont_negatively_regulates,2,3089
...
==

@author nicos angelopoulos
@version  0.1 2018/11/23
@version  0.2 2021/12/03,  options, sort predicates

*/
bio_db_stats :-
    bio_db_stats( [] ).

bio_db_stats( Args ) :-
    Defs = [csv(false),abolish(true)],
    ( is_list(Args) -> append(Args,Defs,Opts)
                     ; append([Args],Defs,Opts) ),
    bio_db_version( Vers, Date ),
    debug( bio_db_stats, '\nbio_db version: ~w, date: ~w', [Vers,Date] ),
    bio_db_repo_version( RpVers, RpDate ),
    debug( bio_db_stats, 'bio_db_repo version: ~w, date: ~w\n', [RpVers,RpDate] ),
    memberchk( abolish(Abl), Opts ),
    bio_db_stats_trips( Abl, Trips ),
    memberchk( csv(Csv), Opts ),
    bio_db_stats_csv( Csv, Trips ),
    ( Abl == true ->
        debug( bio_db_stats, 'You should better halt this session as bio_db predicates were retracted.', [] )
        ;
        true
    ).

bio_db_stats_csv( false, _Trips ) :-
    !.
bio_db_stats_csv( true, Trips ) :-
    !,
    bio_db_stats_csv( 'bio_db_stats.csv', Trips ).
bio_db_stats_csv( CsvF, Trips ) :-
    lib( mtx ),
    findall( row(Pn,Pa,Len), member(Pn/Pa-Len,Trips), Rows ),
    debug( bio_db_stats, 'Writing to csv output to file: ~p', [CsvF] ),
    mtx( CsvF, Rows ).

bio_db_stats_trips( Abl, Trips ) :-
    findall( Pn/Pa-Len, bio_db_pred_stats(Pn,Pa,Abl,Len), Trips ),
    length( Trips, NoPreds ),
    findall( Len, member(_-Len,Trips), Lens ),
    sumlist( Lens, NoRecs ),
    debug( bio_db_stats, 'Total number of predicates: ~d, and records: ~d\n', [NoPreds,NoRecs] ).

bio_db_pred_stats( Pn, Pa, Abl, Len ) :-
    findall( Pn/Pa, current_predicate(bio_db:Pn/Pa), PnPas ),
    sort( PnPas, OrdPnPas ),
    member( Pn/Pa, OrdPnPas ),
    member( Pfx, [map_,edge_] ),
    atom_concat( Pfx, _, Pn ),
    \+ atom_concat( _, info, Pn ),  % currently map_mgim_mouse_syno_mgim_info/2 is tried...
    functor( G, Pn, Pa ),
    findall( 1, G, Ones ),
    length( Ones, Len ),
    debug( bio_db_stats, '~w/~d has ~d records.', [Pn,Pa,Len] ),
    bio_db_stats_abolish( Abl, Pn, Pa ),
    garbage_collect.

bio_db_stats_abolish( false, _Pn, _Pa ) :-
    !.
bio_db_stats_abolish( true, Pn, Pa ) :-
    abolish( bio_db:Pn/Pa ).
