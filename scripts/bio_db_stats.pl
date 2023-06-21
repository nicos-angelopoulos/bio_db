
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- use_module(library(lib)).

:- lib(bio_db).
:- lib(bio_db_repo).
:- lib(by_unix).

:- lib(debug).
:- debug(bio_db_stats).
% :- debug(bio_db).

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
    use false to turn abolish off. This is no longer needed (v0.3)
  * csv(Csv=false)
    requires pack(mtx),
    use _true_ to save as csv to bio_db_stats.csv, _version_ for depositing
    a bio_db_repo installed versioned csv in bio_db_scirpts/ directory,
    or give atom of file name to use

As of bio_db_version(3:6:0,_) the directory scripts/bio_db_stats/ holds the csv output of this script for each 
new pack(bio_db_repo) version, produced with option csv(version).

As of version 0.3 you need pack(by_unix)- this avoids over-loading the memory.

==
?- bio_db_stats.
% cgnc_galg_cgnc_curs/2 has 25925 records.
% cgnc_galg_cgnc_edat/2 has 25925 records.
% cgnc_galg_cgnc_ensg/2 has 15253 records.
% cgnc_galg_cgnc_name/2 has 25925 records.
% cgnc_galg_cgnc_ncbi/2 has 25921 records.
% cgnc_galg_cgnc_symb/2 has 25925 records.
% cgnc_galg_cgnc_syno/2 has 3030 records.
% ense_galg_ensg_chrl/5 has 30108 records.
% ense_galg_ensg_symb/2 has 14249 records.
% ense_galg_enst_chrl/5 has 72689 records.
% ense_galg_enst_ensg/2 has 72689 records.
% ense_gg6a_ensg_chrl/5 has 30862 records.
% ense_gg6a_ensg_symb/2 has 6480 records.
% ense_gg6a_enst_chrl/5 has 74296 records.
% ense_gg6a_enst_ensg/2 has 74296 records.
% ense_homs_ensg_chrl/5 has 62710 records.
% ense_homs_ensg_hgnc/2 has 41287 records.
% ense_homs_ensg_symb/2 has 41287 records.
% ense_homs_enst_chrl/5 has 252702 records.
% ense_homs_enst_ensg/2 has 252702 records.
% ense_musm_ensg_chrl/5 has 57010 records.
% ense_musm_ensg_mgim/2 has 55398 records.
% ense_musm_ensg_symb/2 has 57010 records.
% ense_musm_enst_chrl/5 has 149347 records.
% ense_musm_enst_ensg/2 has 149347 records.
% ense_suss_ensg_chrl/5 has 35670 records.
% ense_suss_ensg_symb/2 has 17511 records.
% ense_suss_enst_chrl/5 has 60273 records.
% ense_suss_enst_ensg/2 has 60273 records.
% gont_galg_symb_gont/4 has 117139 records.
% gont_homs_edge_gisa/2 has 68650 records.
% gont_homs_edge_gnrg/2 has 2951 records.
% gont_homs_edge_gpof/2 has 7696 records.
% gont_homs_edge_gprg/2 has 2945 records.
% gont_homs_edge_greg/2 has 3396 records.
% gont_homs_gont_gonm/2 has 42950 records.
% gont_homs_gont_symb/3 has 336023 records.
% gont_homs_symb_gont/3 has 336023 records.
% gont_musm_gont_symb/4 has 437854 records.
% gont_musm_mgim_gont/4 has 437865 records.
% gont_suss_symb_gont/4 has 125832 records.
% hgnc_homs_ccds_hgnc/2 has 18926 records.
% hgnc_homs_ensg_hgnc/2 has 41001 records.
% hgnc_homs_hgnc_ccds/2 has 18926 records.
% hgnc_homs_hgnc_chrb/2 has 43699 records.
% hgnc_homs_hgnc_ensg/2 has 41001 records.
% hgnc_homs_hgnc_name/2 has 43700 records.
% hgnc_homs_hgnc_ncbi/2 has 43654 records.
% hgnc_homs_hgnc_symb/2 has 43700 records.
% hgnc_homs_ncbi_hgnc/2 has 43654 records.
% hgnc_homs_ncbi_symb/2 has 43654 records.
% hgnc_homs_prev_symb/2 has 15516 records.
% hgnc_homs_symb_hgnc/2 has 43700 records.
% hgnc_homs_symb_ncbi/2 has 43654 records.
% hgnc_homs_syno_symb/2 has 43719 records.
% mgim_musm_mgim_chrl/5 has 676449 records.
% mgim_musm_mgim_genb/2 has 277243 records.
% mgim_musm_mgim_ncbi/2 has 719140 records.
% mgim_musm_mgim_symb/2 has 676449 records.
% mgim_musm_mgim_unip/2 has 16988 records.
% mgim_musm_symb_wdra/2 has 59005 records.
% mgim_musm_syno_mgim/2 has 257027 records.
% ncbi_homs_dnuc_symb/2 has 869087 records.
% ncbi_homs_ensg_ncbi/2 has 36278 records.
% ncbi_homs_ensp_ncbi/2 has 46300 records.
% ncbi_homs_ncbi_ensg/2 has 36278 records.
% ncbi_homs_ncbi_ensp/2 has 46300 records.
% ncbi_homs_rnuc_symb/2 has 466669 records.
% ncbi_musm_syno_symb/2 has 70348 records.
% ncbi_suss_ensg_ncbi/2 has 17782 records.
% ncbi_suss_ensp_ncbi/2 has 23600 records.
% ncbi_suss_ncbi_ensg/2 has 17782 records.
% ncbi_suss_ncbi_ensp/2 has 23600 records.
% pros_homs_pros_prsn/2 has 1851 records.
% pros_homs_pros_sprt/7 has 54556 records.
% strg_galg_edge_ensp/3 has 7821418 records.
% strg_galg_edge_symb/3 has 3910709 records.
% strg_galg_ensp_symb/2 has 15508 records.
% strg_homs_edge_ensp/3 has 11938498 records.
% strg_homs_edge_symb/3 has 5066306 records.
% strg_musm_edge_ensp/3 has 14496358 records.
% strg_musm_edge_symb/3 has 6258522 records.
% strg_suss_edge_ensp/3 has 13781164 records.
% strg_suss_edge_symb/3 has 6890582 records.
% strg_suss_ensp_symb/2 has 21597 records.
% unip_galg_unip_ensp/2 has 45911 records.
% unip_galg_unip_gyno/2 has 1534 records.
% unip_galg_unip_ncbi/2 has 9055 records.
% unip_galg_unip_strp/2 has 3054 records.
% unip_galg_unip_symb/2 has 45484 records.
% unip_homs_ensp_unip/2 has 120717 records.
% unip_homs_hgnc_unip/2 has 81725 records.
% unip_homs_sprt_seqn/2 has 20422 records.
% unip_homs_trem_nucs/2 has 1038884 records.
% unip_homs_trem_seqn/2 has 187358 records.
% unip_homs_unip_hgnc/2 has 81725 records.
% unip_homs_unip_ncbi/2 has 33144 records.
% unip_musm_ensp_unip/2 has 67162 records.
% unip_musm_gyno_unip/2 has 28951 records.
% unip_musm_mgim_unip/2 has 80434 records.
% unip_musm_trem_nucs/2 has 129691 records.
% unip_musm_unip_ncbi/2 has 32442 records.
% unip_musm_unip_symb/2 has 83163 records.
% Total number of predicates: 103, and records: 80948178

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
@version  0.3 2023/06/21,  use pack(by_unix) to use external call-avoid memory problems

*/
bio_db_stats :-
    bio_db_stats( [] ).

bio_db_stats( Args ) :-
    Defs = [csv(false),abolish(true)],
    ( is_list(Args) -> append(Args,Defs,Opts)
                     ; append([Args],Defs,Opts) ),
    bio_db_version( Vers, Date ),
    debug( bio_db_stats, 'bio_db version: ~w, date: ~w', [Vers,Date] ),
    bio_db_repo_version( RpVers, RpDate ),
    debug( bio_db_stats, 'bio_db_repo version: ~w, date: ~w\n', [RpVers,RpDate] ),
    memberchk( abolish(Abl), Opts ),
    bio_db_stats_trips( Abl, Trips, NoPreds, NoRecs ),
    memberchk( csv(Csv), Opts ),
    bio_db_stats_csv( Csv, Trips, NoPreds, NoRecs ),
    ( Abl == true ->
        debug( bio_db_stats, 'You should better halt this session as bio_db predicates were retracted.', [] )
        ;
        true
    ).

bio_db_stats_csv( false, _Trips, _NoPreds, _NoRecs ) :-
    !.
bio_db_stats_csv( true, Trips, NoPreds, NoRecs ) :-
    !,
    bio_db_stats_csv( 'bio_db_stats.csv', Trips, NoPreds, NoRecs ).
bio_db_stats_csv( version, Trips, NoPreds, NoRecs ) :-
    !,
    bio_db_repo_version( RpVers, _RpDate ),
    RpVers = Mj:Mn:Fx,
    atomic_list_concat( ['bio_db_stats/bio_db_stats-',Mj,'.',Mn,'.',Fx,'.csv'], '', CsvF ),
    bio_db_stats_csv( CsvF, Trips, NoPreds, NoRecs ).
bio_db_stats_csv( CsvF, Trips, NoPreds, NoRecs ) :-
    lib( mtx ),
    findall( row(Pn,Pa,Len), member(Pn/Pa-Len,Trips), Rows ),
    debug( bio_db_stats, 'Writing to csv output to file: ~p', [CsvF] ),
    bio_db_version( Vers, Date ),
    Vers = Mj:Mn:Fx,
    Date = date(Yr,Mo,Da),
    atomic_list_concat( [Mj,Mn,Fx], ':', BioDbVersTkn ),
    atomic_list_concat( [Yr,Mo,Da], '.', BioDbDateTkn ),
    bio_db_repo_version( RVers, RDate ),
    RVers = RMj:RMn:RFx,
    RDate = date(RYr,RMo,RDa),
    atomic_list_concat( [RMj,RMn,RFx], ':', RBioDbVersTkn ),
    atomic_list_concat( [RYr,RMo,RDa], '.', RBioDbDateTkn ),
    Hdr1 = row(bio_db_version,BioDbVersTkn,BioDbDateTkn ),
    Hdr2 = row(bio_db_repo_version,RBioDbVersTkn,RBioDbDateTkn),
    Hdr3 = row(total_tables_records,NoPreds,NoRecs),
    mtx( CsvF, [Hdr1,Hdr2,Hdr3|Rows] ).

bio_db_stats_trips( Abl, Trips, NoPreds, NoRecs ) :-
    findall( Pn/Pa, bio_db_data_predicate(Pn,Pa), PnPas ),
    sort( PnPas, OrdPnPas ),
    % debuc( bio_db_stats, enum, ord_pn_pas/OrdPnPas ),
    bio_db_preds_stats( OrdPnPas, Abl, Trips ),
    findall( Len, member(_-Len,Trips), Lens ),
    sum_list( Lens, NoRecs ),
    % findall( Pn/Pa-Len, bio_db_pred_stats(Pn,Pa,Abl,Len), Trips ),
    length( Trips, NoPreds ),
    debug( bio_db_stats, 'Total number of predicates: ~d, and records: ~d\n', [NoPreds,NoRecs] ).

bio_db_data_predicate( Pn, Pa ) :-
    current_predicate( bio_db:Pn/Pa ),
    % member( Pn/Pa, OrdPnPas ),
    % member( Pfx, [map_,edge_] ),
    % atom_concat( Pfx, _, Pn ),
    atomic_list_concat( [Db,Org,Obj1,Obj2], '_', Pn ),
    maplist( atom_length, [Db,Org,Obj1,Obj2], [4,4,4,4] ).


bio_db_preds_stats( [], _Abl, [] ).
bio_db_preds_stats( [Pn/Pa|T], Abl, [Pn/Pa-Len|Lens] ) :-
    % \+ atom_concat( _, info, Pn ),  % currently map_mgim_mouse_syno_mgim_info/2 is tried...
    functor( G, Pn, Pa ),
    % debug( bio_db_stats, 'doing: ~w', [Pn/Pa] ),
    % once( \+ \+ G ),
    % once( predicate_property(bio_db:G,number_of_clauses(Len)) ),
    term_to_atom( G, Gatm ),
    atomic_list_concat( ['findall(1,',Gatm,',Ones),length(Ones,Len),write(Len),nl,halt.'], Findall ),
    Got @@ swipl( -q, -g, 'lib(bio_db)', -g, Findall ),
    last( Got, Last ),
    % Got = [LenAtm],
    atom_number( Last, Len ),
    number( Len ),
    % findall( 1, G, Ones ),
    % length( Ones, Len ),
    debug( bio_db_stats, '~w/~d has ~d records.', [Pn,Pa,Len] ),
    % this is no longer relevant
    % bio_db_stats_abolish( Abl, Pn, Pa ),
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_clauses,
    trim_stacks,
    trim_heap,
    bio_db_preds_stats( T, Abl, Lens ).

bio_db_stats_abolish( false, _Pn, _Pa ) :-
    !.
bio_db_stats_abolish( true, Pn, Pa ) :-
    % abolish( bio_db:Pn/Pa ).
    retractall( bio_db:Pn/Pa ).
