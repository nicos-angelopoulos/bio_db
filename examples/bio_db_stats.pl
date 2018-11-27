
:- use_module(library(lib)).
:- lib( bio_db ).

:- lib(debug).
:- debug(bio_db_stats).

/**  bio_db_stats.

Writes out a number of measures for the datasets served by bio_db.
You should have installed bio_db_repo, if you want to run this.

In the interest of limiting required memory, (the pred assumes you
are using the Prolog interface of bio_db (see bio_db_interface/2),
this predicate abolishes the interrogated bio_db data predicates 
once it has counted its contents. 


==
?- bio_db_stats.


?- bio_db_stats.
% edge_gont_is_a/2 has 77155 records.
% map_hgnc_ensg_hgnc/2 has 37647 records.
% edge_gont_regulates/2 has 3573 records.
% map_hgnc_ccds_hgnc/2 has 19019 records.
% map_mgim_mouse_mgim_genb/2 has 276279 records.
% map_hgnc_entz_symb/2 has 41496 records.
% map_mgim_mouse_mgim_symb/2 has 300103 records.
% map_hgnc_entz_hgnc/2 has 41496 records.
% edge_gont_includes/2 has 77155 records.
% map_mgim_mouse_mgim_unip/2 has 16544 records.
% map_ncbi_dnuc_symb/2 has 297889 records.
% map_mgim_mouse_symb_wdra/2 has 55972 records.
% map_ncbi_unig_entz/2 has 30775 records.
% map_mgim_mouse_syno_mgim/2 has 251446 records.
% map_ncbi_entz_ensp/2 has 42906 records.
% map_ncbi_rnuc_symb/2 has 369320 records.
% map_unip_mouse_ensp_unip/2 has 65083 records.
% map_unip_mouse_mgim_unip/2 has 78847 records.
% map_ncbi_ensp_entz/2 has 42906 records.
% map_unip_mouse_trem_nucs/2 has 192439 records.
% map_ncbi_entz_ensg/2 has 25885 records.
% map_gont_mouse_mgim_gont/3 has 403372 records.
% map_unip_mouse_unip_entz/2 has 33343 records.
% map_unip_mouse_unip_symb/2 has 81440 records.
% map_ncbi_ensg_entz/2 has 25885 records.
% map_unip_mouse_unip_unig/2 has 68564 records.
% map_ense_enst_ensg/2 has 199130 records.
% edge_strg_hs_symb/3 has 4563139 records.
% map_pros_pros_prsn/2 has 1790 records.
% map_ense_ensg_symb/2 has 38020 records.
% edge_strg_hs/3 has 11353056 records.
% map_unip_trem_seqn/2 has 156062 records.
% map_ense_ensg_chrl/5 has 58233 records.
% map_ense_ensg_hgnc/2 has 38020 records.
% map_unip_sprt_seqn/2 has 20408 records.
% map_unip_unip_unig/2 has 139753 records.
% map_hgnc_hgnc_entz-ncbi/2 has 41455 records.
% map_hgnc_hgnc_entz/2 has 41496 records.
% map_unip_unip_hgnc/2 has 72342 records.
% map_ense_enst_chrl/5 has 199130 records.
% map_unip_unip_entz/2 has 33287 records.
% map_hgnc_hgnc_ensg/2 has 37647 records.
% edge_strg_mouse_symb/3 has 3679223 records.
% map_hgnc_hgnc_entz-appv/2 has 38858 records.
% map_unip_trem_nucs/2 has 885674 records.
% edge_strg_mouse/3 has 12614042 records.
% map_hgnc_entz-ncbi_symb/2 has 41455 records.
% map_unip_ensp_unip/2 has 106659 records.
% map_hgnc_hgnc_chrb/2 has 41557 records.
% map_unip_hgnc_unip/2 has 72342 records.
% map_hgnc_symb_entz/2 has 41496 records.
% map_hgnc_entz-appv_symb/2 has 38858 records.
% map_gont_symb_gont/2 has 282299 records.
% map_pros_pros_sprt/7 has 54116 records.
% map_gont_gont_gonm/2 has 47375 records.
% map_hgnc_symb_hgnc/2 has 46016 records.
% map_hgnc_syno_symb/2 has 41404 records.
% map_gont_gont_symb/2 has 282299 records.
% edge_gont_part_of/2 has 7950 records.
% map_hgnc_hgnc_symb/2 has 46016 records.
% map_mgim_mouse_mgim_chrl/5 has 300103 records.
% map_hgnc_prev_symb/2 has 14456 records.
% edge_gont_consists_of/2 has 7950 records.
% edge_gont_negatively_regulates/2 has 3125 records.
% map_hgnc_hgnc_ccds/2 has 19019 records.
% map_hgnc_hgnc_name/2 has 46016 records.
% edge_gont_positively_regulates/2 has 3103 records.

% Total number of predicates: 67, and records: 38710918
% ...halting as all predicates have been retracted.

==

@author nicos angelopoulos
@version  0.1 2018/11/23

*/
bio_db_stats :-
    findall( Pn/Pa-Len, bio_db_stats(Pn,Pa,Len), Trips ),
    length( Trips, NoPreds ),
    findall( Len, member(_-Len,Trips), Lens ),
    sumlist( Lens, NoRecs ),
    nl,
    debug( bio_db_stats, 'Total number of predicates: ~d, and records: ~d', [NoPreds,NoRecs] ),
    nl,
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
