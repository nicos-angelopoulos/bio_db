%% bio_db_data_predicate( Pn, Pa, Org, Cell).
%
% Auto-generated predicate; listing and name-documenting all data predicates in bio_db.
% This predicate is statically generated from the source cell files.
%
%  * hs/ense.pl
%    * map_ense_ensg_hgnc/2
%    * map_ense_ensg_symb/2
%    * map_ense_enst_chrl/5
%    * map_ense_ensg_chrl/5
%    * map_ense_enst_ensg/2
%  * hs/gont.pl
%    * edge_gont_includes/2
%    * edge_gont_is_a/2
%    * edge_gont_regulates/2
%    * edge_gont_positively_regulates/2
%    * edge_gont_negatively_regulates/2
%    * edge_gont_consists_of/2
%    * edge_gont_part_of/2
%    * map_gont_gont_symb/2
%    * map_gont_gont_gonm/2
%    * map_gont_symb_gont/2
%  * hs/hgnc.pl
%    * map_hgnc_ccds_hgnc/2
%    * map_hgnc_ensg_hgnc/2
%    * map_hgnc_entz_hgnc/2
%    * map_hgnc_entz_symb/2
%    * map_hgnc_hgnc_ccds/2
%    * map_hgnc_hgnc_name/2
%    * map_hgnc_hgnc_symb/2
%    * map_hgnc_prev_symb/2
%    * map_hgnc_symb_hgnc/2
%    * map_hgnc_syno_symb/2
%    * map_hgnc_symb_entz/2
%    * map_hgnc_entz-appv_symb/2
%    * map_hgnc_entz-ncbi_symb/2
%    * map_hgnc_hgnc_chrb/2
%    * map_hgnc_hgnc_ensg/2
%    * map_hgnc_hgnc_entz-appv/2
%    * map_hgnc_hgnc_entz-ncbi/2
%    * map_hgnc_hgnc_entz/2
%  * hs/ncbi.pl
%    * map_ncbi_ensg_entz/2
%    * map_ncbi_ensp_entz/2
%    * map_ncbi_entz_ensg/2
%    * map_ncbi_entz_ensp/2
%    * map_ncbi_rnuc_symb/2
%    * map_ncbi_dnuc_symb/2
%    * map_ncbi_unig_entz/2
%  * hs/pros.pl
%    * map_pros_pros_prsn/2
%    * map_pros_pros_sprt/7
%  * hs/strg.pl
%    * edge_strg_hs/3
%    * edge_strg_hs_symb/3
%  * hs/unip.pl
%    * map_unip_hgnc_unip/2
%    * map_unip_ensp_unip/2
%    * map_unip_trem_nucs/2
%    * map_unip_unip_entz/2
%    * map_unip_unip_hgnc/2
%    * map_unip_unip_unig/2
%    * map_unip_sprt_seqn/2
%    * map_unip_trem_seqn/2
%  * mouse/gont.pl
%    * map_gont_mouse_mgim_gont/3
%  * mouse/mgim.pl
%    * map_mgim_mouse_mgim_chrl/5
%    * map_mgim_mouse_mgim_genb/2
%    * map_mgim_mouse_mgim_symb/2
%    * map_mgim_mouse_mgim_unip/2
%    * map_mgim_mouse_symb_wdra/2
%    * map_mgim_mouse_syno_mgim/2
%  * mouse/strg.pl
%    * edge_strg_mouse/3
%    * edge_strg_mouse_symb/3
%  * mouse/unip.pl
%    * map_unip_mouse_ensp_unip/2
%    * map_unip_mouse_mgim_unip/2
%    * map_unip_mouse_trem_nucs/2
%    * map_unip_mouse_unip_entz/2
%    * map_unip_mouse_unip_symb/2
%    * map_unip_mouse_unip_unig/2
%
% Generated on 18.11.26 from bio_db with pack.pl version: 2.0.
% 
%
bio_db_data_predicate(map_ense_ensg_chrl, 5, hs, 'hs/ense.pl').
bio_db_data_predicate(map_ense_ensg_hgnc, 2, hs, 'hs/ense.pl').
bio_db_data_predicate(map_ense_ensg_symb, 2, hs, 'hs/ense.pl').
bio_db_data_predicate(map_ense_enst_chrl, 5, hs, 'hs/ense.pl').
bio_db_data_predicate(map_ense_enst_ensg, 2, hs, 'hs/ense.pl').
bio_db_data_predicate(edge_gont_consists_of, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(edge_gont_includes, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(edge_gont_is_a, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(edge_gont_negatively_regulates, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(edge_gont_part_of, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(edge_gont_positively_regulates, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(edge_gont_regulates, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(map_gont_gont_gonm, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(map_gont_gont_symb, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(map_gont_symb_gont, 2, hs, 'hs/gont.pl').
bio_db_data_predicate(map_hgnc_ccds_hgnc, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_ensg_hgnc, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate('map_hgnc_entz-appv_symb', 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate('map_hgnc_entz-ncbi_symb', 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_entz_hgnc, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_entz_symb, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_hgnc_ccds, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_hgnc_chrb, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_hgnc_ensg, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_hgnc_entz, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate('map_hgnc_hgnc_entz-appv', 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate('map_hgnc_hgnc_entz-ncbi', 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_hgnc_name, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_hgnc_symb, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_prev_symb, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_symb_entz, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_symb_hgnc, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_hgnc_syno_symb, 2, hs, 'hs/hgnc.pl').
bio_db_data_predicate(map_ncbi_dnuc_symb, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_ncbi_ensg_entz, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_ncbi_ensp_entz, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_ncbi_entz_ensg, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_ncbi_entz_ensp, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_ncbi_rnuc_symb, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_ncbi_unig_entz, 2, hs, 'hs/ncbi.pl').
bio_db_data_predicate(map_pros_pros_prsn, 2, hs, 'hs/pros.pl').
bio_db_data_predicate(map_pros_pros_sprt, 7, hs, 'hs/pros.pl').
bio_db_data_predicate(edge_strg_hs, 3, hs, 'hs/strg.pl').
bio_db_data_predicate(edge_strg_hs_symb, 3, hs, 'hs/strg.pl').
bio_db_data_predicate(map_unip_ensp_unip, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_hgnc_unip, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_sprt_seqn, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_trem_nucs, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_trem_seqn, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_unip_entz, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_unip_hgnc, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_unip_unip_unig, 2, hs, 'hs/unip.pl').
bio_db_data_predicate(map_gont_mouse_mgim_gont, 3, mouse, 'mouse/gont.pl').
bio_db_data_predicate(map_mgim_mouse_mgim_chrl, 5, mouse, 'mouse/mgim.pl').
bio_db_data_predicate(map_mgim_mouse_mgim_genb, 2, mouse, 'mouse/mgim.pl').
bio_db_data_predicate(map_mgim_mouse_mgim_symb, 2, mouse, 'mouse/mgim.pl').
bio_db_data_predicate(map_mgim_mouse_mgim_unip, 2, mouse, 'mouse/mgim.pl').
bio_db_data_predicate(map_mgim_mouse_symb_wdra, 2, mouse, 'mouse/mgim.pl').
bio_db_data_predicate(map_mgim_mouse_syno_mgim, 2, mouse, 'mouse/mgim.pl').
bio_db_data_predicate(edge_strg_mouse, 3, mouse, 'mouse/strg.pl').
bio_db_data_predicate(edge_strg_mouse_symb, 3, mouse, 'mouse/strg.pl').
bio_db_data_predicate(map_unip_mouse_ensp_unip, 2, mouse, 'mouse/unip.pl').
bio_db_data_predicate(map_unip_mouse_mgim_unip, 2, mouse, 'mouse/unip.pl').
bio_db_data_predicate(map_unip_mouse_trem_nucs, 2, mouse, 'mouse/unip.pl').
bio_db_data_predicate(map_unip_mouse_unip_entz, 2, mouse, 'mouse/unip.pl').
bio_db_data_predicate(map_unip_mouse_unip_symb, 2, mouse, 'mouse/unip.pl').
bio_db_data_predicate(map_unip_mouse_unip_unig, 2, mouse, 'mouse/unip.pl').
