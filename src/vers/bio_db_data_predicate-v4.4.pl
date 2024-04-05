%% bio_db_data_predicate( Pn, Pa, Org, Cell).
%
% Auto-generated predicate; listing and name-documenting all data predicates in bio_db.
% This predicate is statically generated from the source cell files.
%
%  * chicken/cgnc.pl
%    * cgnc_galg_cgnc_curs/2
%    * cgnc_galg_cgnc_edat/2
%    * cgnc_galg_cgnc_ncbi/2
%    * cgnc_galg_cgnc_ensg/2
%    * cgnc_galg_cgnc_name/2
%    * cgnc_galg_cgnc_symb/2
%    * cgnc_galg_cgnc_syno/2
%  * chicken/ense.pl
%    * ense_galg_enst_chrl/5
%    * ense_galg_ensg_chrl/5
%    * ense_galg_enst_ensg/2
%    * ense_galg_ensg_symb/2
%    * ense_gg6a_enst_chrl/5
%    * ense_gg6a_ensg_chrl/5
%    * ense_gg6a_enst_ensg/2
%    * ense_gg6a_ensg_symb/2
%  * chicken/gont.pl
%    * gont_galg_symb_gont/4
%  * chicken/ncbi.pl
%    * ncbi_galg_dnuc_symb/2
%    * ncbi_galg_ncbi_ensg/2
%    * ncbi_galg_ncbi_ensp/2
%    * ncbi_galg_ncbi_symb/2
%    * ncbi_galg_nsyn_symb/2
%    * ncbi_galg_rnuc_symb/2
%  * chicken/reac.pl
%    * reac_galg_ncbi_reac/2
%    * reac_galg_ncbi_reap/3
%    * reac_galg_reac_reap/3
%    * reac_galg_reac_recl/2
%    * reac_galg_reac_recn/2
%    * reac_galg_reap_repn/2
%  * chicken/strg.pl
%    * strg_galg_edge_ensp/3
%    * strg_galg_edge_symb/3
%    * strg_galg_ensp_symb/2
%  * chicken/unip.pl
%    * unip_galg_unip_ensp/2
%    * unip_galg_unip_ncbi/2
%    * unip_galg_unip_gyno/2
%    * unip_galg_unip_strp/2
%    * unip_galg_unip_symb/2
%  * human/ense.pl
%    * ense_homs_ensg_hgnc/2
%    * ense_homs_ensg_symb/2
%    * ense_homs_enst_chrl/5
%    * ense_homs_ensg_chrl/5
%    * ense_homs_enst_ensg/2
%  * human/gont.pl
%    * gont_homs_edge_gisa/2
%    * gont_homs_edge_greg/2
%    * gont_homs_edge_gprg/2
%    * gont_homs_edge_gnrg/2
%    * gont_homs_edge_gpof/2
%    * gont_homs_gont_symb/3
%    * gont_homs_gont_gonm/2
%    * gont_homs_symb_gont/3
%  * human/hgnc.pl
%    * hgnc_homs_ccds_hgnc/2
%    * hgnc_homs_ensg_hgnc/2
%    * hgnc_homs_ncbi_hgnc/2
%    * hgnc_homs_ncbi_symb/2
%    * hgnc_homs_hgnc_ccds/2
%    * hgnc_homs_hgnc_chrb/2
%    * hgnc_homs_hgnc_ensg/2
%    * hgnc_homs_hgnc_ncbi/2
%    * hgnc_homs_hgnc_name/2
%    * hgnc_homs_hgnc_symb/2
%    * hgnc_homs_prev_symb/2
%    * hgnc_homs_symb_ncbi/2
%    * hgnc_homs_symb_hgnc/2
%    * hgnc_homs_syno_symb/2
%  * human/ncbi.pl
%    * ncbi_homs_dnuc_symb/2
%    * ncbi_homs_ncbi_ensg/2
%    * ncbi_homs_ncbi_ensp/2
%    * ncbi_homs_ncbi_symb/2
%    * ncbi_homs_nsyn_symb/2
%    * ncbi_homs_rnuc_symb/2
%  * human/pros.pl
%    * pros_homs_pros_prsn/2
%    * pros_homs_pros_sprt/7
%  * human/reac.pl
%    * reac_homs_ncbi_reac/2
%    * reac_homs_ncbi_reap/3
%    * reac_homs_reac_reap/3
%    * reac_homs_reac_recl/2
%    * reac_homs_reac_recn/2
%    * reac_homs_reap_repn/2
%  * human/strg.pl
%    * strg_homs_edge_ensp/3
%    * strg_homs_edge_symb/3
%  * human/unip.pl
%    * unip_homs_hgnc_unip/2
%    * unip_homs_ensp_unip/2
%    * unip_homs_trem_nucs/2
%    * unip_homs_unip_ncbi/2
%    * unip_homs_unip_hgnc/2
%    * unip_homs_sprt_seqn/2
%    * unip_homs_trem_seqn/2
%  * mouse/ense.pl
%    * ense_musm_ensg_mgim/2
%    * ense_musm_ensg_symb/2
%    * ense_musm_enst_chrl/5
%    * ense_musm_ensg_chrl/5
%    * ense_musm_enst_ensg/2
%  * mouse/gont.pl
%    * gont_musm_mgim_gont/4
%    * gont_musm_gont_symb/4
%  * mouse/mgim.pl
%    * mgim_musm_mgim_chrl/5
%    * mgim_musm_mgim_genb/2
%    * mgim_musm_mgim_mnme/2
%    * mgim_musm_mgim_mrks/2
%    * mgim_musm_mgim_ncbi/2
%    * mgim_musm_mgim_symb/2
%    * mgim_musm_mgim_unip/2
%    * mgim_musm_mrks_wdra/2
%    * mgim_musm_msyn_mgim/2
%  * mouse/ncbi.pl
%    * ncbi_musm_dnuc_symb/2
%    * ncbi_musm_ncbi_ensg/2
%    * ncbi_musm_ncbi_ensp/2
%    * ncbi_musm_ncbi_symb/2
%    * ncbi_musm_nsyn_symb/2
%    * ncbi_musm_rnuc_symb/2
%  * mouse/reac.pl
%    * reac_musm_ncbi_reac/2
%    * reac_musm_ncbi_reap/2
%    * reac_musm_reac_reap/3
%    * reac_musm_reac_recl/2
%    * reac_musm_reac_recn/2
%    * reac_musm_reap_repn/2
%  * mouse/strg.pl
%    * strg_musm_edge_ensp/3
%    * strg_musm_edge_symb/3
%  * mouse/unip.pl
%    * unip_musm_ensp_unip/2
%    * unip_musm_mgim_unip/2
%    * unip_musm_trem_nucs/2
%    * unip_musm_unip_ncbi/2
%    * unip_musm_unip_symb/2
%    * unip_musm_gyno_unip/2
%  * multi/ncbi.pl
%    * ncbi_mult_taxo_scnm/2
%    * ncbi_mult_taxo_gbnm/2
%  * multi/vgnc.pl
%    * vgnc_mult_vgnc_name/3
%    * vgnc_mult_vgnc_symb/3
%  * pig/ense.pl
%    * ense_suss_ensg_chrl/5
%    * ense_suss_ensg_symb/2
%    * ense_suss_enst_chrl/5
%    * ense_suss_enst_ensg/2
%  * pig/gont.pl
%    * gont_suss_symb_gont/4
%  * pig/ncbi.pl
%    * ncbi_suss_dnuc_symb/2
%    * ncbi_suss_ncbi_ensg/2
%    * ncbi_suss_ncbi_ensp/2
%    * ncbi_suss_ncbi_symb/2
%    * ncbi_suss_nsyn_symb/2
%    * ncbi_suss_rnuc_symb/2
%  * pig/strg.pl
%    * strg_suss_edge_ensp/3
%    * strg_suss_edge_symb/3
%    * strg_suss_ensp_symb/2
%  * pig/vgnc.pl
%    * vgnc_suss_vgnc_ensg/2
%    * vgnc_suss_vgnc_name/2
%    * vgnc_suss_vgnc_ncbi/2
%    * vgnc_suss_vgnc_symb/2
%
% Generated on 24.04.05 from bio_db with pack.pl version: 4.4.
% 
%
bio_db_data_predicate(cgnc_galg_cgnc_curs, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(cgnc_galg_cgnc_edat, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(cgnc_galg_cgnc_ensg, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(cgnc_galg_cgnc_name, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(cgnc_galg_cgnc_ncbi, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(cgnc_galg_cgnc_symb, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(cgnc_galg_cgnc_syno, 2, chicken, 'galg/cgnc.pl').
bio_db_data_predicate(ense_galg_ensg_chrl, 5, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_galg_ensg_symb, 2, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_galg_enst_chrl, 5, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_galg_enst_ensg, 2, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_gg6a_ensg_chrl, 5, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_gg6a_ensg_symb, 2, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_gg6a_enst_chrl, 5, chicken, 'galg/ense.pl').
bio_db_data_predicate(ense_gg6a_enst_ensg, 2, chicken, 'galg/ense.pl').
bio_db_data_predicate(gont_galg_symb_gont, 4, chicken, 'galg/gont.pl').
bio_db_data_predicate(ncbi_galg_dnuc_symb, 2, chicken, 'galg/ncbi.pl').
bio_db_data_predicate(ncbi_galg_ncbi_ensg, 2, chicken, 'galg/ncbi.pl').
bio_db_data_predicate(ncbi_galg_ncbi_ensp, 2, chicken, 'galg/ncbi.pl').
bio_db_data_predicate(ncbi_galg_ncbi_symb, 2, chicken, 'galg/ncbi.pl').
bio_db_data_predicate(ncbi_galg_nsyn_symb, 2, chicken, 'galg/ncbi.pl').
bio_db_data_predicate(ncbi_galg_rnuc_symb, 2, chicken, 'galg/ncbi.pl').
bio_db_data_predicate(reac_galg_ncbi_reac, 2, chicken, 'galg/reac.pl').
bio_db_data_predicate(reac_galg_ncbi_reap, 3, chicken, 'galg/reac.pl').
bio_db_data_predicate(reac_galg_reac_reap, 3, chicken, 'galg/reac.pl').
bio_db_data_predicate(reac_galg_reac_recl, 2, chicken, 'galg/reac.pl').
bio_db_data_predicate(reac_galg_reac_recn, 2, chicken, 'galg/reac.pl').
bio_db_data_predicate(reac_galg_reap_repn, 2, chicken, 'galg/reac.pl').
bio_db_data_predicate(strg_galg_edge_ensp, 3, chicken, 'galg/strg.pl').
bio_db_data_predicate(strg_galg_edge_symb, 3, chicken, 'galg/strg.pl').
bio_db_data_predicate(strg_galg_ensp_symb, 2, chicken, 'galg/strg.pl').
bio_db_data_predicate(unip_galg_unip_ensp, 2, chicken, 'galg/unip.pl').
bio_db_data_predicate(unip_galg_unip_gyno, 2, chicken, 'galg/unip.pl').
bio_db_data_predicate(unip_galg_unip_ncbi, 2, chicken, 'galg/unip.pl').
bio_db_data_predicate(unip_galg_unip_strp, 2, chicken, 'galg/unip.pl').
bio_db_data_predicate(unip_galg_unip_symb, 2, chicken, 'galg/unip.pl').
bio_db_data_predicate(ense_homs_ensg_chrl, 5, human, 'homs/ense.pl').
bio_db_data_predicate(ense_homs_ensg_hgnc, 2, human, 'homs/ense.pl').
bio_db_data_predicate(ense_homs_ensg_symb, 2, human, 'homs/ense.pl').
bio_db_data_predicate(ense_homs_enst_chrl, 5, human, 'homs/ense.pl').
bio_db_data_predicate(ense_homs_enst_ensg, 2, human, 'homs/ense.pl').
bio_db_data_predicate(gont_homs_edge_gisa, 2, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_edge_gnrg, 2, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_edge_gpof, 2, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_edge_gprg, 2, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_edge_greg, 2, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_gont_gonm, 2, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_gont_symb, 3, human, 'homs/gont.pl').
bio_db_data_predicate(gont_homs_symb_gont, 3, human, 'homs/gont.pl').
bio_db_data_predicate(hgnc_homs_ccds_hgnc, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_ensg_hgnc, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_hgnc_ccds, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_hgnc_chrb, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_hgnc_ensg, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_hgnc_name, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_hgnc_ncbi, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_hgnc_symb, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_ncbi_hgnc, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_ncbi_symb, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_prev_symb, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_symb_hgnc, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_symb_ncbi, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(hgnc_homs_syno_symb, 2, human, 'homs/hgnc.pl').
bio_db_data_predicate(ncbi_homs_dnuc_symb, 2, human, 'homs/ncbi.pl').
bio_db_data_predicate(ncbi_homs_ncbi_ensg, 2, human, 'homs/ncbi.pl').
bio_db_data_predicate(ncbi_homs_ncbi_ensp, 2, human, 'homs/ncbi.pl').
bio_db_data_predicate(ncbi_homs_ncbi_symb, 2, human, 'homs/ncbi.pl').
bio_db_data_predicate(ncbi_homs_nsyn_symb, 2, human, 'homs/ncbi.pl').
bio_db_data_predicate(ncbi_homs_rnuc_symb, 2, human, 'homs/ncbi.pl').
bio_db_data_predicate(pros_homs_pros_prsn, 2, human, 'homs/pros.pl').
bio_db_data_predicate(pros_homs_pros_sprt, 7, human, 'homs/pros.pl').
bio_db_data_predicate(reac_homs_ncbi_reac, 2, human, 'homs/reac.pl').
bio_db_data_predicate(reac_homs_ncbi_reap, 3, human, 'homs/reac.pl').
bio_db_data_predicate(reac_homs_reac_reap, 3, human, 'homs/reac.pl').
bio_db_data_predicate(reac_homs_reac_recl, 2, human, 'homs/reac.pl').
bio_db_data_predicate(reac_homs_reac_recn, 2, human, 'homs/reac.pl').
bio_db_data_predicate(reac_homs_reap_repn, 2, human, 'homs/reac.pl').
bio_db_data_predicate(strg_homs_edge_ensp, 3, human, 'homs/strg.pl').
bio_db_data_predicate(strg_homs_edge_symb, 3, human, 'homs/strg.pl').
bio_db_data_predicate(unip_homs_ensp_unip, 2, human, 'homs/unip.pl').
bio_db_data_predicate(unip_homs_hgnc_unip, 2, human, 'homs/unip.pl').
bio_db_data_predicate(unip_homs_sprt_seqn, 2, human, 'homs/unip.pl').
bio_db_data_predicate(unip_homs_trem_nucs, 2, human, 'homs/unip.pl').
bio_db_data_predicate(unip_homs_trem_seqn, 2, human, 'homs/unip.pl').
bio_db_data_predicate(unip_homs_unip_hgnc, 2, human, 'homs/unip.pl').
bio_db_data_predicate(unip_homs_unip_ncbi, 2, human, 'homs/unip.pl').
bio_db_data_predicate(ense_musm_ensg_chrl, 5, mouse, 'musm/ense.pl').
bio_db_data_predicate(ense_musm_ensg_mgim, 2, mouse, 'musm/ense.pl').
bio_db_data_predicate(ense_musm_ensg_symb, 2, mouse, 'musm/ense.pl').
bio_db_data_predicate(ense_musm_enst_chrl, 5, mouse, 'musm/ense.pl').
bio_db_data_predicate(ense_musm_enst_ensg, 2, mouse, 'musm/ense.pl').
bio_db_data_predicate(gont_musm_gont_symb, 4, mouse, 'musm/gont.pl').
bio_db_data_predicate(gont_musm_mgim_gont, 4, mouse, 'musm/gont.pl').
bio_db_data_predicate(mgim_musm_mgim_chrl, 5, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mgim_genb, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mgim_mnme, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mgim_mrks, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mgim_ncbi, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mgim_symb, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mgim_unip, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_mrks_wdra, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(mgim_musm_msyn_mgim, 2, mouse, 'musm/mgim.pl').
bio_db_data_predicate(ncbi_musm_dnuc_symb, 2, mouse, 'musm/ncbi.pl').
bio_db_data_predicate(ncbi_musm_ncbi_ensg, 2, mouse, 'musm/ncbi.pl').
bio_db_data_predicate(ncbi_musm_ncbi_ensp, 2, mouse, 'musm/ncbi.pl').
bio_db_data_predicate(ncbi_musm_ncbi_symb, 2, mouse, 'musm/ncbi.pl').
bio_db_data_predicate(ncbi_musm_nsyn_symb, 2, mouse, 'musm/ncbi.pl').
bio_db_data_predicate(ncbi_musm_rnuc_symb, 2, mouse, 'musm/ncbi.pl').
bio_db_data_predicate(reac_musm_ncbi_reac, 2, mouse, 'musm/reac.pl').
bio_db_data_predicate(reac_musm_ncbi_reap, 2, mouse, 'musm/reac.pl').
bio_db_data_predicate(reac_musm_reac_reap, 3, mouse, 'musm/reac.pl').
bio_db_data_predicate(reac_musm_reac_recl, 2, mouse, 'musm/reac.pl').
bio_db_data_predicate(reac_musm_reac_recn, 2, mouse, 'musm/reac.pl').
bio_db_data_predicate(reac_musm_reap_repn, 2, mouse, 'musm/reac.pl').
bio_db_data_predicate(strg_musm_edge_ensp, 3, mouse, 'musm/strg.pl').
bio_db_data_predicate(strg_musm_edge_symb, 3, mouse, 'musm/strg.pl').
bio_db_data_predicate(unip_musm_ensp_unip, 2, mouse, 'musm/unip.pl').
bio_db_data_predicate(unip_musm_gyno_unip, 2, mouse, 'musm/unip.pl').
bio_db_data_predicate(unip_musm_mgim_unip, 2, mouse, 'musm/unip.pl').
bio_db_data_predicate(unip_musm_trem_nucs, 2, mouse, 'musm/unip.pl').
bio_db_data_predicate(unip_musm_unip_ncbi, 2, mouse, 'musm/unip.pl').
bio_db_data_predicate(unip_musm_unip_symb, 2, mouse, 'musm/unip.pl').
bio_db_data_predicate(ncbi_mult_taxo_gbnm, 2, multi, 'mult/ncbi.pl').
bio_db_data_predicate(ncbi_mult_taxo_scnm, 2, multi, 'mult/ncbi.pl').
bio_db_data_predicate(vgnc_mult_vgnc_name, 3, multi, 'mult/vgnc.pl').
bio_db_data_predicate(vgnc_mult_vgnc_symb, 3, multi, 'mult/vgnc.pl').
bio_db_data_predicate(ense_suss_ensg_chrl, 5, pig, 'suss/ense.pl').
bio_db_data_predicate(ense_suss_ensg_symb, 2, pig, 'suss/ense.pl').
bio_db_data_predicate(ense_suss_enst_chrl, 5, pig, 'suss/ense.pl').
bio_db_data_predicate(ense_suss_enst_ensg, 2, pig, 'suss/ense.pl').
bio_db_data_predicate(gont_suss_symb_gont, 4, pig, 'suss/gont.pl').
bio_db_data_predicate(ncbi_suss_dnuc_symb, 2, pig, 'suss/ncbi.pl').
bio_db_data_predicate(ncbi_suss_ncbi_ensg, 2, pig, 'suss/ncbi.pl').
bio_db_data_predicate(ncbi_suss_ncbi_ensp, 2, pig, 'suss/ncbi.pl').
bio_db_data_predicate(ncbi_suss_ncbi_symb, 2, pig, 'suss/ncbi.pl').
bio_db_data_predicate(ncbi_suss_nsyn_symb, 2, pig, 'suss/ncbi.pl').
bio_db_data_predicate(ncbi_suss_rnuc_symb, 2, pig, 'suss/ncbi.pl').
bio_db_data_predicate(strg_suss_edge_ensp, 3, pig, 'suss/strg.pl').
bio_db_data_predicate(strg_suss_edge_symb, 3, pig, 'suss/strg.pl').
bio_db_data_predicate(strg_suss_ensp_symb, 2, pig, 'suss/strg.pl').
bio_db_data_predicate(vgnc_suss_vgnc_ensg, 2, pig, 'suss/vgnc.pl').
bio_db_data_predicate(vgnc_suss_vgnc_name, 2, pig, 'suss/vgnc.pl').
bio_db_data_predicate(vgnc_suss_vgnc_ncbi, 2, pig, 'suss/vgnc.pl').
bio_db_data_predicate(vgnc_suss_vgnc_symb, 2, pig, 'suss/vgnc.pl').
