
:- set_prolog_flag(allow_dot_in_atom,false).
:- set_prolog_flag(stack_limit, 8 000 000 000).

:- use_module(library(csv)).      % csv_read_file/2.
:- use_module(library(lists)).    % member/2.
:- use_module(library(apply)).    % maplist/4.
% :- use_module(library(debug)).    % /1,3. -> debug_call
:- use_module(library(listing)).  % portray_clause/2.
:- use_module(library(readutil)). % read_line_to_codes/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib). 
:- lib(options).
:- lib(debug_call).                     % debuc/1,3.
:- lib(stoics_lib:map_list_options/3).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(map_uniprot/4).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).             % bio_db_add_infos_to/2.
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).

% use this if from outside europe:
% unip_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz' ).
% trem_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping_selected.tab.gz' ).

std_mouse_maps_unip_defaults( Defs ) :-
                                        Defs = [ db(unip),
                                                 debug(true),
                                                 debug_fetch(true),
                                                 debug_url(false),
                                                 org(mouse),
                                                 unip_file_full('MOUSE_10090_idmapping.dat.gz'),
                                                 unip_file_sele('MOUSE_10090_idmapping_selected.tab.gz')
                                        ].

/** std_mouse_maps_unip(+Opts).

Create some uniprot maps.

Opts
  * db(Db=unip)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Ubg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * org(Org=mouse)
    organism
  * unip_file_full(Uff='MOUSE_10090_idmapping.dat.gz'
    remote file name for Unip protein downloads (all proteins)
  * unip_file_sele(Ufs='MOUSE_10090_idmapping_selected.tab.gz')
    remote file name for Unip downloads (selected, annotated proteins- subset of Uff)

==
?- maps_std_uniprot.
?- shell( 'wc -l uniprot_*' ).

ορέστης;build_repo/mouse% date ; pupsh std_mouse_maps_unip.pl ; date
Tue 27 Dec 16:05:52 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
% Loc: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/unip'
...
Tue 27 Dec 16:06:57 GMT 2022

ορέστης;unip/maps% date 
Tue 27 Dec 16:08:45 GMT 2022
ορέστης;unip/maps% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/unip/maps
ορέστης;unip/maps% wc -l *_m*
   67172 unip_musm_ensp_unip.pl
   28758 unip_musm_gyno_unip.pl
   80455 unip_musm_mgim_unip.pl
  129676 unip_musm_trem_nucs.pl
   32782 unip_musm_unip_ncbi.pl
   83182 unip_musm_unip_symb.pl
  422025 total
==

@author nicos angelopoulos
@version  0.2 2015/4/27
@version  0.3 2023/9/25 use more local libs and control via options

*/
std_mouse_maps_unip( Args ) :-
     Self = std_mouse_maps_unip,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnDir, Opts ),
     /* double check unip part works with the nucl part // 15.05.15 */
     working_directory( Old, DnDir ),
     % unip_mouse( Url ),
     bio_db_source_url( Url, [debug_url-debug,unip_file_full-url_file], Opts ),
     % url_file( Url, 
     UrlOpts = [debug(true),interface(wget),dnld_file(File)],
     options( debug_fetch(Fbg), Opts ),
     url_file_local_date_mirror( Url, DnDir, [debug(Fbg)|UrlOpts] ),
     os_make_path( maps, debug(true) ),
     debuc( Self, 'Dir location: ~p', DnDir ),
     Rev = [id_map('MOUSE_10090_idmapping.dat'),org(mouse),f_call(de_vers),interface(prolog),reverse(true)],
     map_uniprot( 'Ensembl_PRO', Csv, [EnspF], Rev ),
     Sem = [interface(prolog),f_call(de_semi('MGI')),reverse(true),id_map('MOUSE_10090_idmapping.dat'),org(mouse)],
     map_uniprot( 'MGI', Csv, [MgiF], Sem ),

     Ifc = [id_map('MOUSE_10090_idmapping.dat'),org(mouse),interface(prolog)],
     Rfc = [id_map('MOUSE_10090_idmapping.dat'),org(mouse),interface(prolog),reverse(true)],

     map_uniprot( 'GeneID', Csv, [EtzF], Ifc ),
     % map_uniprot( 'UniGene', Csv, [UniGF], Ifc ),
     map_uniprot( 'Gene_Name', Csv, [SymbF], Ifc ),
     map_uniprot( 'Gene_Synonym', Csv, [GynoF], Rfc ),

     % Files = [HgncF,FromHgncF,EtzF,UniGF,EnspF],
     % Files = [MgiF,EtzF,UniGF,EnspF,SymbF,GynoF],
     Files = [MgiF,EtzF,EnspF,SymbF,GynoF],
     Cpts = [org(mouse),type(maps)],
     map_list_options( link_to_bio_sub(unip), Files, call_options(Cpts) ),

     bio_db_dnt_times( File, SwDnDt, _SwDnEn ),
     SwOpts = [source(Url),datetime(SwDnDt)],
     bio_db_add_infos_to( [header(row('Ensembl_Protein','UniProt'))|SwOpts], 'maps/unip_musm_ensp_unip.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','Entrez_ID'))|SwOpts], 'maps/unip_musm_unip_ncbi.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','MGI'))|SwOpts], 'maps/unip_musm_mgim_unip.pl' ),
     % bio_db_add_infos_to( [header(row('Uni Protein','HGNC ID'))|SwOpts], 'maps/map_unip_mouse_unip_hgnc.pl' ),
     % Unigene has been discontinued
     %bio_db_add_infos_to( [header(row('Uni_Protein','Uni_Gene'))|SwOpts], 'maps/map_unip_mouse_unip_unig.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','Symbol'))|SwOpts], 'maps/unip_musm_unip_symb.pl' ),
     bio_db_add_infos_to( [header(row('Symbol','Uni_Protein'))|SwOpts], 'maps/unip_musm_gyno_unip.pl' ),
     working_directory( _, DnDir ),
     bio_db_source_url( TremUrl, [debug_url-debug,unip_file_sele-url_file], Opts ),
     % 15.05.14 adding support for treMBL, at least that 's what i think the selected file is all about
     TrUrlOpts = [debug(true),interface(wget),dnld_file(TrFile)],
     url_file_local_date_mirror( TremUrl, DnDir, TrUrlOpts ),
     bio_db_dnt_times( TrFile, TrDnDt, _TrDnEn ),
     os_make_path( maps, afresh(false) ),
     os_make_path( trembl, afresh(true) ),
     directory_file_path( _, TremFile, TremUrl ),
     % @ pwd(),
     directory_file_path( trembl, TremFile, TremTrg ),
     copy_file( TremFile, TremTrg ),
     working_directory( _, trembl ), 
     file_name_extension( TremDatF, gz, TremFile ),
     atom_concat( 'gunzip ', TremFile, Gunzip ),
     debuc( Self, 'Gunzipping: ~p', TremFile ),
     shell( Gunzip ),
     csv_read_file( TremDatF, TremRows, [separator(0'\t)] ),
     debuc( Self, length, trem_rows/TremRows ),
     % 17/22
     findall( unip_musm_trem_nucs(TremId,Nucs), (
                       member(TremRow,TremRows), arg(1,TremRow,TremId), \+ empty(TremId), 
                       arg(17,TremRow,NucsConcat), \+ empty(NucsConcat), 
                       atomic_list_concat(NucsList,'; ',NucsConcat),
                       member(Nucs,NucsList)
                                ), 
                                      TNRows ),
     debuc( Self, length, tn_len/TNRows ),
     sort( TNRows, TNOrdRows ),
     open( '../maps/unip_musm_trem_nucs.pl', write, TNOut ),
     maplist( portray_clause(TNOut), TNOrdRows ),
     close( TNOut ),
     working_directory( _, '../maps' ),
     link_to_bio_sub( unip, 'unip_musm_trem_nucs.pl', [org(mouse),type(maps)] ),

     TrOpts = [source(TremUrl),datetime(TrDnDt),header(row('treMBLE_Protein','Nucleotide_Sequence'))],
     bio_db_add_infos_to( TrOpts, 'unip_musm_trem_nucs.pl' ),
     % run this manually, it is a biggie: 
     % uniprot_sprot.dat is 2.9 G
     % std_map_usyn_unip,
     %
     working_directory( _, Old ).

empty( '' ).
     
/*
std_map_usyn_unip :-
     open( '/media/nicos/lmtk3/downloads/uniprot_sprot.dat', read, In ),
     read_line_to_codes( In, Line ),
     sprot_synonym_rows( Line, In, SynRs ),
     debug_call( uniprot, length, syn_rows/SynRs ),
     close( In ),
     sort( SynRs, OrdRs ),
     unip_dnload( DnDir ),
     os_path( DnDir, maps, MapsD ),
     Opts = [prefix(unip),dir(MapsD)],
     csv_ids_map( _, usyn, unip, [row(usyn,unip)|OrdRs], MapF, Opts ),
     os_path( MapsD, MapF, AbsMapF ),
     % link_to_map_sub( unip, AbsMapF ).
    link_to_bio_sub( unip, AbsMapF, [org(mouse),type(maps)] ).
    */

sprot_synonym_rows( end_of_file, _In, [] ) :- !.
sprot_synonym_rows( Line, In, SynRs ) :-
     atom_codes( Atom, Line ),
     ( atom_concat('AC   ',SynsPsf,Atom) ->
          atom_concat(SynsAtom,';',SynsPsf),
          atomic_list_concat(Syns,'; ',SynsAtom),
          Syns = [Cannon|Ryns],
          sport_prot_synonym_rows( Ryns, Cannon, SynRs, TSynRs )
          ;
          TSynRs = SynRs
     ),
     read_line_to_codes( In, Codes ),
     sprot_synonym_rows( Codes, In, TSynRs ).
     
sport_prot_synonym_rows( [], _Cannon, SynRs, SynRs ).
sport_prot_synonym_rows( [H|T], Cannon, [row(H,Cannon)|TSynRs], SynRs ) :-
     sport_prot_synonym_rows( T, Cannon, TSynRs, SynRs ).

os_rm_rf( Dir ) :-
     exists_directory(Dir) ->
     delete_directory_and_contents(Dir),
     !.
os_rm_rf( _Dir ).

de_vers( Prv, Cln ) :-
     atomic_list_concat( [Cln,Ver], '.', Prv ),
     atom_number( Ver, _ ),
     !.
de_vers( Cln, Cln ).

de_semi( Pfx, AccPrv, Acc ) :-
     atomic_list_concat( [Pfx,AccAtm], ':', AccPrv ), 
     atom_number( AccAtm, Acc ),
     !.
de_semi( Pfx, AccPrv, _Acc ) :-
     write( de_semi_disaster(Pfx,AccPrv) ), nl,
     abort.
