
:- set_prolog_flag(allow_dot_in_atom,false).
:- set_prolog_flag(stack_limit, 12 000 000 000).

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

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(map_uniprot/4).
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2.
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).
:- lib(stoics_lib:map_list_options/3).

% unip_chicken( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/CHICK_9031_idmapping.dat.gz' ).
% trem_chicken( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/CHICK_9031_idmapping_selected.tab.gz' ).
% use this if from outside europe:
% unip_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz' ).
%trem_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping_selected.tab.gz' ).

unip_dnload( Self, Loc ) :-
     absolute_file_name( bio_db_build_downloads(unip), Loc ),
     debuc( Self, 'Loc: ~p', Loc ),
     os_make_path( Loc, debug(true) ).

std_chicken_maps_unip_defaults( [ db(unip),
                                  debug(true),
                                  debug_url(false),
                                  debug_fetch(true),
                                  iactive(true),
                                  org(chicken),
                                  unip_file_full('CHICK_9031_idmapping.dat.gz'),
                                  unip_file_sele('HUMAN_9031_idmapping_selected.tab.gz')
                                ]
                              ).

/** std_chicken_maps_unip(Opts).

Create bio_db uniprot maps.

Opts
  * db(Db=unip)
    source database
  * debug(Dbg=true)
    progress, informational messages
  * debug_fetch(Ubg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * org(Org=chicken)
    organism
  * unip_file_full(UnipFF='HUMAN_9606_idmapping.dat.gz')
    the file name for the id mapping download
  * unip_file_sele(UnipFF='HUMAN_9606_idmapping.dat.gz')
    the file name for the selected ids mapping download (stricter than the above)

==
?- std_chicken_maps_unip([]).

ορέστης;unip/maps% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/unip/maps
ορέστης;unip/maps% date
Tue 27 Dec 12:58:09 GMT 2022
ορέστης;unip/maps% wc -l *gal*
  73128 unip_galg_unip_ensp.pl
   1862 unip_galg_unip_gyno.pl
  12697 unip_galg_unip_ncbi.pl
  13189 unip_galg_unip_strp.pl
  61681 unip_galg_unip_symb.pl
 162557 total

@author nicos angelopoulos
@version  0.1 2022/12/20

*/
std_chicken_maps_unip( Args ) :-
     Self = std_chicken_maps_unip,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     % unip_dnload( Self, DnDir ),  %
     /* double check unip part works with the nucl part // 15.05.15 */
     build_dnload_loc( Self, DnDir, Opts ),
     working_directory( Old, DnDir ),

     % unip_chicken( Url ),
     bio_db_source_url( Url, [debug_url-debug,unip_file_full-url_file], Opts ),
     options( debug_fetch(Fbg), Opts ),
     UrlOpts = [debug(Fbg),dnld_file(File)|Opts],
     url_file_local_date_mirror( Url, DnDir, UrlOpts ),
     % cd( bio_dn_root(uniprot) ),
     % os_rm_rf( maps ), % don't do that human puts stuff there tooo ! 
     os_make_path( maps, debug(true) ),
     debuc( Self, 'Dir location: ~p', DnDir ),
     Rev = [id_map('CHICK_9031_idmapping.dat'),f_call(de_pfx_dot),org(galg),interface(prolog),reverse(false)],
     map_uniprot( 'Ensembl_PRO', Csv, [EnspF], Rev ),

     % Fgnc = [interface(prolog),f_call(de_semi('HGNC'))],
     % map_uniprot( 'HGNC', Csv, [FromHgncF], Fgnc ),
     Sem = [interface(prolog),f_call(de_semi_dot('9031')),reverse(false),id_map('CHICK_9031_idmapping.dat'),org(galg)],
     map_uniprot( 'STRING', Csv, [StrF], Sem ),

     Ifc = [id_map('CHICK_9031_idmapping.dat'),org(galg),interface(prolog)],
     Rfc = [id_map('CHICK_9031_idmapping.dat'),org(galg),interface(prolog),reverse(false)],

     map_uniprot( 'GeneID', Csv, [NcbF], Ifc ),
     % map_uniprot( 'UniGene', Csv, [UniGF], Ifc ),
     map_uniprot( 'Gene_Name', Csv, [SymbF], Ifc ),
     map_uniprot( 'Gene_Synonym', Csv, [GynoF], Rfc ),

     % Files = [HgncF,FromHgncF,EtzF,UniGF,EnspF],
     % Files = [MgiF,EtzF,UniGF,EnspF,SymbF,GynoF],
     Files = [StrF,NcbF,EnspF,SymbF,GynoF],
     % working_directory( _, maps ),
     % maplist( link_to_map_sub(unip), Files ),
    Cpts = [org(galg),type(maps)],
    map_list_options( link_to_bio_sub(unip), Files, call_options(Cpts) ),

     bio_db_dnt_times( File, SwDnDt, _SwDnEn ),
     SwOpts = [source(Url),datetime(SwDnDt)],
     bio_db_add_infos_to( [header(row('UniProt','Ensembl_Protein'))|SwOpts], 'maps/map_unip_galg_unip_ensp.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','NCBI_ID'))|SwOpts], 'maps/map_unip_galg_unip_ncbi.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','String Protein'))|SwOpts], 'maps/map_unip_galg_unip_strp.pl' ),
     % bio_db_add_infos_to( [header(row('Uni Protein','HGNC ID'))|SwOpts], 'maps/map_unip_mouse_unip_hgnc.pl' ),
     % Unigene has been discontinued
     %bio_db_add_infos_to( [header(row('Uni_Protein','Uni_Gene'))|SwOpts], 'maps/map_unip_mouse_unip_unig.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','Symbol'))|SwOpts], 'maps/map_unip_galg_unip_symb.pl' ),
     bio_db_add_infos_to( [header(row('Uni_Protein','Symbol Synonym'))|SwOpts], 'maps/map_unip_galg_unip_gyno.pl' ),
     working_directory( _, Old ).

empty( '' ).
     
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
    link_to_bio_sub( unip, AbsMapF, [org(chicken),type(maps)] ).

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

de_pfx_dot( In, Out ) :-
     at_con( [Out|_], '.', In ),
     !.
de_pfx_dot( In, _Acc ) :-
     write( de_pfx_dot_disaster(In) ), nl,
     abort.

de_semi_dot( Pfx, AccPrv, Acc ) :-
     atomic_list_concat( [Pfx,Acc], '.', AccPrv ), 
     % atom_number( AccAtm, Acc ),
     !.
de_semi_dot( Pfx, AccPrv, _Acc ) :-
     write( de_semi_dot_disaster(Pfx,AccPrv) ), nl,
     abort.

de_semi( Pfx, AccPrv, Acc ) :-
     atomic_list_concat( [Pfx,AccAtm], ':', AccPrv ), 
     atom_number( AccAtm, Acc ),
     !.
de_semi( Pfx, AccPrv, _Acc ) :-
     write( de_semi_disaster(Pfx,AccPrv) ), nl,
     abort.
