
:- set_prolog_flag(allow_dot_in_atom,false).
:- set_prolog_flag(stack_limit, 8 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib). 
:- lib(options).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- ensure_loaded('../hs/src/map_uniprot').  % /4.
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2.
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).

:- debug(std_mouse_maps_unip). % fixme:
:- debug(link_to_map_sub).

unip_mouse( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/MOUSE_10090_idmapping.dat.gz' ).
trem_mouse( 'ftp://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/MOUSE_10090_idmapping_selected.tab.gz' ).
% use this if from outside europe:
% unip_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping.dat.gz' ).
%trem_hs( 'ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping_selected.tab.gz' ).

unip_dnload( Self, Loc ) :-
	absolute_file_name( bio_db_build_downloads(unip), Loc ),
    debug( Self, 'Loc: ~p', Loc ),
	os_make_path( Loc, debug(true) ).

std_mouse_maps_unip_defaults(debug(true)).

%% maps_std_uniprot.
%
% Create some uniprot maps.
%
%==
% ?- maps_std_uniprot.
% ?- shell( 'wc -l uniprot_*' ).
%==
%
% @author nicos angelopoulos
% @version  0.2 2015/4/27
% @tbd use hgnc as template to download from 
% @tbd ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/
%
std_mouse_maps_unip( Args ) :-
    Self = std_mouse_maps_unip,
    options_append( Self, Args, Opts ),
	bio_db_build_aliases( Opts ),
	unip_dnload( Self, DnDir ),  %
	/* double check unip part works with the nucl part // 15.05.15 */
	working_directory( Old, DnDir ),
	unip_mouse( Url ),
	% url_file( Url, 
	UrlOpts = [debug(url_local),interface(wget),file(File)],
	url_file_local_date_mirror( Url, DnDir, UrlOpts ),
	% cd( bio_dn_root(uniprot) ),
	% os_rm_rf( maps ), % don't do that human puts stuff there tooo ! 
	os_make_path( maps, debug(true) ),

	debug( Self, 'Dir location: ~p', DnDir ),
	Rev = [uniprot('MOUSE_10090_idmapping.dat'),org(mouse),interface(prolog),reverse(true)],
	map_uniprot( 'Ensembl_PRO', Csv, [EnspF], Rev ),

	% Fgnc = [interface(prolog),f_call(de_semi('HGNC'))],
	% map_uniprot( 'HGNC', Csv, [FromHgncF], Fgnc ),
	Sem = [interface(prolog),f_call(de_semi('MGI')),reverse(true),uniprot('MOUSE_10090_idmapping.dat'),org(mouse)],
	map_uniprot( 'MGI', Csv, [MgiF], Sem ),

	Ifc = [uniprot('MOUSE_10090_idmapping.dat'),org(mouse),interface(prolog)],
	Rfc = [uniprot('MOUSE_10090_idmapping.dat'),org(mouse),interface(prolog),reverse(true)],

	map_uniprot( 'GeneID', Csv, [EtzF], Ifc ),
	% map_uniprot( 'UniGene', Csv, [UniGF], Ifc ),
	map_uniprot( 'Gene_Name', Csv, [SymbF], Ifc ),
	map_uniprot( 'Gene_Synonym', Csv, [GynoF], Rfc ),

	% Files = [HgncF,FromHgncF,EtzF,UniGF,EnspF],
	% Files = [MgiF,EtzF,UniGF,EnspF,SymbF,GynoF],
	Files = [MgiF,EtzF,EnspF,SymbF,GynoF],
	% working_directory( _, maps ),
 	% maplist( link_to_map_sub(unip), Files ),
    maplist( link_to_bio_sub(mouse,unip,maps), Files ),

	bio_db_dnt_times( File, SwDnDt, _SwDnEn ),
	SwOpts = [source(Url),datetime(SwDnDt)],
	bio_db_add_infos_to( [header(row('Ensembl_Protein','UniProt'))|SwOpts], 'maps/map_unip_mouse_ensp_unip.pl' ),
	bio_db_add_infos_to( [header(row('Uni_Protein','Entrez_ID'))|SwOpts], 'maps/map_unip_mouse_unip_entz.pl' ),
	bio_db_add_infos_to( [header(row('Uni_Protein','MGI'))|SwOpts], 'maps/map_unip_mouse_mgim_unip.pl' ),
	% bio_db_add_infos_to( [header(row('Uni Protein','HGNC ID'))|SwOpts], 'maps/map_unip_mouse_unip_hgnc.pl' ),
    % Unigene has been discontinued
	%bio_db_add_infos_to( [header(row('Uni_Protein','Uni_Gene'))|SwOpts], 'maps/map_unip_mouse_unip_unig.pl' ),
	bio_db_add_infos_to( [header(row('Uni_Protein','Symbol'))|SwOpts], 'maps/map_unip_mouse_unip_symb.pl' ),
	bio_db_add_infos_to( [header(row('Symbol','Uni_Protein'))|SwOpts], 'maps/map_unip_mouse_gyno_unip.pl' ),

	working_directory( _, DnDir ),

	% trem_hs( TremUrl ),
    trem_mouse( TremUrl ),
	% 15.05.14 adding support for treMBL, at least that 's what i think the selected file is all about
	TrUrlOpts = [debug(url_local),interface(wget),file(TrFile)],
	url_file_local_date_mirror( TremUrl, DnDir, TrUrlOpts ),
	bio_db_dnt_times( TrFile, TrDnDt, _TrDnEn ),

	os_make_path( maps, afresh(false) ),
	os_make_path( trembl, afresh(true) ),
	directory_file_path( _, TremFile, TremUrl ),
	pwd,
	directory_file_path( trembl, TremFile, TremTrg ),
	copy_file( TremFile, TremTrg ),
	working_directory( _, trembl ), 
	file_name_extension( TremDatF, gz, TremFile ),
	atom_concat( 'gunzip ', TremFile, Gunzip ),
	debug( Self, 'Gunzipping: ~p', TremFile ),
	shell( Gunzip ),
	csv_read_file( TremDatF, TremRows, [separator(0'\t)] ),
	length( TremRows, TremLen ), 
	write( trem_length(TremLen) ), nl,
	% 17/22
	findall( map_unip_mouse_trem_nucs(TremId,Nucs), (
	                  member(TremRow,TremRows), arg(1,TremRow,TremId), \+ empty(TremId), 
	                  arg(17,TremRow,NucsConcat), \+ empty(NucsConcat), 
				   atomic_list_concat(NucsList,'; ',NucsConcat),
				   member(Nucs,NucsList)
				            ), 
						        TNRows ),
	length(TNRows, TNLen), 
	write( tn_len(TNLen) ), nl,
	sort( TNRows, TNOrdRows ),
	open( '../maps/map_unip_mouse_trem_nucs.pl', write, TNOut ),
	maplist( portray_clause(TNOut), TNOrdRows ),
	close( TNOut ),
	working_directory( _, '../maps' ),
 	link_to_bio_sub( mouse, unip, maps, 'map_unip_mouse_trem_nucs.pl' ),

	TrOpts = [source(TremUrl),datetime(TrDnDt),header(row('treMBLE_Protein','Nucleotide_Sequence'))],
	bio_db_add_infos_to( TrOpts, map_unip_mouse_trem_nucs.pl ),
	% run this manually, it is a biggie: 
	% uniprot_sprot.dat is 2.9 G
	% std_map_usyn_unip,
	%

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
    link_to_bio_sub( mouse, unip, maps, AbsMapF ).

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

de_semi( Pfx, AccPrv, Acc ) :-
	atomic_list_concat( [Pfx,AccAtm], ':', AccPrv ), 
	atom_number( AccAtm, Acc ),
	!.
de_semi( Pfx, AccPrv, _Acc ) :-
	write( de_semi_disaster(Pfx,AccPrv) ), nl,
	abort.
