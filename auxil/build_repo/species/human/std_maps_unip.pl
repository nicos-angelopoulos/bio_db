
:- set_prolog_flag(stack_limit, 80 000 000 000).

:- use_module(library(csv)).        % csv_read_file/3.
:- use_module(library(filesex)).    % directory_file_path/3, copy_file/2.
:- use_module(library(apply)).      % maplist/2.
:- use_module(library(lists)).      % member/2.
:- use_module(library(listing)).    % portray_clause/2.
:- use_module(library(readutil)).   % read_line_to_codes/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(by_unix).   % @/1.
:- lib(os_lib). 
:- lib(options). 
:- lib(debug_call).% /3. 

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  %/1.

% local libs & sources
:- lib(map_uniprot/4).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).  % bio_db_add_infos_to/2
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).

std_maps_unip_defaults( Defs ) :-
                                   Defs = [ 
                                            db(unip),
                                            debug(true),
                                            debug_url(false),
                                            debug_fetch(true),
                                            iactive(true),
                                            org(human),
                                            unip_file_full('HUMAN_9606_idmapping.dat.gz'),
                                            unip_file_sele('HUMAN_9606_idmapping_selected.tab.gz')
                                          ].

%% maps_std_unip(+Opts).
%
% Create uniprot maps.
%
% Opts
%  * db(Db=unip)
%    source database
%  * debug(Dbg=true)
%    informational, progress messages
%  * debug_fetch(Ubg=true)
%    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
%  * debug_url(Ubg=false)
%    whether to debug the concatenation of the url (via bio_db_source_url/3)
%  * iactive(Iact=true)
%    whether the session is interactive, otherwise wget gets --no-verbose
%  * unip_file_full(UnipFF='HUMAN_9606_idmapping.dat.gz')
%    the file name for the obo download
%  * unip_file_sele(UnipFF='HUMAN_9606_idmapping.dat.gz')
%    the file name for the selected ids mapping download (stricter than the above)
%
%==
% ?- maps_std_uniprot.
% ?- shell( 'wc -l uniprot_*' ).
%==
%
% @author nicos angelopoulos
% @version  0.2 2015/4/27
% @tbd use hgnc as template to download from 
%
std_maps_unip( Args ) :-
     Self = std_maps_unip,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnDir, Opts ),
	working_directory( Old, DnDir ),
     bio_db_source_url( Url, [debug_url-debug,unip_file_full-url_file], Opts ),
     options( debug_fetch(Fbg), Opts ),
	url_file_local_date_mirror( Url, DnDir, [dnld_file(Bname),debug(Fbg)|Opts] ),
	os_make_path( maps, debug(true) ),
     %
	debuc( Self, 'Dir location: ~p', DnDir ),
	Rev = [interface(prolog),reverse(true)],
	map_uniprot( 'Ensembl_PRO', Csv, [EnspF], Rev ),
     %
	Fgnc = [interface(prolog),f_call(de_semi('HGNC'))],
	map_uniprot( 'HGNC', Csv, [FromHgncF], Fgnc ),
     %
	Sem = [interface(prolog),f_call(de_semi('HGNC')),reverse(true)],
	map_uniprot( 'HGNC', Csv, [HgncF], Sem ),
     %
	Ifc = interface(prolog),
	map_uniprot( 'GeneID', Csv, [EtzF], Ifc ),
	% map_uniprot( 'UniGene', Csv, :- use_module(library(filesex)).[UniGF], Ifc ),
	Files = [HgncF,FromHgncF,EtzF,EnspF],
 	maplist( link_to_bio_sub(unip), Files ),
     % 
	bio_db_dnt_times( Bname, SwDnDt, _SwDnEn ),
	SwOpts = [source(Url),datetime(SwDnDt)],
	bio_db_add_infos_to( [header(row('Ensembl Protein','Uni Protein'))|SwOpts], 'maps/unip_homs_ensp_unip.pl' ),
	bio_db_add_infos_to( [header(row('Uni Protein','Entrez ID'))|SwOpts], 'maps/unip_homs_unip_entz.pl' ),
	bio_db_add_infos_to( [header(row('Uni Protein','HGNC ID'))|SwOpts], 'maps/unip_homs_unip_hgnc.pl' ),
     bio_db_source_url( TremUrl, [debug_url-debug,unip_file_sele-url_file], Opts ),
	TrUrlOpts = [debug(true),interface(wget),dnld_file(TremFile)|Opts],
	url_file_local_date_mirror( TremUrl, DnDir, [debug(Fbg)|TrUrlOpts] ),
	bio_db_dnt_times( TremFile, TrDnDt, _TrDnEn ),
     debuc( Self, 'File: ~w, dnt_start: ~w', [TremFile,TrDnDt] ),
	os_make_path( maps, afresh(false) ),
	os_make_path( trembl, afresh(true) ),
     debuc( Self, 'File: ~w, Url: ~w', [TremFile,TremUrl] ),
     debuc( Self, pwd, false ),
	directory_file_path( trembl, TremFile, TremTrg ),
	copy_file( TremFile, TremTrg ),
	working_directory( _, trembl ), 
	file_name_extension( TremDatF, gz, TremFile ),
	atom_concat( 'gunzip ', TremFile, Gunzip ),
	debuc( Self, 'Gunzipping: ~p', TremFile ),
	shell( Gunzip ),
	csv_read_file( TremDatF, TremRows, [separator(0'\t)] ),
     debuc( Self, length, trem/TremRows ),
	% 
	findall( unip_homs_trem_nucs(TremId,Nucs), (
	                  member(TremRow,TremRows), arg(1,TremRow,TremId), \+ empty(TremId), 
	                  arg(17,TremRow,NucsConcat), \+ empty(NucsConcat), 
				   atomic_list_concat(NucsList,'; ',NucsConcat),
				   member(Nucs,NucsList)
				            ), 
						        TNRows ),
    debuc( Self, length, tn/TNRows ),
	sort( TNRows, TNOrdRows ),
	open( '../maps/unip_homs_trem_nucs.pl', write, TNOut ),
	maplist( portray_clause(TNOut), TNOrdRows ),
	close( TNOut ),
	working_directory( _, '../maps' ),
     @ rm( -rf, '../trembl' ),   % fixme: untested in real run.....
 	link_to_bio_sub(unip, 'unip_homs_trem_nucs.pl' ),

	TrOpts = [source(TremUrl),datetime(TrDnDt),header(row('treMBLE Protein','Nucleotide Sequence'))],
	bio_db_add_infos_to( TrOpts, 'unip_homs_trem_nucs.pl' ),
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
	debuc( uniprot, length, syn_rows/SynRs ),
	close( In ),
	sort( SynRs, OrdRs ),
	unip_dnload( DnDir ),
	os_path( DnDir, maps, MapsD ),
	Opts = [prefix(unip),dir(MapsD)],
	csv_ids_map( _, usyn, unip, [row(usyn,unip)|OrdRs], MapF, Opts ),
	os_path( MapsD, MapF, AbsMapF ),
 	link_to_bio_sub( unip, AbsMapF ).
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

/* delete me
os_rm_rf( Dir ) :-
	exists_directory(Dir) ->
	delete_directory_and_contents(Dir),
	!.
os_rm_rf( _Dir ).
*/

de_semi( Pfx, AccPrv, Acc ) :-
	atomic_list_concat( [Pfx,AccAtm], ':', AccPrv ), 
	atom_number( AccAtm, Acc ),
	!.
de_semi( Pfx, AccPrv, _Acc ) :-
	write( de_semi_disaster(Pfx,AccPrv) ), nl,
	abort.
