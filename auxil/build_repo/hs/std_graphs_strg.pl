
:- set_prolog_flag(stack_limit, 10 000 000 000).

:- use_module( library(apply) ).        % maplist/2.
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
% :- lib(bio_db).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:url_file/3).

% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(ncbi:bio_db_build_downloads('ncbi/maps/map_ncbi_ensp_entz')).
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_entz_symb')).

% local libs & sources
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).  % bio_db_add_infos_to/2, fixme:
:- lib(std_graphs_strg_auto_version/1).

:- debuc(by_unix).
:- debuc(std_graphs_strg).  % fixme: you probably don't need this. 
% now there is an option...

std_graphs_strg_defaults( [debug(true)|T] ) :-
    ( std_graphs_strg_auto_version(Vers) -> % let options/2 do the erroring
                                            % because user might provide it
        T = [string_version(Vers)]
        ;
        T = []   
    ).

/** std_graphs_strg(+Opts).

String does not provide an easy way to get the current version.<br>
This script now does go into auto detecting the version. 
If you notice that it is picking an old one, you can re-run with

==
  ?- std_graphs_strg( string_version('9.1') ).
==

@author nicos angelopoulos
@version 0.2  2018/11/05
@tbd add information file or info terms/comments in edges file

*/

% last good one: std_graphs_string( '10' ).  2016/09/08
% last good one: std_graphs_string( '10.5' ).  2018/03/30
std_graphs_strg( Args ) :-
    Self = std_graphs_strg,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    % load necessary data that has already been generated
    ensure_loaded(ncbi:bio_db_build_downloads('ncbi/maps/map_ncbi_ensp_entz')),
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_entz_symb')),

    % std_graphs_strg( VersionPrv ) :-
    options( string_version(VersionPrv), Opts ),
    ( number(VersionPrv) -> atom_number(Version,VersionPrv); Version = VersionPrv ),
	debuc( Self, 'Version: ~w', Version ),
	std_graphs_string_version_base_name( Version, Bname, From ),
	Self = std_graphs_strg,
	debuc( Self, 'Base name: ~w', Bname ),
	absolute_file_name( bio_db_build_downloads(strg), Parent ),
	% absolute_file_name( baio_db_downloads(string/Bname), LocalFile ),
	% directory_file_path( Parent, _BnameAgain, LocalFile ),
	directory_file_path( Parent, Bname, LocalFile ),
	os_make_path( Parent, debug(true) ),
	std_graph_string_download_string( LocalFile, From, Self ),
	working_directory( Here, Parent ),
	@ gunzip( -k, Bname ),  % keeps .gz file
	% @ gunzip( '9606.protein.links.v10.txt.gz' ),
	Edge = edge_strg_hs,
	Opt = [ csv_read(separator(0' )),predicate_name(Edge),
	        rows_transform(maplist(user:de_hs)),header_remove(true),
            mtx_opt(convert(false))
		 ],
	file_name_extension( TxtF, gz, Bname ),
	Mess1 = 'Converting string file: ~p, to Prolog',
	% file_name_extension( Stem, txt, TxtF ),
	% file_name_extension( Stem, pl, PlF ),
	debuc( Self, 'Directory: ~p', [Parent] ),
	debuc( Self, Mess1, [TxtF] ),
	mtx_prolog( TxtF, File, Opt ),
	debuc( _, 'Edges output: ~w', File ),
	delete_file( TxtF ),
	% @ rm( -rf, graphs ), % don't do this. there are now other organisms 
    % puting stuff in graphs/ (eg mouse...)
	os_make_path( graphs, debug(true) ),
	Trg = 'graphs/edge_strg_hs.pl',
	@ rm( -f, Trg ),
	@ mv( File, Trg ),

	consult( edge_strg_hs:Trg ),
	debuc( _, 'Consulted hs: ~w', [edge_strg_hs:Trg] ),

	findall( edge_strg_hs_symb(SymbA,SymbB,W),
	                     ( edge_strg_hs:edge_strg_hs(EnsP1,EnsP2,W),
					       ensp_symb(EnsP1,Symb1),
					       ensp_symb(EnsP2,Symb2),
					       sort(Symb1,Symb2,SymbA,SymbB)
					 ),
		  	UnoSymbEdges
		  ),
	sort( UnoSymbEdges, SymbEdges ),
    length( SymbEdges, SymbEdgesLen ),
	debuc( _, 'Unique symbol edges hs: ~w', [SymbEdgesLen] ),
	EdgeSymbsF = 'graphs/edge_strg_hs_symb.pl',
	portray_clauses( SymbEdges, file(EdgeSymbsF) ),
	bio_db_dnt_times( Bname, DnDt, _EndDt ),
	HsOpts = [source(From),datetime(DnDt),header(row('Ensembl Protein','Ensembl Protein',weight))],
	bio_db_add_infos_to( HsOpts, Trg ),

	SymbOpts = [source(From),datetime(DnDt),header(row('HGNC Symbol','HGNC Symbol',weight))],
	bio_db_add_infos_to( SymbOpts, EdgeSymbsF ),

	link_to_bio_sub( strg, Trg, [org(hs),type(graphs)] ),
	link_to_bio_sub( strg, EdgeSymbsF, [org(hs),type(graphs)]  ),
	working_directory( _, Here ).

ensp_symb( EnsP, Symb ) :-
	ncbi:map_ncbi_ensp_entz( EnsP, Entz ),
	hgnc:map_hgnc_entz_symb( Entz, Symb ),
	!.

sort( X, Y, A, B ) :-
	Y @< X,
	!,
	A = Y, B = X.
sort( A, B, A, B ).

std_graph_string_download_string( LocalFile, _From, Self ) :-
	exists_file( LocalFile ),
	debuc( Self, 'Using existing local string file: ~p', LocalFile ),
	!.
std_graph_string_download_string( Local, Remote, Self ) :-
	debuc( Self, 'Downloading from: ~p', Remote ),
	url_file( Remote, Local, dnt(true) ),
	debuc( Self, '... to local file: ~p', Local ).

std_graphs_string_version_base_name( VersionPrv, Bname, Remote ) :-
	( atom_concat(v,Version,VersionPrv)->true;Version=VersionPrv ),
	atom_concat( v, Version, Vied ),
	% Pfx = 'http://string-db.org/newstring_download/protein.links.v',
	Pfx = 'https://string-db.org/download/protein.links.v',
	atom_concat( Pfx, Version, RemoteDir ),
	atomic_list_concat( [9606,protein,links,Vied,txt,gz], '.', Bname ),
	directory_file_path( RemoteDir, Bname, Remote ).
	% 10/9606.protein.links.v10.txt.gz

bio_db_std_string :-
	Opt = [ csv_read(separator(0' )), predicate_name(hs_strg_edge),
	        rows_transform(maplist(user:de_hs))
		 ],
	mtx_prolog( bio_dn(strg/'protein.links.hs.txt'), File, Opt ),
	debuc( _, 'Edges output: ~w', File ),
	bio_db_std_string_link( File ).

de_hs( row(HsEnsP1,HsEnsP2,WAtm), row(EnsP1,EnsP2,W) ) :-
	atom_concat( '9606.', EnsP1, HsEnsP1 ),
	atom_concat( '9606.', EnsP2, HsEnsP2 ),
    ( number(WAtm) -> W = WAtm; atom_number(WAtm,W) ),
	!.
de_hs( Row, _ ) :-
	debuc( _, 'Failed to translate row: ~w', Row ),
	abort.

bio_db_std_string_link( File ) :-
	HsBio = bio_db_build_data( 'graphs/strg/hs_strg_edge.pl' ),
	absolute_file_name( HsBio, HsTarget ),
	bio_db_std_string_link_target( HsTarget, File ).

bio_db_std_string_link_target( HsTarget, File ) :-
	read_link( HsTarget, _OldFile, _What ),
	delete_file( HsTarget ),
	!,
	atomic_list_concat( ['ln -s',File,HsTarget], ' ', Shell ),
	shell( Shell ).
bio_db_std_string_link_target( HsTarget, File ) :-
	atomic_list_concat( ['ln -s',File,HsTarget], ' ', Shell ),
	shell( Shell ).
