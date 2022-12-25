
:- use_module(library(lists)).      % member/2, nth1/3.
:- use_module(library(apply)).      % include/3, maplist/2,3.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:map_succ_list/3).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:io_lines/2).

% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% load necessary data that has already been generated
% :- unip:ensure_loaded( bio_db_build_downloads('unip/maps/map_unip_sprt_seqn') ). % /2
% :- unip:ensure_loaded( bio_db_build_downloads('unip/maps/map_unip_unip_hgnc') ).
% :- hgnc:ensure_loaded( bio_db_build_downloads('hgnc/maps/map_hgnc_hgnc_symb') ).

% local libs & sources
:- lib(bio_db_add_infos/1).  % bio_db_add_infos_to/2.
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(link_to_bio_sub/2).

pros_alignments_url( 'ftp://ftp.expasy.org/databases/prosite/prosite_alignments.tar.gz' ).

pros_dnload_dir( Old, Loc, Opts ) :-
	absolute_file_name( bio_db_build_downloads(pros), Loc ),
	os_make_path( Loc, Opts ),
	debuc( maps_unip_seqs, 'Prosite build directory: ~p', Loc ),
	working_directory( Old, Loc ).

std_maps_pros_defaults( debug(true) ).

/** std_maps_pros.

Support for prosite annotations of uniprot proteins.

@author nicos angelopoulos
@version  0.1 2016/2/4

*/
std_maps_pros :-
	std_maps_pros( [] ).
	
std_maps_pros( Args ) :-
	Self = std_maps_pros,
	options_append( Self, Args, Opts ),
	bio_db_build_aliases( Opts ),
    % load necessary data that has already been generated
    unip:ensure_loaded( bio_db_build_downloads('unip/maps/map_unip_sprt_seqn') ),
    unip:ensure_loaded( bio_db_build_downloads('unip/maps/map_unip_unip_hgnc') ),
    hgnc:ensure_loaded( bio_db_build_downloads('hgnc/maps/map_hgnc_hgnc_symb') ),
	debuc( std_maps_pros, 'Starting Prosite maps', true ),
	pros_dnload_dir( Old, DnDir, Opts ),
	pros_alignments_url( Url ),
	UrlOpts = [debug(true),interface(wget),file(ProsF),ext('tar.gz')],
	url_file_local_date_mirror( Url, DnDir, UrlOpts ),
	bio_db_dnt_times( ProsF, DnSt, _DnEn ),
	debuc( Self, 'Prosite local file: ~p', ProsF ),
	( os_dir(prosite_alignments) ->
        @rm( -rf, '/tmp/prosite_alignments' ),
		@mv( prosite_alignments, /tmp ),
		debuc( Self, 'Moved existing dir prosite alignments to /tmp/', true )
		;
		true
	),
	@ gunzip( -f, -k, 'prosite_alignments.tar.gz' ),
	@ tar( xf, 'prosite_alignments.tar' ),
	@ rm( -f, 'prosite_alignments.tar' ),
	directory_files( prosite_alignments, PalignFs ),
	include( os_ext(msa), PalignFs, MsaFs ),
	map_succ_list( msa_file_pros_prsn, MsaFs, ProsPrsnClauses ),
	PrsnF = 'map_pros_pros_prsn.pl',
	portray_clauses( ProsPrsnClauses, file(PrsnF) ),
	debuc( std_map_pros, wrote, PrsnF ),
	PrsnOpts = [header(),datime(DnSt),source(Url)],
	bio_db_add_infos_to( PrsnOpts, PrsnF ),
	os_make_path( maps ),
	os_path( maps, PrsnF, RelPrsnF ),
	rename_file( PrsnF, RelPrsnF ),

	maplist( msa_file_pros_sprt, MsaFs, ProsSprtClausesNest ),
	flatten( ProsSprtClausesNest, ProsSprtClauses ),
	SprtF = 'map_pros_pros_sprt.pl',
	portray_clauses( ProsSprtClauses, file(SprtF) ),
	debuc( std_map_pros, wrote, SprtF ),
	SprtOpts = [header(),datime(DnSt),source(Url)],
	bio_db_add_infos_to( SprtOpts, SprtF ),
	os_path( maps, SprtF, RelSprtF ),
	rename_file( SprtF, RelSprtF ),

    maplist( link_to_bio_sub(pros), [RelPrsnF,RelSprtF] ),
	working_directory( _, Old ),
	debuc( std_maps_pros, 'Done Prosite maps', true ).
	
msa_file_pros_prsn( File, map_pros_pros_prsn(Base,Prsn) ) :-
	% debuc( std_maps_pros, 'prositing prosite names: ~p', File ),
	os_ext( msa, Base, File ),
	os_path( prosite_alignments, File, Path ),
	% file_to_list_of_lines( Path, Lines ),
    io_lines( Path, Lines ),
	findall( PrsnA, (member(Line,Lines),Line = [0'>|Rine],
	                 atom_codes(Ratm,Rine), 
				  at_con([A,B,_C],'|',Ratm),
				  at_con([_,'HUMAN'],'_',A),
				  at_con([Prot,RightB],'/',B),
				  unip:map_unip_sprt_seqn( Prot, _ ),
				  at_con([_,PrsnA],': ',RightB)
				  % we can also match Pros to Left C (sep /)
				  )
				  , PrsnAs ),
	sort( PrsnAs, PrsnOrd ),
	% ( File == 'PS51172.msa' -> trace; true ),
	report_non_singleton_prsns( PrsnOrd, Prsn ).

report_non_singleton_prsns( [], _Prsn ) :- !, fail.
report_non_singleton_prsns( [Prsn], Prsn ) :- !.
report_non_singleton_prsns( Other, _Prsn ) :-
	throw( non_unique_prsn(Other) ).

msa_file_pros_sprt( File, Maps ) :-
	% debuc( std_maps_pros, 'prositing prosite swiprots: ~p', File ),
	os_ext( msa, Base, File ),
	os_path( prosite_alignments, File, Path ),
	% file_to_list_of_lines( Path, Lines ),
    io_lines( Path, Lines ),
	findall( Map, (  nth1(N,Lines,Line1),
	                 Line1 = [0'>|Rine],
	                 atom_codes(Ratm,Rine), 
				  at_con([A,B,_C],'|',Ratm),
				  at_con([_,'HUMAN'],'_',A),
				  at_con([Prot,RightB],'/',B),
				  unip:map_unip_sprt_seqn( Prot, _ ),
				  ( (unip:map_unip_unip_hgnc(Prot,Hgnc),
				     hgnc:map_hgnc_hgnc_symb(Hgnc,Symb))
					-> 
					true
					;
					Symb = unk
				  ),
				  at_con([FromTo,PrsnA],': ',RightB),
				  at_con([From,To],'-',FromTo),
				  M is N + 1,
				  nth1(M,Lines,Line2),
				  atom_codes(AtmL2,Line2),
			       Map = map_pros_pros_sprt(Base,PrsnA,Prot,Symb,From,To,AtmL2)
			    ),
			    		Maps ).

