
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
:- lib(link_to_bio_sub/2).
:- lib(bio_db_add_infos/1).             % bio_db_add_infos_to/2.
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).

std_maps_pros_defaults( Defs ) :-
                              Defs = [ db(pros),
                                       debug(true),
                                       debug_fetch(true),
                                       debug_url(false),
                                       iactive(true),
                                       org(human),
                                       pros_file('prosite_alignments.tar.gz')
                                     ].

/** std_maps_pros(+Opts).

Support for prosite annotations of uniprot proteins.

Opts
  * db(Db=pros)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * pros_file(ProsF='prosite_alignments.tar.gz')
    the file name for prosite downlad

@author nicos angelopoulos
@version  0.1 2016/2/4
@version  0.2 2023/9/22,  moved URL components to the options

*/
std_maps_pros :-
	std_maps_pros( [] ).
	
std_maps_pros( Args ) :-
	Self = std_maps_pros,
	options_append( Self, Args, Opts ),
	bio_db_build_aliases( Opts ),
     % load necessary data that has already been generated
     unip:ensure_loaded( bio_db_build_downloads('unip/maps/unip_homs_sprt_seqn') ),
     unip:ensure_loaded( bio_db_build_downloads('unip/maps/unip_homs_unip_hgnc') ),
     hgnc:ensure_loaded( bio_db_build_downloads('hgnc/maps/hgnc_homs_hgnc_symb') ),
	debuc( std_maps_pros, 'Starting Prosite maps', true ),
     build_dnload_loc( Self, DnDir, Opts ),
     bio_db_source_url( Url, [debug_url-debug,pros_file-url_file], Opts ),
     options( debug_fetch(Fbg), Opts ),
	UrlOpts = [debug(Fbg),interface(wget),file(ProsF),ext('tar.gz')|Opts],
	url_file_local_date_mirror( Url, DnDir, UrlOpts ),
     working_directory( Old, DnDir ),
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
     debuc( Self, length, msa_fs/MsaFs ),
	map_succ_list( msa_file_pros_prsn, MsaFs, ProsPrsnClauses ),
     debuc( Self, length, pros_clauses/ProsPrsnClauses ),
	PrsnF = 'pros_homs_pros_prsn.pl',
	portray_clauses( ProsPrsnClauses, file(PrsnF) ),
	debuc( Self, wrote, PrsnF ),
	PrsnOpts = [header(row('Prosite id','Prosite name')),datime(DnSt),source(Url)],
	bio_db_add_infos_to( PrsnOpts, PrsnF ),
	os_make_path( maps ),
	os_path( maps, PrsnF, RelPrsnF ),
	rename_file( PrsnF, RelPrsnF ),

	maplist( msa_file_pros_sprt, MsaFs, ProsSprtClausesNest ),
	flatten( ProsSprtClausesNest, ProsSprtClauses ),
	SprtF = 'pros_homs_pros_sprt.pl',
	portray_clauses( ProsSprtClauses, file(SprtF) ),
	debuc( Self, wrote, SprtF ),

     % fixme: the header below is wrong
	SprtOpts = [header(row('Prosite id','Prosite protein')),datime(DnSt),source(Url)],
	bio_db_add_infos_to( SprtOpts, SprtF ),
	os_path( maps, SprtF, RelSprtF ),
	rename_file( SprtF, RelSprtF ),

    maplist( link_to_bio_sub(pros), [RelPrsnF,RelSprtF] ),
	working_directory( _, Old ),
	debuc( std_maps_pros, 'Done Prosite maps', true ).
	
msa_file_pros_prsn( File, pros_homs_pros_prsn(Base,Prsn) ) :-
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
				  unip:unip_homs_sprt_seqn( Prot, _ ),
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
				  unip:unip_homs_sprt_seqn( Prot, _ ),
				  ( (unip:unip_homs_unip_hgnc(Prot,Hgnc),
				     hgnc:hgnc_homs_hgnc_symb(Hgnc,Symb))
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
			       Map = pros_homs_pros_sprt(Base,PrsnA,Prot,Symb,From,To,AtmL2)
			    ),
			    		Maps ).

