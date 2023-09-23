
:- set_prolog_flag(stack_limit, 2 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lists)).  % member/2,...
:- use_module(library(apply)).  % maplist/2.

:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).

:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:kv_decompose/3).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_ensg_hgnc')).
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_hgnc_symb')).
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')).

% local libs & sources
:- lib(csv_to_pl/2).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_source_url/2).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2

:- debuc(std_maps_ense).

std_maps_ense_defaults( Defs ) :-
                                   Defs = [ db(ense),
                                            debug(true),
                                            debug_url(false),
                                            ense_homs_base(ense_homs),
                                            ense_homs_file(call(ense_homs_url_file)),
                                            iactive(true)
                                          ].

/** std_maps_ense( +Opts ).

Maps based on ensembl .gtf file. Including transcripts to genes, genes
to hgncs and locations of transcipt and genes to chromosomome locations.

Opts
  * assembly(Assembly)
    assembly number (just the number)
  * db(ense)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/2)
  * ense_homs_base(Eoms=ense_homs)
    Url or bio_db_source_base_url/2 token for download diretory
  * ense_homs_file(Eile=call(ense_homs_url_file))
    the file name  for the download (appended to Ufx@bio_db_source_base_url(gont_obo,Ufx))- or call that produces it
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * release(Release)
    release number

Nonmeclature:
  * ense: the database abbv.
  * enst: ensembl transcript
  * ensg: ensembl gene
  * chrl: chromosomal location

Produces:
  * ense_homs_enst_ensg
  * ense_homs_ensg_hgnc
  * ense_homs_ensg_symb
  * ense_homs_ensg_chrl
  * ense_homs_enst_chrl

@author  nicos angelopoulos
@version 0.1, 2016/6/17
@version 0.2, 2016/6/17, added assembly and release options
@version 0.3, 2023/9/23, factor out the url locations and Url construction
@see ftp://ftp.ensembl.org/pub/release-84/gtf/homo_sapiens/Homo_sapiens.GRCh38.84.gtf.gz
@see http://ftp.ensembl.org/pub/current_gtf/homo_sapiens/

*/
std_maps_ense( Args ) :-
     Self = std_maps_ense,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     % load necessary data that has already been generated
     ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_ensg_hgnc')),
     ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_hgnc_symb')),
     ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_symb_hgnc')),
	debuc( Self, 'Starting...', true ),
	absolute_file_name( bio_db_build_downloads(ense), DnDir ),
	os_make_path( DnDir ),
	debuc( Self, 'Downloads dir for ense: ~p', DnDir ),
     options( [ense_homs_base(EomsB),ense_homs_file(EomsF),debug_url(Ubg)], Opts ),
     Epts = [url_base(EomsB),url_file(EomsF),debug(Ubg)],
     bio_db_source_url( Url, Epts ),
     ( options(iactive(false),Opts) -> WgVerb=false; WgVerb=true ),
	url_file_local_date_mirror( Url, DnDir, [file(File),interface(wget),verb(WgVerb)] ),
	debuc( Self, 'Dnload done, file is: ~p', File ),
	working_directory( Old, DnDir ),
	bio_db_dnt_times( File, DnDt, _DnEn ),
	debuc( by_unix ),
	os_un_zip( File, _, [keep(true),on_exists(skip),debug(true)] ),
	% @ gunzip( -k, -f, File ),
	os_ext( gz, Stem, File ),
	os_ext( tab, Stem, TabF ),
	% fixme: ???:
	% Stem = 'Homo_sapiens.GRCh38.84.gtf-16.06.20', 
	% TabF = 'Homo_sapiens.GRCh38.84.gtf-16.06.20.tab', 
     % fixme: swi has skipping of initial comment lines...
	% atomic_list_concat( [grep,'-v','"^#"',Stem,'>',TabF], ' ', Shell ),
     % debuc( Self, '~w, shelling/1 : ~w', [Self,Shell] ),
	% shell( Shell ),
	% debuc( Self, '...done...', true ),
	% @ grep( -v, '"^#"', Stem, '>', TabF ),
	% @ ls(),
     % debuc( Self, 'Reading from: ~p', [TabF] ),
	% mtx( TabF, Rows, sep(tab) ),
     %
     debuc( Self, 'Reading from: ~p', [Stem] ),
     mtx( Stem, Rows, [sep(tab),csv_read(skip_header('#'))] ),
	debuc( Self, length, rows/Rows ),
	ense_transcripts( Rows, EnsTGRows, EnsTLRows ),
     debuc( Self, length, [tr_to_names_map,tr_locations]/[EnsTGRows,EnsTLRows] ),
	mtx( 'ense_homs_enst_ensg.csv', EnsTGRows ),
	mtx( 'ense_homs_enst_chrl.csv', EnsTLRows ),

	ense_genes( Rows, EnsGHRows, EnsGSRows, EnsGCRows, Disagreed, NotInHgnc ),
     debuc( Self, length, ense_to_hgnc_symbol_disagreements/Disagreed ),
     debuc( Self, length, ense_has_no_hgnc/NotInHgnc ),
	mtx( 'ense_homs_ensg_hgnc.csv', EnsGHRows ),
	mtx( 'ense_homs_ensg_symb.csv', EnsGSRows ),
	mtx( 'ense_homs_ensg_chrl.csv', EnsGCRows ),

	Csvs = [ 'ense_homs_enst_ensg.csv', 'ense_homs_enst_chrl.csv',
		    'ense_homs_ensg_hgnc.csv', 'ense_homs_ensg_symb.csv',
              'ense_homs_ensg_chrl.csv'
	       ],
	debuc( Self, 'mapping: ~w', [Csvs] ),
	maplist( csv_to_pl(Self), Csvs ),

	/*

	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,
	findall( row(EnsG,ChrG,SrtG,EndG,DirG),  (
										member(RowG,Rows),


	*/
	maplist( new_ext(pl), Csvs, Pls ),
	% fixme: do the headers :( 
	AddOpts = [source(Url),datetime(DnDt)],
	% maplist( bio_db_add_infos_to( [header(row('GO Term','HGNC Symbol'))|AddOpts], 'maps/map_gont_gont_symb.pl' ),
	% maplist( bio_db_add_infos_to(AddOpts), Pls ),
	Headers = [	
		row('Ensembl Transcript','Ensembl Gene'),
		row('Ensembl Transcript','Chromosome', 'Start', 'End', 'Direction'),
		row('Ensembl Gene','HGNC ID'),
		row('Ensembl Gene','HGNC Symbol'),
		row('Ensembl Gene','Chromosome', 'Start', 'End', 'Direction')
			],
	findall( PlF, (	nth1(N,Pls,PlF),	nth1(N,Headers,Header),
					debuc( Self, 'ingoing prolog file: ~p', PlF ),
					debuc( Self, '...with header: ~w and options: ~w', [Header,AddOpts] ),
					bio_db_add_infos_to([header(Header)|AddOpts], PlF)
					),
					_PlFs ),
	os_make_path( maps ),
	maplist( mv_to_sub(maps), Pls ),
    @ rm( -f, Stem, TabF ),
	working_directory( _, maps ),
	maplist( link_to_bio_sub(ense), Pls ),

	working_directory( _, Old ),
	debuc( Self, '...Done', true ).

mv_to_sub( Sub, File ) :-
	os_path( Sub, File, Rel ),
	rename_file( File, Rel ).

new_ext( New, File, NewFile ) :-
	os_ext( _Old, New, File, NewFile ).
	
ense_genes( [], [], [], [], [], [] ).
ense_genes( [RowG|Rows], GHRows, GSRows, [EnsGC|GCRows], Dis, Nin ) :-
	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,InfoG),
	EnsGC= row(EnsG,ChrG,SrtG,EndG,DirG),
	ense_info( gene_id, InfoG, EnsG ),
	ense_info( gene_name, InfoG, def(EnsG), EnsN ),
	ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, Dis, Nin, TGHRows, TGSRows, Tis, Tin ),
	!,
	ense_genes( Rows, TGHRows, TGSRows, GCRows, Tis, Tin ).
ense_genes( [RowG|Rows], _, _, _, _, _ ) :-
	RowG = row(_ChrG,_Db,gene,_SrtG,_EndG,_,_DirG,_,_InfoG),
	!,
	length( Rows, Len ),
	throw( tripped_on_gene_row(RowG,Len) ).
% here: nonGRow, is not a gen row, so it is skipped
ense_genes( [_NonGRow|Rows], GHRows, GSRows, GCRows, Dis, Nin ) :-
	ense_genes( Rows, GHRows, GSRows, GCRows, Dis, Nin ).

ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, Dis, Nin, TGHRows, TGSRows, Tis, Tin ) :-
	% map_hgnc_ensg_hgnc( EnsG, Hgnc ),
	findall( AHgnc, hgnc:hgnc_homs_ensg_hgnc(EnsG,AHgnc), [Hgnc] ),
        % there are 10 or so EnsGs, map to multi Hs
	!,
	hgnc:hgnc_homs_hgnc_symb( Hgnc, Symb ),
	ense_gene_hgnc_symbols( EnsN, Symb, Dis, Tis ),
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,Symb)|TGSRows],
    Nin = Tin.
ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, Dis, Nin, TGHRows, TGSRows, Tis, Tin ) :-
	% \+ map_hgnc_ensg_hgnc( EnsG, _ ),
	hgnc:hgnc_homs_symb_hgnc( EnsN, Hgnc ),
	!,
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,EnsN)|TGSRows],
    Dis = Tis,
    Nin = Tin.
ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, Dis, [EnsG-EnsN|Tin], GHRows, GSRows, Dis, Tin ).

ense_gene_hgnc_symbols( Symb, Symb, Dis, Tis ) :- !, Dis = Tis.
ense_gene_hgnc_symbols( EnsN, Symb, Dis, Tis ) :-
    Mess = 'Ensembl gene name (~w), different than HGNC symbol: (Symb)',
    debuc( std_maps_ense(details), Mess, [EnsN,Symb] ),
    Dis = [EnsN-Symb|Tis].

ense_transcripts( [], [], [] ).
ense_transcripts( [RowT|Rows], [EnsTG|TGRows], [EnsTL|TLRows] ) :-
	RowT = row(ChrT,_Db,transcript,SrtT,EndT,_,DirT,_,InfoT),
	% findall( row(EnsT,EnsG)-row(EnsT,ChrT,SrtT,EndT,DirT), ( )).
	ense_info( transcript_id, InfoT, EnsT ),
	ense_info( gene_id, InfoT, EnsG ),
	ense_chromosome( ChrT ),
	!,
	EnsTG = row(EnsT,EnsG),
	EnsTL = row(EnsT,ChrT,SrtT,EndT,DirT),
	ense_transcripts( Rows, TGRows, TLRows ).
ense_transcripts( [RowT|Rows], _, _ ) :-
	RowT = row(ChrT,_Db,transcript,_SrtT,_EndT,_,_DirT,_,_InfoT),
	ense_chromosome( ChrT ),
	!,
	length( Rows, Len ),
	throw( tripped_on_transcript_row(RowT,Len) ).
ense_transcripts( [_RowT|Rows], TGRows, TLRows ) :-
	ense_transcripts( Rows, TGRows, TLRows ).

ense_info( Key, Lookup, Value ) :-
	ense_info( Key, Lookup, true, Value ).
	
ense_info( Key, Lookup, _Strict, Value ) :-
	atomic_list_concat( Parts, '"; ', Lookup ),
	atom_concat( Key, ' "', Left ),
	member( Part, Parts ),
	atom_concat( Left, Value, Part ),
	!.
ense_info( Key, Lookup, Strict, Value ) :-
	ense_info_failure( Strict, Key, Lookup, Value ).

ense_info_failure( true, Key, Lookup, _ ) :-
	throw( lookup_failure(Key,Lookup) ).
ense_info_failure( def(Def), _Key, _Lookup, Def ).
% this is never called, currently:
ense_info_failure( false, _Key, _Lookup, false ).

ense_chromosome( 'X' ) :- !.
ense_chromosome( 'Y' ) :- !.
ense_chromosome( N ) :- integer( N ), !.
