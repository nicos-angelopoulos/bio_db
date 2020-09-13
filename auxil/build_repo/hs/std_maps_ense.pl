
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
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2

:- debuc(std_maps_ense).

std_maps_ense_defaults(debug(true)).

/** std_maps_ense( +Opts ).

Maps based on ensembl .gtf file. Including transcripts to genes, genes
to hgncs and locations of transcipt and genes to chromosomome locations.

Opts
  * assembly(Assembly)
    assembly number (just the number)
  * release(Release)
    release number

Nonmeclature:
  * ense: the database abbv.
  * enst: ensembl transcript
  * ensg: ensembl gene
  * chrl: chromosomal location

Produces:
  * map_ense_enst_ensg
  * map_ense_ensg_hgnc
  * map_ense_ensg_symb
  * map_ense_ensg_chrl
  * map_ense_enst_chrl

@author  nicos angelopoulos
@version 0.1, 2016/6/17
@version 0.2, 2016/6/17, added assembly and release options
@see ftp://ftp.ensembl.org/pub/release-84/gtf/homo_sapiens/Homo_sapiens.GRCh38.84.gtf.gz
@see http://ftp.ensembl.org/pub/current_gtf/homo_sapiens/
@tbd automate selection of latest version

*/
std_maps_ense( Args ) :-
    Self = std_maps_ense,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    % load necessary data that has already been generated
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_ensg_hgnc')),
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_hgnc_symb')),
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')),

	debuc( Self, 'Starting...', true ),
	absolute_file_name( bio_db_build_downloads(ense), DnDir ),
	os_make_path( DnDir ),
	debuc( Self, 'Downloads dir for ense: ~p', DnDir ),
	% Url = 'ftp://ftp.ensembl.org/pub/release-85/gtf/homo_sapiens/Homo_sapiens.GRCh38.85.gtf.gz',
	% Url = 'ftp://ftp.ensembl.org/pub/release-89/gtf/homo_sapiens/Homo_sapiens.GRCh38.89.gtf.gz',
    % options( [assembly(Amb),release(Rel)], Opts ),
	% Pfx = 'ftp://ftp.ensembl.org/pub/release-',
    % Mfx = '/gtf/homo_sapiens/Homo_sapiens.GRCh',
    % at_con( [Pfx,Rel,Mfx,Amb,'.',Rel,'.gtf.gz'], Url ),
	% Url = 'ftp://ftp.ensembl.org/pub/release-101/gtf/homo_sapiens/Homo_sapiens.GRCh38.101.gtf.gz',
	% Url = 'ftp://ftp.ensembl.org/pub/release-99/gtf/homo_sapiens/Homo_sapiens.GRCh38.99.gtf.gz',
    /* 20.09.10 - starting auto recognition of latest version
    */
    FtpDir = 'ftp://ftp.ensembl.org/pub/current_gtf/homo_sapiens/',
    Found @@ curl( -l, '--no-progress-meter', FtpDir ),
    % Homo_sapiens.GRCh38.101.gtf.gz
    findall( HsGtf-Amb-Rel, (member(HsGtf,Found),at_con(['Homo_sapiens',GRChTkn,RelAtm,gtf,gz],'.',HsGtf),
                         atom_concat('GRCh',AmbAtm,GRChTkn),
                         atom_number(AmbAtm,Amb), atom_number(RelAtm,Rel)
                        ),
                            HsGtfs ),
    ( HsGtfs = [HsGtfF-_Amb-_Rel] ->
        true
        ;
        throw( non_unique_auto_ided_ense_gtf_file(HsGtfF) )
    ),
    atom_concat( FtpDir, HsGtfF, Url ),
	url_file_local_date_mirror( Url, DnDir, [file(File),interface(wget)] ),
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
	mtx( 'map_ense_enst_ensg.csv', EnsTGRows ),
	mtx( 'map_ense_enst_chrl.csv', EnsTLRows ),

	ense_genes( Rows, EnsGHRows, EnsGSRows, EnsGCRows, Disagreed, NotInHgnc ),
    debuc( Self, length, ense_to_hgnc_symbol_disagreements/Disagreed ),
    debuc( Self, length, ense_has_no_hgnc/NotInHgnc ),
	mtx( 'map_ense_ensg_hgnc.csv', EnsGHRows ),
	mtx( 'map_ense_ensg_symb.csv', EnsGSRows ),
	mtx( 'map_ense_ensg_chrl.csv', EnsGCRows ),

	Csvs = [ 'map_ense_enst_ensg.csv', 'map_ense_enst_chrl.csv',
		    'map_ense_ensg_hgnc.csv', 'map_ense_ensg_symb.csv',
              'map_ense_ensg_chrl.csv'
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
	ense_info( gene_name, InfoG, EnsN ),
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
	findall( AHgnc, hgnc:map_hgnc_ensg_hgnc(EnsG,AHgnc), [Hgnc] ),
        % there are 10 or so EnsGs, map to multi Hs
	!,
	hgnc:map_hgnc_hgnc_symb( Hgnc, Symb ),
	ense_gene_hgnc_symbols( EnsN, Symb, Dis, Tis ),
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,Symb)|TGSRows],
    Nin = Tin.
ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, Dis, Nin, TGHRows, TGSRows, Tis, Tin ) :-
	% \+ map_hgnc_ensg_hgnc( EnsG, _ ),
	hgnc:map_hgnc_symb_hgnc( EnsN, Hgnc ),
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
ense_info( Key, Lookup, Strict, false ) :-
	ense_info_failure( Strict, Key, Lookup ).

ense_info_failure( true, Key, Lookup ) :-
	throw( lookup_failure(Key,Lookup) ).
ense_info_failure( false, _Key, _Lookup ).

ense_chromosome( 'X' ) :- !.
ense_chromosome( 'Y' ) :- !.
ense_chromosome( N ) :- integer( N ), !.
