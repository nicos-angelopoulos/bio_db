
:- set_prolog_flag(stack_limit, 2 000 000 000).

:- use_module(library(lists)).
:- use_module(library(apply)).

:- use_module(library(lib)).
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).

:- lib(stoics_lib:at_con/3).

% also loads lib locations for locals below
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local
:- lib(csv_to_pl/1).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).

:- debuc(std_mouse_maps_ense).

std_maps_ense_defaults(debug(true)).

/** std_mouse_maps_ense( +Opts ).

Maps based on ensembl .gtf file.

Currently only gene symbols, but as per human it should be trivial to do
sequences.

  * ense: the database abbv.

@author nicos angelopoulos
@version  0:1 2020/9/10
@tbd transcripts (see std_maps_ense.pl).

*/

std_mouse_maps_ense( Args ) :-
    Self = std_maps_ense,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
	absolute_file_name( bio_db_build_downloads(ense), DnDir ),
	os_make_path( DnDir ),
	debuc( Self, 'Downloads dir for ense: ~p', DnDir ),
    FtpDir = 'ftp://ftp.ensembl.org/pub/current_gtf/mus_musculus/',
    Found @@ curl( -l, '--no-progress-meter', FtpDir ),
    findall( MsGtf-Amb-Rel, (
                         member(MsGtf,Found),
                         at_con(['Mus_musculus',GRChTkn,RelAtm,gtf,gz],'.',MsGtf),
                         atom_concat('GRCm',AmbAtm,GRChTkn),
                         atom_number(AmbAtm,Amb), atom_number(RelAtm,Rel)
                        ),
                            MsGtfs ),
    ( MsGtfs = [MsGtfF-_Amb-_Rel] ->
        true
        ;
        throw( non_unique_auto_ided_ense_gtf_file(MsGtfs) )
    ),
    atom_concat( FtpDir, MsGtfF, Url ),
	url_file_local_date_mirror( Url, DnDir, [file(File),interface(wget)] ),
	debuc( Self, 'Dnload done, file is: ~p', File ),
	working_directory( Old, DnDir ),
	bio_db_dnt_times( File, DnDt, _DnEn ),
	debuc( by_unix ),
	os_un_zip( File, _, [keep(true),on_exists(skip),debug(true)] ),
	os_ext( gz, Stem, File ),
	os_ext( tab, Stem, TabF ),
    % fixme: swi has skipping of initial comment lines...
	atomic_list_concat( [grep,'-v','"^#"',Stem,'>',TabF], ' ', Shell ),
    debuc( Self, '~w, shelling/1 : ~w', [Self,Shell] ),
	shell( Shell ),
    debuc( Self, 'Reading from: ~p', [TabF] ),
	mtx( TabF, Rows, sep(tab) ),
	debuc( Self, length, rows/Rows ),
    trace,
	% ense_genes( Rows, EnsGHRows, EnsGSRows, EnsGCRows ),
    ense_transcripts( Rows, EnsTGRows, EnsTLRows ),
	mtx( 'map_mouse_ense_enst_ensg.csv', EnsTGRows ),
	mtx( 'map_mouse_ense_enst_chrl.csv', EnsTLRows ),

	ense_genes( Rows, EnsGSRows, EnsGCRows ),
    %
    % mtx( 'map_mouse_ense_ensg_msgi.csv', EnsGHRows ),
    sort( EnsGSRows, EnsGSRowsSet ),
    sort( EnsGCRows, EnsGCRowsSet ),
	mtx( 'map_mouse_ense_ensg_symb.csv', EnsGSRowsSet ),
	mtx( 'map_mouse_ense_ensg_chrl.csv', EnsGCRowsSet ),
    	Csvs = [ 
                 'map_mouse_ense_enst_ensg.csv', 'map_mouse_ense_enst_chrl.csv',
		         'map_mouse_ense_ensg_symb.csv', 'map_mouse_ense_ensg_chrl.csv'
	       ],
	debuc( Self, 'mapping: ~w', [Csvs] ),
	maplist( csv_to_pl, Csvs ),
	maplist( new_ext(pl), Csvs, Pls ),
	AddOpts = [source(Url),datetime(DnDt)],
	Headers = [	
		row('Ensembl Transcript','Ensembl Gene'),
		row('Ensembl Transcript','Chromosome', 'Start', 'End', 'Direction'),
		row('Ensembl Gene','MGIM Symbol'),
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
	maplist( link_to_map_sub(ense), Pls ),
	working_directory( _, Old ),
	debuc( Self, '...Done', true ).
                    
ense_genes( [], [], [] ).
ense_genes( [RowG|Rows], [GSRow|TGSRows], [EnsGC|GCRows] ) :-
	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,InfoG),
	EnsGC= row(EnsG,ChrG,SrtG,EndG,DirG),
	ense_info( gene_id, InfoG, EnsG ),
	ense_info( gene_name, InfoG, Symb ),
	% ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ),
    % fixme: check Symb is an mgim symbol ?
    GSRow = row(EnsG,Symb),
	!,
	ense_genes( Rows, TGSRows, GCRows ).
ense_genes( [RowG|Rows], _, _ ) :-
	RowG = row(_ChrG,_Db,gene,_SrtG,_EndG,_,_DirG,_,_InfoG),
	!,
	length( Rows, Len ),
	throw( tripped_on_gene_row(RowG,Len) ).
ense_genes( [_RowG|Rows], GSRows, GCRows ) :-
	ense_genes( Rows, GSRows, GCRows ).

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
