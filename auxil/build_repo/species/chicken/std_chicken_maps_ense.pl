
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
:- lib(csv_to_pl/2).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2
:- lib(link_to_bio_sub/3).

std_chicken_maps_ense_defaults(debug(true)).

/** std_chicken_maps_ense( +Opts ).

Maps based on ensembl .gtf file.

fixme:  map_ense_gallus_ensg_symb.pl, 2nd arg contains "faf" and ENSG for some symbols, 
although the later might be margianlly useful ?  

Currently only gene symbols, but as per human it should be trivial to do sequences.

  * ense: the database abbv.

==
?- std_chicken_maps_ense([]).

ορέστης;build_repo/chicken% date ; pupsh std_chicken_maps_ense.pl ; date
Tue 27 Dec 13:04:38 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
...
% ...Done
Tue 27 Dec 13:09:40 GMT 2022

==

@author nicos angelopoulos
@version  0:1 2020/9/10
@tbd transcripts (see ../human/std_maps_ense.pl).

*/

std_chicken_maps_ense( Args ) :-
    std_chicken_maps_ense( galg, gallus_gallus, Args ),
    std_chicken_maps_ense( gg6a, gallus_gallus_gca000002315v5, Args ).

std_chicken_maps_ense( Tkn, EnsDir, Args ) :-
    Self = std_chicken_maps_ense,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    absolute_file_name( bio_db_build_downloads(ense), DnDir ),
    os_make_path( DnDir ),
    debuc( Self, 'Downloads dir for ense: ~p', DnDir ),
    % FtpDir = 'ftp://ftp.ensembl.org/pub/current_gtf/gallus_gallus/',
    at_con( ['ftp://ftp.ensembl.org/pub/current_gtf/',EnsDir,'/'], '', FtpDir ),
    % FtpDir = 'https://ftp.ensembl.org/pub/current_gtf/gallus_gallus/',
    Found @@ curl( -l, '--no-progress-meter', FtpDir ),
    % Gallus_gallus.bGalGal1.mat.broiler.GRCg7b.108.gtf.gz
    % Mus_musculus.GRCm39.108.gtf.gz
    std_gallus_ense_gtf_file( Tkn, Found, MsGtfF ),
    atom_concat( FtpDir, MsGtfF, Url ),
    url_file_local_date_mirror( Url, DnDir, [file(File),interface(wget)] ),
    debuc( Self, 'Dnload done, file is: ~p', File ),
    working_directory( Old, DnDir ),
    bio_db_dnt_times( File, DnDt, _DnEn ),
    debuc( by_unix ),
    os_un_zip( File, _, [keep(true),on_exists(skip),debug(true)] ),
    os_ext( gz, Stem, File ),
    debuc( Self, 'Reading from: ~p', [Stem] ),
    mtx( Stem, Rows, [sep(tab),csv_read(skip_header('#'))] ),
    debuc( Self, length, rows/Rows ),
    % ense_genes( Rows, EnsGHRows, EnsGSRows, EnsGCRows ),
    ense_transcripts( Rows, EnsTGRows, EnsTLRows ),
    debuc( Self, length, tg_rows/EnsTGRows ),
    debuc( Self, length, tl_rows/EnsTLRows ),
    at_con( [ense,Tkn,enst,'ensg.csv'], '_', EnsTGF ),
    % mtx( 'map_ense_gallus_enst_ensg.csv', EnsTGRows ),
    mtx( EnsTGF, EnsTGRows ),
    at_con( [ense,Tkn,enst,'chrl.csv'], '_', EnsTLF ),
    mtx( EnsTLF, EnsTLRows ),
    % mtx( 'map_ense_gallus_enst_chrl.csv', EnsTLRows ),

    ense_genes( Rows, Self, _EnsGMRows, EnsGSRows, EnsGCRows ),
    Lbls = [gtfRows,ensGM,ensGS,ensGC],
    % ERws = [Rows,EnsGMRows, EnsGSRows, EnsGCRows],
    ERws = [Rows, EnsGSRows, EnsGCRows],
    debuc( Self, length, Lbls/ERws ),
    %
    sort( EnsGSRows, EnsGSRowsSet ),

     at_con( [ense,Tkn,ensg,'symb.csv'], '_', EnsGSF ),
     % mtx( 'map_ense_gallus_ensg_symb.csv', EnsGSRowsSet ),
     mtx( EnsGSF, EnsGSRowsSet ),
     at_con( [ense,Tkn,ensg,'chrl.csv'], '_', EnsGLF ),
     mtx( EnsGLF, EnsGCRows  ),
     % mtx( 'map_ense_gallus_ensg_chrl.csv', EnsGCRows ),

     Csvs = [ 
                 EnsTGF,
                 % 'map_ense_gallus_enst_ensg.csv', 
                 EnsTLF,
                 % 'map_ense_gallus_enst_chrl.csv',
                 % % 'map_ense_gallus_ensg_mgim.csv',
                 EnsGSF,
                 % 'map_ense_gallus_ensg_symb.csv',
                 EnsGLF
                 % 'map_ense_gallus_ensg_chrl.csv'
            ],
     debuc( Self, 'mapping: ~w', [Csvs] ),
     maplist( csv_to_pl(Self), Csvs ),
     maplist( new_ext(pl), Csvs, Pls ),
     AddOpts = [source(Url),datetime(DnDt)],
     Headers = [    
          row('Ensembl Transcript','Ensembl Gene'),
          row('Ensembl Transcript','Chromosome', 'Start', 'End', 'Direction'),
          % row('Ensembl ID','MGIM ID'),
          row('Ensembl Gene ID','Symbol'),
          row('Ensembl Gene ID','Chromosome', 'Start', 'End', 'Direction')
        ],
    findall( PlF, ( nth1(N,Pls,PlF),    nth1(N,Headers,Header),
                         debuc( Self, 'ingoing prolog file: ~p', PlF ),
                         debuc( Self, '...with header: ~w and options: ~w', [Header,AddOpts] ),
                         bio_db_add_infos_to([header(Header)|AddOpts], PlF)
                         ),
                         _PlFs ),
     os_make_path( maps ),
     maplist( mv_to_sub(maps), Pls ),
    @ rm( -f, Stem ),
     working_directory( _, maps ),
    Cpts = call_options([org(gallus)]),
     map_list_options( link_to_bio_sub(ense), Pls, Cpts ),
     working_directory( _, Old ),
     debuc( Self, '...Done', true ).

std_gallus_ense_gtf_file( galg, Found, MsGtfF ) :-
    findall( MsGtf-Amb-Rel, (
                         member(MsGtf,Found),
                         at_con(['Gallus_gallus','bGalGal1',mat,broiler,GRChTkn,RelAtm,gtf,gz],'.',MsGtf),
                         % atom_concat('GRCg',AmbAtm,GRChTkn),
                         atom_concat('GRCg',Amb,GRChTkn),
                         % atom_number(AmbAtm,Amb),
                         atom_number(RelAtm,Rel),
                         write( mar(MsGtf,Amb,Rel) ), nl
                        ),
                            MsGtfs ),
    ( MsGtfs = [MsGtfF-_Amb-_Rel] ->
        true
        ;
        throw( non_unique_auto_ided_ense_gtf_file(MsGtfs) )
    ).
std_gallus_ense_gtf_file( gg6a, Found, MsGtfF ) :-
    write( found(Found) ), nl,
    % Gallus_gallus_gca000002315v5.GRCg6a.108.gtf.gz
    findall( MsGtf-Amb-Rel, (
                         member(MsGtf,Found),
                         at_con(['Gallus_gallus_gca000002315v5',GRChTkn,RelAtm,gtf,gz],'.',MsGtf),
                         % atom_concat('GRCg',AmbAtm,GRChTkn),
                         atom_concat('GRCg',Amb,GRChTkn),
                         % atom_number(AmbAtm,Amb),
                         atom_number(RelAtm,Rel),
                         write( mar(MsGtf,Amb,Rel) ), nl
                        ),
                            MsGtfs ),
    ( MsGtfs = [MsGtfF-_Amb-_Rel] ->
        true
        ;
        throw( non_unique_auto_ided_ense_gtf_file(MsGtfs) )
    ).

mv_to_sub( Sub, File ) :-
     os_path( Sub, File, Rel ),
     rename_file( File, Rel ).

new_ext( New, File, NewFile ) :-
     os_ext( _Old, New, File, NewFile ).

ense_genes( [], _Self, [], [], [] ).
ense_genes( [RowG|Rows], Self, GMRows, GSRows, [EnsGC|GCRows] ) :-
     RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,InfoG),
     EnsGC= row(EnsG,ChrG,SrtG,EndG,DirG),
     ense_info( gene_id, InfoG, EnsG ),
     ( ense_info(gene_name,InfoG,fail,Symb) ->
          GSRows = [row(EnsG,Symb)|TGSRows]
          ;
          GSRows = TGSRows
     ),
     !,
     ense_genes( Rows, Self, GMRows, TGSRows, GCRows ).
ense_genes( [RowG|Rows], _Self, _, _, _ ) :-
     RowG = row(_ChrG,_Db,gene,_SrtG,_EndG,_,_DirG,_,_InfoG),
     !,
     length( Rows, Len ),
     throw( tripped_on_gene_row(RowG,Len) ).
ense_genes( [_RowG|Rows], Self, GMRows, GSRows, GCRows ) :-
     ense_genes( Rows, Self, GMRows, GSRows, GCRows ).

ense_transcripts( [], [], [] ).
ense_transcripts( [RowT|Rows], [EnsTG|TGRows], [EnsTL|TLRows] ) :-
     RowT = row(ChrT,_Db,transcript,SrtT,EndT,_,DirT,_,InfoT),
     % findall( row(EnsT,EnsG)-row(EnsT,ChrT,SrtT,EndT,DirT), ( )).
     ense_info( transcript_id, InfoT, EnsT ),
     ense_info( gene_id, InfoT, EnsG ),
     % ense_chromosome( ChrT ),
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
% fail if false
ense_info_failure( false, _Key, _Lookup, false ).

ense_chromosome( 'X' ) :- !.
ense_chromosome( 'Y' ) :- !.
ense_chromosome( N ) :- integer( N ), !.