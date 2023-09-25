
:- use_module(library(filesex)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:io_lines/2).

% also sets lib alias that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(de_semi/3).
:- lib(sep_split/3).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2
:- lib(url_file_local_date_mirror/3).
:- lib(map_list_options/3).

true(_,_).

std_chicken_maps_cgnc_defaults( Defs ) :-
    Defs = [        cgnc_file('cgnc_complete_set.txt'),
                    db(cgnc),
                    db_dir(cgnc),  % is this used ?
                    debug(true),
                    debug_fetch(true),
                    debug_url(false),
                    download(true),
                    iactive(true),
                    org(chicken),
                    sub(maps)
           ].

/** std_chicken_maps_cgnc( +Opts ).

Create ID maps from CGNC's main download data file.

Input is the tab delimited file containing all CGNC public data. 
The fields in the file are: CGNC ID, Entrez Gene ID, Ensembl Gene ID,
gene symbol, gene name, gene synonym, curation status and last edit date.

In the interest of completeness we create a map for each column based on CGNC ID (cgnc).

Tokens
  * cgnc('CGNC id')
    chicken gene nomenclature consortium
  * ncbi('Entrez')
    Ncbi/Entrez gene ids 
  * ensg('Gene id')
    Ensembl gene ids
  * symb('gene symbol')
    Gene symobls
  * name('gene name')
    Gene, long, name.
  * syno('gene synonym')
    Gene synonym(s) - check syntax for multies
  * curs('curation status')
    Curation status (should be a factor)
  * edat('last edit date')
    should be a date data type

Opts
  * db(Db=cgnc)
    source database
  * db_dir(DbDir=cgnc)
    relative directory within downloads and data to work in
  * debug(Dbg=true)
    progress, informational messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * download(Dn=true)
    set to false to skip downloading a fresh copy of the CGNC file(s)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * org(Org=chicken)
    organism
  * sub(Dir=maps)
    sub-directory for creating the maps

==
?- std_chicken_maps_cgnc.
?- cd( '$HOME/.local/share/swi-prolog/pack/Downloads/bio_db-22.12.17/maps/cgnc' ).
?- shell( 'wc -l *' ).

==

@author nicos angelopoulos
@version  0.1 2022/12/17,  from hgnc
@tbd convert to url_..._mirror.pl
@see http://birdgenenames.org/cgnc/downloads.jsp?file=standard
*/
std_chicken_maps_cgnc :-
    std_chicken_maps_cgnc( [] ).

std_chicken_maps_cgnc( Args ) :-
    Self = std_chicken_maps_cgnc,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    build_dnload_loc( Self, DnDir, Opts ),
    options( db_dir(RelDir), Opts ),
    options( download(Dnload), Opts ),
    cgnc_download_file( Dnload, DnDir, Self, SrcUrl, [file(CsvF)|Opts] ),
    working_directory( Old, Dir ),
    os_ext( dnt, CsvF, DntF ),
    bio_db_dnt_times( DntF, DnDt, _DnEnd ),
    options( sub(SubDir), Opts ),
    make_directory_path( SubDir ),
    std_chicken_cgnc_mtx( CsvF, Mtx ),
    % 23.06.05, the cgnc file introduced a /9 (instead of /8 line in late May).
    % mtx( CsvF, Mtx, sep(tab) ),
    StdO= [dir(SubDir),cnm_transform(cgnc_cname)|Opts],
    Cgnc = 'CGNC id',
    Symb = 'gene symbol',
    Ncbi = 'Entrez Gene id',
    Ensg = 'Ensembl Gene id',
    Name = 'gene name',
    Syno = 'gene synonym',
    Curs = 'curation status',
    Edat = 'last edit date',

    cgnc_std_map( Cgnc, Symb, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, SymbF ),    % cgnc_symb
    cgnc_std_map( Cgnc, Ncbi, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, NcbiF ),    % cgnc_ncbi
    cgnc_std_map( Cgnc, Ensg, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, EnsgF ),    % cgnc_ensg
    cgnc_std_map( Cgnc, Name, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, NameF ),    % cgnc_name
    cgnc_std_map( Cgnc, Syno, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, SynoF ),    % cgnc_syno
    cgnc_std_map( Cgnc, Curs, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, CursF ),    % cgnc_curs
    cgnc_std_map( Cgnc, Edat, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, EdatF ),    % cgnc_edat
    debuc( Self, 'doing links...', [] ),
    Files = [SymbF,NcbiF,EnsgF,NameF,SynoF,CursF,EdatF],
    % maplist( link_to_bio_sub(RelDir), Files ),
    map_list_options( link_to_bio_sub(RelDir), Files, call_options([org(chicken),type(maps)]) ),
    % file_name_extension( TxtF, gz, GzF ),
    % delete_file( TxtF ),
    working_directory( _, Old ).

cgnc_std_map( Cid1, Cid2, CsvF, Csv, Self, StdO, SrcUrl/DnDt, OutF ) :-
    cgnc_std_column_to_value_call( Cid1, Call1 ),
    cgnc_std_column_to_value_call( Cid2, Call2 ),
    Opts = [to_value_1(Call1),to_value_2(Call2)|StdO],
    % debuc( std_maps_hgnc, 'doing file for columns: ~w, ~w', [Cid1, Cid2] ),
    csv_ids_map( CsvF, Cid1, Cid2, Csv, OutF, [source(SrcUrl),datetime(DnDt)|Opts] ),
    debuc( Self, 'deposited on: ~w (columns: ~w, ~w)', [OutF,Cid1,Cid2] ).

cgnc_std_column_to_value_call( 'CGNC id', pos_integer ). % check they are unique and present (key) % de_semi('CGNC') ).
cgnc_std_column_to_value_call( 'Entrez Gene id', pos_integer ).
cgnc_std_column_to_value_call( 'Ensembl Gene id', non_empty_atom ).   % fixme: prefixed
cgnc_std_column_to_value_call( 'gene symbol', non_empty_atom ).
cgnc_std_column_to_value_call( 'gene name', non_empty_atom ).
cgnc_std_column_to_value_call( 'gene synonym', sep_split('|') ).      % fixme: check for multis
cgnc_std_column_to_value_call( 'curation status', non_empty_atom ).  
cgnc_std_column_to_value_call( 'last edit date', non_empty_atom ).    % fixme: a date
    
cohese_ensebl_gene_id( Dom, _Subo, Gid ) :-
    Dom \== '', 
    !,
    Gid = Dom.
cohese_ensebl_gene_id( _Dom, Subo, Gid ) :-
    Subo \== '', 
    !,
    Gid = Subo.
cohese_ensebl_gene_id( _Dom, _Subo, '' ).

cohese_gene_id( Dom, _Subo, Gid ) :-
    number( Dom ),
    Dom > 0,
    !,
    Gid = Dom.
cohese_gene_id( _Dom, Subo, Gid ) :-
    number( Subo ),
    Subo > 0, 
    !,
    Gid = Subo.
cohese_gene_id( _Dom, _Subo, '' ).
    
pos_integer( Numb, Numb ) :-
    integer( Numb ),
    Numb > 0.

non_empty_atom( Other, NonEmpty ) :-
    Other \== '',
    NonEmpty = Other.

cgnc_cname( A, B ) :-
    cgnc_cname_known( A, B ),
    !.
cgnc_cname( A, A ).

% cmpl = complement = both currated by HGNC and supplied by respective database.
% 
% CGNC id	Entrez Gene id	 Ensembl Gene id gene symbol	gene name	gene synonym	curation status	last edit date
cgnc_cname_known( 'CGNC id', cgnc ).
cgnc_cname_known( 'Entrez Gene id', ncbi ).
cgnc_cname_known( 'Ensembl Gene id', ensg ).
cgnc_cname_known( 'gene symbol', symb ).
cgnc_cname_known( 'gene name', name ).
cgnc_cname_known( 'gene synonym', syno ).
cgnc_cname_known( 'curation status', curs ).
cgnc_cname_known( 'last edit date', edat ).

% 23.06.05, the cgnc file introduced a /9 (instead of /8 line in late May).
% in next version revert back to the mtx/3 call above
std_chicken_cgnc_mtx( CsvF, Mtx ) :-
    mtx( CsvF, Csv, [match(false),sep(tab)] ),
     std_chicken_cgnc_mtx_fix( Csv, Mtx ).

std_chicken_cgnc_mtx_fix( [], [] ).
std_chicken_cgnc_mtx_fix( [H|Rs], [R|M] ) :-
     functor( H, _, Arity ),
     ( Arity =:= 8 -> 
          R = H   
          ;
          Arity =:= 9,
          H =.. [Func,89661,121109795,'','HSFX1','heat shock transcription factor','X1','','Approved','2023-05-23'],
          R =.. [Func,89661,121109795,'','HSFX1','heat shock transcription factor X1','','Approved','2023-05-23']
     ),
     std_chicken_cgnc_mtx_fix( Rs, M ).

cgnc_download_file( true, DnDir, Self, Url, Opts ) :-
     % Url = 'http://birdgenenames.org/cgnc/downloads.jsp?file=standard',
     options( debug_fetch(Fbg), Opts ),
     url_file_local_date_mirror( Url, DnDir, [date(prefix),debug(Fbg),interface(wget)|Opts] ),
     cgnc_download_file_fix( Self, Dst ).
cgnc_download_file( false, _DnDir, Self, Url, Opts ) :-
     memberchk( file(File), Opts ),
     options( cgnc_file(File), Opts ),
     debuc( Self, 'Asked not to download: ~p', [File] ),
     Url = 'http://birdgenenames.org/cgnc/downloads.jsp?file=standard'.

cgnc_download_file_fix( Self, Dst ) :-
     io_lines( Dst, Lines ),
     debuc( Self, length, cgnc_dnload/Lines ),
     Lines = [[13],BCs|Tines],
     atom_codes( B, BCs ),
     at_con( BParts, '\t', B ),
     % debuc( Self, enum, cgnc_2nd_line_parts/BParts ),
     BParts = ['CGNC id','Entrez Gene id','gene symbol','gene name','gene synonym','curation status','last edit date'],
     BParts = [CGNC,Ncbi,Symb,Name,Syno,Curs,Edat],
     !,
     NewBParts = [CGNC,Ncbi,'Ensembl Gene id',Symb,Name,Syno,Curs,Edat],
     at_con( NewBParts, '\t', NewB ),
     atom_codes( NewB, NewBCs ),
     io_lines( Dst, [NewBCs|Tines] ).
cgnc_download_file_fix( Self, Dst ) :-
     debuc( Self, 'CGNC download seems in good order: ~p', [Dst] ).
