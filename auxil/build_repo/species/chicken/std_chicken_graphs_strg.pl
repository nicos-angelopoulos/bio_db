
:- set_prolog_flag(stack_limit, 18 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).
:- lib(debug_call).  % debuc/1,3

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
% :- lib(bio_db).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(stoics_lib:message_report/3).
:- lib(stoics_lib:portray_clauses/2).

% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% local libs & sources
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).     % bio_db_add_infos_to/2.
:- lib(bio_db_source_base_url/2).
:- lib(portray_informed_clauses/4).
:- lib(url_file_local_date_mirror/3).
:- lib(std_graphs_strg_auto_version/1).
:- lib(bio_db_string_version_base_name/4). % here: replace old call below with two calls to this, one with links and one with info

:- debuc(by_unix).

std_chicken_graphs_strg_defaults( Args, Defs ) :-
               Defs = [  db(strg),
                         debug(true),
                         debug_url(false),
                         iactive(true),
                         org(chicken),
                         relation(links)
                         |  T  
                      ],
     ( std_graphs_strg_auto_version(Vers,Args) ->     % let options/2 do the erroring, cause user might provide it
          T = [string_version(Vers)]
          ;
          T = []   
     ).

% last good one: std_graphs_string( '10' ).  2016/09/08
% last good one: std_graphs_string( '10.5' ).  2018/03/30

/** std_chicken_graphs_strg(+Opts).

String graphs and a map for chicken products.

Opts
  * db(Db=string)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * org(Org=chicken)
    organism
  * relation(Rel=links)
    relation of STRING we are interested in (bio_db_string_version_base_name/4)
  * string_version(Vers)
    default is collected by visiting the STRING web-page (missing from defaults if collection failed)

==
?- std_chicken_graphs_str([]).

ορέστης;dnloads/strg% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/strg
ορέστης;dnloads/strg% date
Tue 27 Dec 14:01:05 GMT 2022
ορέστης;dnloads/strg% wc -l graphs/strg_g*
  7821425 graphs/strg_galg_edge_ensp.pl
  3910716 graphs/strg_galg_edge_symb.pl
 11732141 total
ορέστης;dnloads/strg% wc -l maps/strg_galg_ensp_symb.pl
15515 maps/strg_galg_ensp_symb.pl

==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
std_chicken_graphs_strg( Args ) :-
    Self = std_chicken_graphs_strg,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    options( string_version(VersionPrv), Opts ),
    ( number(VersionPrv) -> atom_number(Version,VersionPrv); Version = VersionPrv ),
    debuc( Self, 'Version: ~w', Version ),
    bio_db_string_version_base_name( Version, Bname, Remote, Opts ),
    bio_db_string_version_base_name( Version, _InfoBnameVanilla, InfoFrom, [relation(info)|Opts] ),
    % std_graphs_string_version_base_name( Version, Bname, InfoBname, From, InfoFrom ),
    debuc( Self, 'Base name: ~w', Bname ),
    absolute_file_name( bio_db_build_downloads(strg), Parent ),
    % absolute_file_name( baio_db_downloads(string/Bname), LocalFile ),
    % directory_file_path( Parent, _BnameAgain, LocalFile ),
    % directory_file_path( Parent, Bname, LocalFile ),
    os_make_path( Parent, debug(true) ),
    url_file_local_date_mirror( Remote, Parent, [file(DnF),dnt(true),iface(wget)|Opts] ),
    % std_graph_string_download_string( LocalFile, From, Self, Opts ),
    working_directory( Here, Parent ),
    debuc( Self, 'Download name: ~w', DnF ),
    @ gunzip( -k, DnF ),  % keeps .gz file
    % @ gunzip( '9606.protein.links.v10.txt.gz' ),
    EnspPn = strg_galg_edge_ensp,
    file_name_extension( TxtF, gz, DnF ),
    debuc( Self, 'Directory: ~p', [Parent] ),
    Mess1 = 'Converting string file: ~p, to Prolog',
    debuc( Self, Mess1, [TxtF] ),
    MtxOpts = [ csv_read(separator(0' )),predicate_name(EnspPn),
            rows_transform(maplist(user:de_gallus)),header_remove(true) ],
    debuc( Self, 'mtx_prolog options: ~w', [MtxOpts] ),
    mtx_prolog( TxtF, File, MtxOpts ),
    debuc( Self, 'Edges output: ~w', File ),
    delete_file( TxtF ),
    os_make_path( graphs, debug(true) ),
    os_make_path( maps, debug(true) ),
    os_dir_stem_ext( graphs, EnspPn, pl, EnspRel ),
    @ rm( -f, EnspRel ),
    @ mv( File, EnspRel ),
    consult( EnspPn:EnspRel ),
     
    % info file connect protein to SYmbol
    % directory_file_path( Parent, InfoBname, LocalInfoFile ),
    % std_graph_string_download_string( LocalInfoFile, InfoFrom, Self, Opts ),
    url_file_local_date_mirror( InfoFrom, Parent, [file(InfoBname),dnt(true),iface(wget)|Opts] ),
    @ gunzip( -k, InfoBname ),  % keeps .gz file
    % Map = map_strg_gallus_ensp_symb,
    Map = strg_galg_ensp_symb,
    file_name_extension( InfoTxtF, gz, InfoBname ),
    InfoMess1 = 'Converting map string file: ~p, to Prolog',
    debuc( Self, InfoMess1, [InfoTxtF] ),
    mtx( InfoTxtF, InfoMtx, sep(tab) ),
    InfoMtx = [_|InfoRows],
    maplist( strg_map_row(Map), InfoRows, MapRows ),
    os_dir_stem_ext( MapPlF, [odir(maps),ext(pl),stem(Map)] ),
    bio_db_dnt_times( InfoBname, InfoDnDt, _InfoEndDt ),
    MapInfos = [ source-InfoFrom,datetime-InfoDnDt,header-header('Ensembl Protein ID','Symbol'),
                       data_types-data_types(atom,atom)
               ],
    portray_informed_clauses( MapRows, MapInfos, MapPlF, [] ),
    % mtx( InfoBname, MapRows ),
    debuc( Self, 'wrote, and consulting: ~p', [MapPlF] ),
    Map:consult(MapPlF),

     strg_chicken_symbolise_edges( Self, EnspPn, EnspRel, Map, UnoSymbEdges ),

    sort( UnoSymbEdges, SymbEdges ),
    length( SymbEdges, SymbEdgesLen ),
    debuc( Self, 'unique symbol edges (gallus): ~w', [SymbEdgesLen] ),
    EdgeSymbsF = 'graphs/strg_galg_edge_symb.pl',
    bio_db_dnt_times( DnF, DnDt, _EndDt ),
    EdgeSymbsInfos = [ source-From,datetime-DnDt,header-header('Symbol','Symbol',weight),
                       data_types-data_types(atom,atom,integer)
                     ],
    portray_informed_clauses( SymbEdges, EdgeSymbsInfos, EdgeSymbsF, [] ),
    debuc( Self, 'Portrayed onto: ~p', [EdgeSymbsF] ),

    BaseOpts = [ source(From), datetime(DnDt),
                  header(row('Ensembl_Protein','Ensembl_Protein',weight))
                ],
    debuc( Self, 'doing infos for: ~p', [EnspRel] ),
    bio_db_add_infos_to( BaseOpts, EnspRel ),
    link_to_bio_sub( strg, EnspRel, [org(gallus),type(graphs)] ),
    link_to_bio_sub( strg, EdgeSymbsF, [org(gallus),type(graphs)] ),
    link_to_bio_sub( strg, MapPlF, [org(gallus),type(maps)] ),
    delete_file( InfoTxtF ),
    working_directory( _, Here ).

/* this is the old implementation:
strg_chicken_symbolise_edges( Self, EnspPn, EnspRel, Map, UnoSymbEdges ) :-
    findall( strg_galg_edge_symb(SymbA,SymbB,W),
                         ( EnspPn:strg_galg_edge_ensp(EnsP1,EnsP2,W),
                           Map:strg_galg_ensp_symb(EnsP1,Symb1),
                           Map:strg_galg_ensp_symb(EnsP2,Symb2),
                           sort_four(Symb1,Symb2,SymbA,SymbB)
                     ),
            UnoSymbEdges
          ).
*/

/* new implementation
*/
strg_chicken_symbolise_edges( Self, EnspPn, EnspRelF, Map, Edges ) :-
     open( EnspRelF, read, InS ),
     read( InS, Term ),
     debuc( Self, task(start), symbolise(streamed) ),
     strg_chicken_symbolise_edges_stream( Term, EnspPn, Map, InS, Edges ),
     debuc( Self, task(stop), symbolise(streamed) ),
     close( InS ).

strg_chicken_symbolise_edges_stream( end_of_file, _Pn, _Map, _InS, Edges ) :-
     !,
     [] = Edges.
strg_chicken_symbolise_edges_stream( InTerm, Pn, Map, InS, Edges ) :-
     ( functor(InTerm,Pn,3) ->
               arg( 1, InTerm, EnsP1 ),
               arg( 2, InTerm, EnsP2 ),
               arg( 3, InTerm, W     ),
               ( (Map:strg_galg_ensp_symb( EnsP1, Symb1 ),
                  Map:strg_galg_ensp_symb( EnsP2, Symb2 )) ->
                         sort_four( Symb1, Symb2, SymbA, SymbB ),
                         [strg_galg_edge_symb(SymbA,SymbB,W)|TEdges] = Edges
                         ;
                         TEdges = Edges
               )
               ;
               throw(rogue_ensp_to_symb_term(InTerm))
     ),
     read( InS, NxtTerm ),
     strg_chicken_symbolise_edges_stream( NxtTerm, Pn, Map, InS, TEdges ).

strg_map_row( Pname, InfoRow, Term ) :-
     arg( 1, InfoRow, PfxProt ),
     arg( 2, InfoRow, Symb ),
     atom_concat( '9031.', Prot, PfxProt ),
     Term =.. [Pname,Prot,Symb].

/*
ensp_mouse_symb( EnsP, Symb ) :-   % fixme: make sure the cut is green ! 
    unip:map_unip_mouse_ensp_unip( EnsP, Unip ),
    (   unip:map_unip_mouse_mgim_unip(Mgim,Unip)
        ; 
        mgim:map_mgim_mouse_mgim_unip(Mgim,Unip)
    ),
    mgim:map_mgim_mouse_mgim_symb( Mgim, Symb ),
    !.
    */

sort_four( X, Y, A, B ) :-
    Y @< X,
    !,
    A = Y, B = X.
sort_four( A, B, A, B ).

/*
std_graph_string_download_string( LocalFile, _From, Self, _Opts ) :-
    exists_file( LocalFile ),
    debuc( Self, 'Using existing local string file: ~p', LocalFile ),
    !.
std_graph_string_download_string( Local, Remote, Self, Opts ) :-
    debuc( Self, 'Downloading from: ~p', Remote ),
    % url_file( Remote, Local, [dnt(true),iface(wget),verb(Verb)] ),
    url_file_local_date_mirror( Remote, Local, [dnt(true),iface(wget)|Opts] ),
    debuc( Self, '... to local file: ~p', Local ).
    */

std_graphs_string_version_base_name( VersionPrv, Bname, InfoBname, Remote, InfoRemote ) :-
    ( atom_concat(v,Version,VersionPrv)->true;Version=VersionPrv ),
    atom_concat( v, Version, Vied ),
    % Pfx = 'https://string-db.org/download/protein.links.v',
    bio_db_source_base_url( string, DnldBaseUrl ),
    atomic_list_concat( [9031,protein,links,Vied,txt,gz], '.', Bname ),
    atomic_list_concat( [DnldBaseUrl,'protein.links.v',Version,'/',Bname], Remote ),
    % 10/9606.protein.links.v10.txt.gz
    % 9031.protein.info.v11.5.txt
    % InfoPfx = 'https://stringdb-static.org/download/protein.info.v',
    % atom_concat( InfoPfx, Version, InfoRemoteDir ),
    atomic_list_concat( [9031,protein,info,Vied,txt,gz], '.', InfoBname ),
    atomic_list_concat( [DnldBaseUrl,'protein.info.v',Version,'/',InfoBname], InfoRemote ).

de_gallus( row(MousEnsP1,MousEnsP2,WAtm), row(EnsP1,EnsP2,W) ) :-
    atom_concat( '9031.', EnsP1, MousEnsP1 ),
    atom_concat( '9031.', EnsP2, MousEnsP2 ),
    ( number(WAtm) -> W = WAtm; atom_number(WAtm,W) ),
    !.
de_gallus( Row, _ ) :-
    debuc( _, 'Failed to translate row: ~w', Row ),
    abort.

/*
bio_db_std_string :-
    Opt = [ csv_read(separator(0' )), predicate_name(hs_strg_edge),
            rows_transform(maplist(user:de_mouse))
         ],
    mtx_prolog( bio_dn(strg/'protein.links.mouse.txt'), File, Opt ),
    debuc( _, 'Edges output: ~w', File ),
    bio_db_std_string_link( File ).

bio_db_std_string_link( File ) :-
    HsBio = bio_db_build_data( 'graphs/strg/mouse_strg_edge.pl' ),
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
    */
