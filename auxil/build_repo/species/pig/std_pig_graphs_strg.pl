
:- set_prolog_flag(stack_limit, 20 000 000 000).
% :- set_prolog_flag(stack_limit, 18 000 000 000).

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
:- lib(stoics_lib:url_file/3).

% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% local libs & sources
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).             % bio_db_add_infos_to/2.
:- lib(build_dnload_loc/3).
:- lib(portray_informed_clauses/4).
:- lib(url_file_local_date_mirror/3).
:- lib(std_graphs_strg_auto_version/1).
:- lib(bio_db_string_version_base_name/5).

:- debuc(by_unix).
:- debuc(std_graphs_strg). % fixme:

std_pig_graphs_strg_defaults( Args, Defs ) :- 
                    Defs = [  db(strg),
                              debug(true),
                              debug_fetch(true),
                              debug_url(false),
                              iactive(true),
                              relation(links),
                              org(pig)
                              | T
                         ],
          ( std_graphs_strg_auto_version(Vers,Args) -> % let options/2 do the erroring
                                                       % because user might provide it
                    T = [string_version(Vers)]
                    ;
                    T = []   
          ).

% last good one: std_graphs_string( '10' ).  2016/09/08
% last good one: std_graphs_string( '10.5' ).  2018/03/30

/** std_pig_graphs_strg(+Opts).

String graphs and a map for pig products.

Opts
  * db(Db=strg)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=false)
    whether to debug the fetching of the url
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * links_stem(Ltem='protein.links.v')
    stem for the filename of the remote links file
  * org(Org=pig)
    organism
  * relation(Rel=links)
    relation of STRING we are interested in (bio_db_string_version_base_name/5)
  * string_version(Vers)
    default is collected by visiting the STRING web-page

==
?- std_pig_graphs_str([]).

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
@version  0:1 2023/05/31
*/
std_pig_graphs_strg( Args ) :-
    Self = std_pig_graphs_strg,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    options( string_version(VersionPrv), Opts ),
    ( number(VersionPrv) -> atom_number(Version,VersionPrv); Version = VersionPrv ),
    debuc( Self, 'Version: ~w', Version ),
    bio_db_string_version_base_name( Version, _VersD, RelName, SrcUrl, Opts ),
    debuc( Self, 'Rel file name: ~w', RelName ),
    build_dnload_loc( Self, DnlD, Opts ),
    options( debug_fetch(Fbg), Opts ),
    url_file_local_date_mirror( SrcUrl, DnlD, [debug(Fbg),dnld_file(Bname)|Opts] ),
    working_directory( Here, DnlD ),
    @ gunzip( -k, -f, Bname ),  % keeps .gz file
    EnspPn = strg_suss_edge_ensp,
    file_name_extension( TxtF, gz, Bname ),
    Mess1 = 'Converting string file: ~p, to Prolog',
    debuc( Self, Mess1, [TxtF] ),
    MtxOpts = [ csv_read(separator(0' )),predicate_name(EnspPn),
            rows_transform(maplist(user:de_pig)),header_remove(true) ],
    debuc( Self, 'mtx_prolog options: ~w', [MtxOpts] ),
    mtx_prolog( TxtF, File, MtxOpts ),
    debuc( Self, 'Edges output: ~w', File ),
    delete_file( TxtF ),
    os_make_path( graphs, debug(true) ),
    os_make_path( maps, debug(true) ),
    os_dir_stem_ext( graphs, EnspPn, pl, EnspRelF ),
    @ rm( -f, EnspRelF ),
    @ mv( File, EnspRelF ),
    % info file connect protein to SYmbol
    % directory_file_path( DnlD, InfoBname, LocalInfoFile ),
    % std_graph_string_download_string( LocalInfoFile, InfoFrom, Self ),
    % url_file_local_date_mirror( SrcUrl, DnlD, [debug(Fbg),dnld_file(Bname),iface(wget)|Opts] ),
    bio_db_string_version_base_name( Version, _TheVersD, _VersF, InfoSrcUrl, [relation(info)|Opts] ),
    url_file_local_date_mirror( InfoSrcUrl, DnlD, [dnld_file(InfoBname),dnt(true)|Opts] ),
    @ gunzip( -k, -f, InfoBname ),  % keeps .gz file
    % Map = map_strg_gallus_ensp_symb,
    Map = strg_suss_ensp_symb,
    file_name_extension( InfoTxtF, gz, InfoBname ),
    InfoMess1 = 'Converting map string file: ~p, to Prolog',
    debuc( Self, InfoMess1, [InfoTxtF] ),
    mtx( InfoTxtF, InfoMtx, sep(tab) ),
    InfoMtx = [_|InfoRows],
    maplist( strg_map_row(Map), InfoRows, MapRows ),
    os_dir_stem_ext( MapPlF, [odir(maps),ext(pl),stem(Map)] ),
    bio_db_dnt_times( InfoBname, InfoDnDt, _InfoEndDt ),
    MapInfos = [ source-InfoSrcUrl,datetime-InfoDnDt,header-header('Ensembl Protein ID','Symbol'),
                       data_types-data_types(atom,atom)
               ],
    portray_informed_clauses( MapRows, MapInfos, MapPlF, [] ),
    % mtx( InfoBname, MapRows ),

    debuc( Self, 'wrote, and consulting: ~p', [MapPlF] ),
    Map:consult(MapPlF),

    strg_pig_symbolise_edges( Self, EnspPn, EnspRelF, Map, UnoSymbEdges ),

    sort( UnoSymbEdges, SymbEdges ),
    length( SymbEdges, SymbEdgesLen ),
    debuc( Self, 'unique symbol edges (pig): ~w', [SymbEdgesLen] ),
    EdgeSymbsF = 'graphs/strg_suss_edge_symb.pl',
    bio_db_dnt_times( Bname, DnDt, _EndDt ),
    EdgeSymbsInfos = [ source-From,datetime-DnDt,header-header('Symbol','Symbol',weight),
                       data_types-data_types(atom,atom,integer)
                     ],
    portray_informed_clauses( SymbEdges, EdgeSymbsInfos, EdgeSymbsF, [] ),
    debuc( Self, 'Portrayed onto: ~p', [EdgeSymbsF] ),

    BaseOpts = [ source(From), datetime(DnDt),
                  header(row('Ensembl_Protein','Ensembl_Protein',weight))
                ],
    debuc( Self, 'doing infos for: ~p', [EnspRelF] ),
    bio_db_add_infos_to( BaseOpts, EnspRelF ),
    link_to_bio_sub( strg, EnspRelF, [org(pig),type(graphs)] ),
    link_to_bio_sub( strg, EdgeSymbsF, [org(pig),type(graphs)] ),
    link_to_bio_sub( strg, MapPlF, [org(pig),type(maps)] ),
    delete_file( InfoTxtF ),
    working_directory( _, Here ).

/** old implementation:
strg_pig_symbolise_edges( Self, EnspPn, EnspRelF, Map, UnoSymbEdges ) :-
    debuc( Self, task(start), symbolise(original) ),
    consult( EnspPn:EnspRelF ),
    findall( strg_suss_edge_symb(SymbA,SymbB,W),
                         ( EnspPn:strg_suss_edge_ensp(EnsP1,EnsP2,W),
                           Map:strg_suss_ensp_symb(EnsP1,Symb1),
                           Map:strg_suss_ensp_symb(EnsP2,Symb2),
                           sort_four(Symb1,Symb2,SymbA,SymbB)
                     ),
            UnoSymbEdges
          ),
    debuc( Self, task(stop), symbolise(original) ).
*/

strg_pig_symbolise_edges( Self, EnspPn, EnspRelF, Map, Edges ) :-
     open( EnspRelF, read, InS ),
     read( InS, Term ),
     debuc( Self, task(start), symbolise(streamed) ),
     strg_pig_symbolise_edges_stream( Term, EnspPn, Map, InS, Edges ),
     debuc( Self, task(stop), symbolise(streamed) ),
     close( InS ).

strg_pig_symbolise_edges_stream( end_of_file, _Pn, _Map, _InS, Edges ) :-
     !,
     [] = Edges.
strg_pig_symbolise_edges_stream( InTerm, Pn, Map, InS, Edges ) :-
     ( functor(InTerm,Pn,3) ->
               arg( 1, InTerm, EnsP1 ),
               arg( 2, InTerm, EnsP2 ),
               arg( 3, InTerm, W     ),
               ( (Map:strg_suss_ensp_symb( EnsP1, Symb1 ),
                  Map:strg_suss_ensp_symb( EnsP2, Symb2 )) ->
                         sort_four( Symb1, Symb2, SymbA, SymbB ),
                         [strg_suss_edge_symb(SymbA,SymbB,W)|TEdges] = Edges
                         ;
                         TEdges = Edges
               )
               ;
               throw(rogue_ensp_to_symb_term(InTerm))
     ),
     read( InS, NxtTerm ),
     strg_pig_symbolise_edges_stream( NxtTerm, Pn, Map, InS, TEdges ).

strg_map_row( Pname, InfoRow, Term ) :-
     arg( 1, InfoRow, PfxProt ),
     arg( 2, InfoRow, Symb ),
     atom_concat( '9823.', Prot, PfxProt ),
     Term =.. [Pname,Prot,Symb].

ensp_mouse_symb( EnsP, Symb ) :-   % fixme: make sure the cut is green ! 
    unip:map_unip_mouse_ensp_unip( EnsP, Unip ),
    (   unip:map_unip_mouse_mgim_unip(Mgim,Unip)
        ; 
        mgim:map_mgim_mouse_mgim_unip(Mgim,Unip)
    ),
    mgim:map_mgim_mouse_mgim_symb( Mgim, Symb ),
    !.

sort_four( X, Y, A, B ) :-
    Y @< X,
    !,
    A = Y, B = X.
sort_four( A, B, A, B ).

std_graph_string_download_string( LocalFile, _From, Self ) :-
    exists_file( LocalFile ),
    debuc( Self, 'Using existing local string file: ~p', LocalFile ),
    !.
std_graph_string_download_string( Local, Remote, Self ) :-
    debuc( Self, 'Downloading from: ~p', Remote ),
    url_file( Remote, Local, [dnt(true),iface(wget)] ),
    debuc( Self, '... to local file: ~p', Local ).

de_pig( row(MousEnsP1,MousEnsP2,WAtm), row(EnsP1,EnsP2,W) ) :-
    atom_concat( '9823.', EnsP1, MousEnsP1 ),
    atom_concat( '9823.', EnsP2, MousEnsP2 ),
    ( number(WAtm) -> W = WAtm; atom_number(WAtm,W) ),
    !.
de_pig( Row, _ ) :-
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
