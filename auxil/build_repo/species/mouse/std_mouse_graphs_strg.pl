
:- set_prolog_flag(stack_limit, 12 000 000 000).
% :- set_prolog_flag(stack_limit, 18 000 000 000).  % try this

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
:- lib(stoics_lib:url_file/3).
:- lib(stoics_lib:message_report/3).
:- lib(stoics_lib:portray_clauses/2).
% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(unip:bio_db_build_downloads('unip/maps/map_unip_mouse_ensp_unip')).
% :- ensure_loaded(unip:bio_db_build_downloads('unip/maps/map_unip_mouse_mgim_unip')).
% :- ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_unip')).
% :- ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_symb')).

% local libs & sources
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).                  % bio_db_add_infos_to/2.
:- lib(build_dnload_loc/3).
:- lib(portray_informed_clauses/4).
:- lib(url_file_local_date_mirror/3).
:- lib(std_graphs_strg_auto_version/1).
:- lib(bio_db_string_version_base_name/5).   % uses bio_db_source_url/3

:- debuc(by_unix).
:- debuc(std_graphs_strg). % fixme:

std_mouse_graphs_strg_defaults( Args, Defs ) :-
               Defs = [  db(strg),
                         debug(true),
                         debug_fetch(true),
                         debug_url(false),
                         iactive(true),
                         org(mouse),
                         relation(links)
                         | T
                      ],
    ( std_graphs_strg_auto_version(Vers,Args) ->       % let options/2 do the erroring
                                                       % because user might provide it
        T = [string_version(Vers)]
        ;
        T = []   
    ).

% last good one: std_graphs_string( '10' ).  2016/09/08
% last good one: std_graphs_string( '10.5' ).  2018/03/30

/** std_mouse_graphs_strg( Opts ).

Mouse graphs for STRING protein protein interactions.

Depends on std_maps_mgim std_maps_unip.

Opts
  * db(Db=strg)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=false)
    whether to debug the fetching of the url
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * links_stem(Ltem='protein.links.v')
    stem for the filename of the remote links file
  * org(Org=mouse)
    organism
  * relation(Rel=links)
    relation of STRING we are interested in (bio_db_string_version_base_name/5)
  * string_version(Vers)
    default is collected by visiting the STRING web-page

==
?- std_mouse_graphs_strg([]).
==

@author nicos angelopoulos
@version  0:2 2022/12/27,  
@see https://string-db.org
@tbd this is closely related to the human pred, we should factor the common things out

*/
std_mouse_graphs_strg( Args ) :-
    Self = std_mouse_graphs_strg,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    options( string_version(VersionPrv), Opts ),
    % load necessary data that has already been generated
    ensure_loaded(unip:bio_db_build_downloads('unip/maps/unip_musm_ensp_unip')),
    ensure_loaded(unip:bio_db_build_downloads('unip/maps/unip_musm_mgim_unip')),
    ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/mgim_musm_mgim_unip')),
    ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/mgim_musm_mgim_symb')),
    ( number(VersionPrv) -> atom_number(Version,VersionPrv); Version = VersionPrv ),
    % ensure_loaded( bio_db_build_aliases ),
    debuc( Self, 'Version: ~w', Version ),
    % std_graphs_string_version_base_name( Version, Bname, From ),
    bio_db_string_version_base_name( Version, _VersD, RemBname, SrcUrl, Opts ),
    debuc( Self, 'Remote base name: ~w', RemBname ),
    % absolute_file_name( bio_db_build_downloads(strg), Parent ),
    % os_path( Parent, VersD, DnlD ),
    % os_make_path( DnlD, debug(true) ),
    build_dnload_loc( Self, DnlD, Opts ),
    debuc( Self, 'Downloading from: ~p', SrcUrl ),
    url_file_local_date_mirror( SrcUrl, DnlD, [dnld_file(Bname),iface(wget)|Opts] ),
    debuc( Self, 'Basename to work on: ~p', [Bname] ),
    working_directory( Here, DnlD ),
    @ gunzip( -k, -f, Bname ),  % keeps .gz file
    % @ gunzip( '9606.protein.links.v10.txt.gz' ),
    % Edge = edge_strg_mouse,
    EnspPn = strg_musm_edge_ensp,
    file_name_extension( TxtF, gz, Bname ),
    debuc( Self, 'Directory: ~p', [DnlD] ),
    Mess1 = 'Converting string file: ~p, to Prolog',
    debuc( Self, Mess1, [TxtF] ),
    MtxOpts = [ csv_read(separator(0' )), predicate_name(EnspPn),
                rows_transform(maplist(user:de_mouse)), header_remove(true) 
              ],
    mtx_prolog( TxtF, File, MtxOpts ),
    debuc( Self, 'Wrote on file: ~p', [File] ),
    delete_file( TxtF ),
    % @ rm( -rf, graphs ), don't do that ! there are now multiple downloads from string..
    os_make_path( graphs, debug(true) ),
    % Trg = 'graphs/edge_strg_mouse.pl',
    os_dir_stem_ext( graphs, EnspPn, pl, EnspRelF ),
    @ rm( -f, EnspRelF ),
    @ mv( File, EnspRelF ),
    mouse_strg_symbolise_edges( Self, EnspPn, EnspRelF, UnoSymbEdges ),
    sort( UnoSymbEdges, SymbEdges ),
    length( SymbEdges, SymbEdgesLen ),
    debuc( Self, 'unique symbol edges (mouse): ~w', [SymbEdgesLen] ),
    EdgeSymbsF = 'graphs/strg_musm_edge_symb.pl',
    bio_db_dnt_times( Bname, DnDt, _EndDt ),
    EdgeSymbsInfos = [ source-SrcUrl, datetime-DnDt, header-header('Symbol','Symbol',weight),
                       data_types-data_types(atom,atom,integer)
                     ],
    portray_informed_clauses( SymbEdges, EdgeSymbsInfos, EdgeSymbsF, [] ),
    % SymbOpts = [source(From),datetime(DnDt),header(row('MGI_Symbol','MGI_Symbol',weight))],
    % bio_db_add_infos_to( SymbOpts, EdgeSymbsF ),
    debuc( Self, wrote, EdgeSymbsF ),
    MousOpts = [ source(SrcUrl), datetime(DnDt),
                  header(row('Ensembl_Protein','Ensembl_Protein',weight))
                ],
    debuc( Self, task(stop), infosise(copy_stream(EnspRelF )) ),
    bio_db_add_infos_to( MousOpts, EnspRelF ),
    debuc( Self, task(stop), infosise(copy_stream) ),
    link_to_bio_sub( strg, [EnspRelF,EdgeSymbsF], [org(mouse),type(graphs)] ),
    working_directory( _, Here ).

% At 13:34:57 on 10th of Jun 2023 starting task: symbolise(streamed).
% At 13:37:29 on 10th of Jun 2023 stop task: symbolise(streamed).
% 
mouse_strg_symbolise_edges( Self, EnspPn, EnspRelF, Edges ) :-
     open( EnspRelF, read, InS ),
     read( InS, Term ),
     debuc( Self, task(start), symbolise(streamed) ),
     mouse_strg_symbolise_edges_stream( Term, EnspPn, InS, Edges ),
     debuc( Self, task(stop), symbolise(streamed) ),
     close( InS ).

mouse_strg_symbolise_edges_stream( end_of_file, _Pn, _InS, Edges ) :-
     !,
     [] = Edges.
mouse_strg_symbolise_edges_stream( InTerm, Pn, InS, Edges ) :-
     ( functor(InTerm,Pn,3) ->
               arg( 1, InTerm, EnsP1 ),
               arg( 2, InTerm, EnsP2 ),
               arg( 3, InTerm, W     ),
               ( (ensp_mouse_symb( EnsP1, Symb1 ),
                  ensp_mouse_symb( EnsP2, Symb2 )) ->
                         sort_four( Symb1, Symb2, SymbA, SymbB ),
                         [strg_musm_edge_symb(SymbA,SymbB,W)|TEdges] = Edges
                         ;
                         TEdges = Edges
               )
               ;
               throw(rogue_ensp_to_symb_term(InTerm))
     ),
     read( InS, NxtTerm ),
     mouse_strg_symbolise_edges_stream( NxtTerm, Pn, InS, TEdges ).


ensp_mouse_symb( EnsP, Symb ) :-   % fixme: make sure the cut is green ! 
    unip:unip_musm_ensp_unip( EnsP, Unip ),
    (   unip:unip_musm_mgim_unip(Mgim,Unip)
        ; 
        mgim:mgim_musm_mgim_unip(Mgim,Unip)
    ),
    mgim:mgim_musm_mgim_symb( Mgim, Symb ),
    !.

sort_four( X, Y, A, B ) :-
    Y @< X,
    !,
    A = Y, B = X.
sort_four( A, B, A, B ).

de_mouse( row(MousEnsP1,MousEnsP2,WAtm), row(EnsP1,EnsP2,W) ) :-
    atom_concat( '10090.', EnsP1, MousEnsP1 ),
    atom_concat( '10090.', EnsP2, MousEnsP2 ),
    ( number(WAtm) -> W = WAtm; atom_number(WAtm,W) ),
    !.
de_mouse( Row, _ ) :-
    debuc( _, 'Failed to translate row: ~w', Row ),
    abort.

