
:- set_prolog_flag(stack_limit, 10 000 000 000).

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

% load necessary data that has already been generated
% :- ensure_loaded(unip:bio_db_build_downloads('unip/maps/map_unip_mouse_ensp_unip')).
% :- ensure_loaded(unip:bio_db_build_downloads('unip/maps/map_unip_mouse_mgim_unip')).
% :- ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_unip')).
% :- ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_symb')).

% local libs & sources
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).     % bio_db_add_infos_to/2.
:- lib(std_graphs_strg_auto_version/1).

:- debuc(by_unix).
:- debuc(std_graphs_strg). % fixme:

std_mouse_graphs_strg_defaults( [debug(true)|T] ) :-
    ( std_graphs_strg_auto_version(Vers) -> % let options/2 do the erroring
                                            % because user might provide it
        T = [string_version(Vers)]
        ;
        T = []   
    ).

% last good one: std_graphs_string( '10' ).  2016/09/08
% last good one: std_graphs_string( '10.5' ).  2018/03/30
std_mouse_graphs_strg( Args ) :-
    Self = std_mouse_graphs_strg,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    options( string_version(VersionPrv), Opts ),
    % load necessary data that has already been generated
    ensure_loaded(unip:bio_db_build_downloads('unip/maps/map_unip_mouse_ensp_unip')),
    ensure_loaded(unip:bio_db_build_downloads('unip/maps/map_unip_mouse_mgim_unip')),
    ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_unip')),
    ensure_loaded(mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_symb')),
    ( number(VersionPrv) -> atom_number(Version,VersionPrv); Version = VersionPrv ),
    % ensure_loaded( bio_db_build_aliases ),
    debuc( Self, 'Version: ~w', Version ),
    std_graphs_string_version_base_name( Version, Bname, From ),
    debuc( Self, 'Base name: ~w', Bname ),
    absolute_file_name( bio_db_build_downloads(strg), Parent ),
    % absolute_file_name( baio_db_downloads(string/Bname), LocalFile ),
    % directory_file_path( Parent, _BnameAgain, LocalFile ),
    directory_file_path( Parent, Bname, LocalFile ),
    os_make_path( Parent, debug(true) ),
    std_graph_string_download_string( LocalFile, From, Self ),
    working_directory( Here, Parent ),
    @ gunzip( -k, Bname ),  % keeps .gz file
    % @ gunzip( '9606.protein.links.v10.txt.gz' ),
    Edge = edge_strg_mouse,
    file_name_extension( TxtF, gz, Bname ),
    debuc( Self, 'Directory: ~p', [Parent] ),
    Mess1 = 'Converting string file: ~p, to Prolog',
    debuc( Self, Mess1, [TxtF] ),
    Opt = [ csv_read(separator(0' )),predicate_name(Edge),
            rows_transform(maplist(user:de_mouse)),header_remove(true) ],
    mtx_prolog( TxtF, File, Opt ),
    debuc( _, 'Edges output: ~w', File ),
    delete_file( TxtF ),
    % @ rm( -rf, graphs ), don't do that ! there are now multiple downloads from string..
    os_make_path( graphs, debug(true) ),
    Trg = 'graphs/edge_strg_mouse.pl',
    @ rm( -f, Trg ),
    @ mv( File, Trg ),

    consult( edge_strg_mouse:Trg ),
    debuc( _, 'consulted eges from: ~w', [edge_strg_mouse:Trg] ),

    findall( edge_strg_mouse_symb(SymbA,SymbB,W),
                         ( edge_strg_mouse:edge_strg_mouse(EnsP1,EnsP2,W),
                           ensp_mouse_symb(EnsP1,Symb1),
                           ensp_mouse_symb(EnsP2,Symb2),
                           sort(Symb1,Symb2,SymbA,SymbB)
                     ),
            UnoSymbEdges
          ),
    sort( UnoSymbEdges, SymbEdges ),
    length( SymbEdges, SymbEdgesLen ),
    debuc( _, 'unique symbol edges (mouse): ~w', [SymbEdgesLen] ),
    EdgeSymbsF = 'graphs/edge_strg_mouse_symb.pl',
    portray_clauses( SymbEdges, file(EdgeSymbsF) ),
    bio_db_dnt_times( Bname, DnDt, _EndDt ),
    MousOpts = [ source(From), datetime(DnDt),
                  header(row('Ensembl_Protein','Ensembl_Protein',weight))
                ],
    bio_db_add_infos_to( MousOpts, Trg ),

    SymbOpts = [source(From),datetime(DnDt),header(row('MGI_Symbol','MGI_Symbol',weight))],
    bio_db_add_infos_to( SymbOpts, EdgeSymbsF ),

    link_to_bio_sub( strg, Trg, [org(mouse),type(graphs)] ),
    link_to_bio_sub( strg, EdgeSymbsF, [org(mouse),type(graphs)] ),
    working_directory( _, Here ).

ensp_mouse_symb( EnsP, Symb ) :-   % fixme: make sure the cut is green ! 
    unip:map_unip_mouse_ensp_unip( EnsP, Unip ),
    (   unip:map_unip_mouse_mgim_unip(Mgim,Unip)
        ; 
        mgim:map_mgim_mouse_mgim_unip(Mgim,Unip)
    ),
    mgim:map_mgim_mouse_mgim_symb( Mgim, Symb ),
    !.

sort( X, Y, A, B ) :-
    Y @< X,
    !,
    A = Y, B = X.
sort( A, B, A, B ).

std_graph_string_download_string( LocalFile, _From, Self ) :-
    exists_file( LocalFile ),
    debuc( Self, 'Using existing local string file: ~p', LocalFile ),
    !.
std_graph_string_download_string( Local, Remote, Self ) :-
    debuc( Self, 'Downloading from: ~p', Remote ),
    url_file( Remote, Local, dnt(true) ),
    debuc( Self, '... to local file: ~p', Local ).

std_graphs_string_version_base_name( VersionPrv, Bname, Remote ) :-
    ( atom_concat(v,Version,VersionPrv)->true;Version=VersionPrv ),
    atom_concat( v, Version, Vied ),
    % Pfx = 'http://string-db.org/newstring_download/protein.links.v',
    Pfx = 'https://string-db.org/download/protein.links.v',
    atom_concat( Pfx, Version, RemoteDir ),
    atomic_list_concat( [10090,protein,links,Vied,txt,gz], '.', Bname ),
    directory_file_path( RemoteDir, Bname, Remote ).
    % 10/9606.protein.links.v10.txt.gz

de_mouse( row(MousEnsP1,MousEnsP2,WAtm), row(EnsP1,EnsP2,W) ) :-
    atom_concat( '10090.', EnsP1, MousEnsP1 ),
    atom_concat( '10090.', EnsP2, MousEnsP2 ),
    ( number(WAtm) -> W = WAtm; atom_number(WAtm,W) ),
    !.
de_mouse( Row, _ ) :-
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
