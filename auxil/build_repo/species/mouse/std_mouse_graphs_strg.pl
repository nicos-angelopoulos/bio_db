
:- set_prolog_flag(stack_limit, 12 000 000 000).

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
:- lib(portray_informed_clauses/4).

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

/** std_mouse_graphs_strg( Opts ).

Mouse graphs for STRING protein protein interactions.

Depends on std_maps_mgim std_maps_unip.

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
    % Edge = edge_strg_mouse,
    EnspPn = strg_musm_edge_ensp,
    file_name_extension( TxtF, gz, Bname ),
    debuc( Self, 'Directory: ~p', [Parent] ),
    Mess1 = 'Converting string file: ~p, to Prolog',
    debuc( Self, Mess1, [TxtF] ),
    Opt = [ csv_read(separator(0' )),predicate_name(EnspPn),
            rows_transform(maplist(user:de_mouse)),header_remove(true) ],
    mtx_prolog( TxtF, File, Opt ),
    debuc( Self, 'Wrote on file: ~p', [File] ),
    delete_file( TxtF ),
    % @ rm( -rf, graphs ), don't do that ! there are now multiple downloads from string..
    os_make_path( graphs, debug(true) ),

    % Trg = 'graphs/edge_strg_mouse.pl',
    os_dir_stem_ext( graphs, EnspPn, pl, EnspRelF ),
    @ rm( -f, EnspRelF ),
    @ mv( File, EnspRelF ),

    consult( EnspPn:EnspRelF ),
    debuc( Self, 'consulted eges from: ~w', [EnspPn:EnspRelF] ),

    EnspGoal =.. [EnspPn,EnsP1,EnsP2,W],
    findall( strg_musm_edge_symb(SymbA,SymbB,W),
                         ( EnspPn:EnspGoal,
                           ensp_mouse_symb(EnsP1,Symb1),
                           ensp_mouse_symb(EnsP2,Symb2),
                           sort_four(Symb1,Symb2,SymbA,SymbB)
                     ),
            UnoSymbEdges
          ),
    sort( UnoSymbEdges, SymbEdges ),
    length( SymbEdges, SymbEdgesLen ),
    debuc( Self, 'unique symbol edges (mouse): ~w', [SymbEdgesLen] ),
    EdgeSymbsF = 'graphs/strg_musm_edge_symb.pl',

    bio_db_dnt_times( Bname, DnDt, _EndDt ),
    EdgeSymbsInfos = [ source-From, datetime-DnDt, header-header('Symbol','Symbol',weight),
                       data_types-data_types(atom,atom,integer)
                     ],
    portray_informed_clauses( SymbEdges, EdgeSymbsInfos, EdgeSymbsF, [] ),
    % SymbOpts = [source(From),datetime(DnDt),header(row('MGI_Symbol','MGI_Symbol',weight))],
    % bio_db_add_infos_to( SymbOpts, EdgeSymbsF ),
    debuc( Self, wrote, EdgeSymbsF ),

    MousOpts = [ source(From), datetime(DnDt),
                  header(row('Ensembl_Protein','Ensembl_Protein',weight))
                ],
    bio_db_add_infos_to( MousOpts, EnspRelF ),

    link_to_bio_sub( strg, EnspRelF, [org(mouse),type(graphs)] ),
    link_to_bio_sub( strg, EdgeSymbsF, [org(mouse),type(graphs)] ),
    working_directory( _, Here ).

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

std_graph_string_download_string( LocalFile, _From, Self ) :-
    exists_file( LocalFile ),
    debuc( Self, 'Using existing local string file: ~p', LocalFile ),
    !.
std_graph_string_download_string( Local, Remote, Self ) :-
    debuc( Self, 'Downloading from: ~p', Remote ),
    url_file( Remote, Local, [dnt(true),iface(wget)] ),
    debuc( Self, '... to local file: ~p', Local ).

std_graphs_string_version_base_name( VersionPrv, Bname, Remote ) :-
    ( atom_concat(v,Version,VersionPrv)->true;Version=VersionPrv ),
    atom_concat( v, Version, Vied ),
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

