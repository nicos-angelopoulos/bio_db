
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(debug_call).
:- lib(stoics_lib:portray_clauses/2).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases' ).  % /1.

% local libs & sources
:- lib(link_to_bio_sub/4).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2

std_graphs_gont_defaults(debug(true)).

% entry point
std_graphs_gont( Args ) :-
    Self = std_graphs_gont,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    % File = '/usr/local/users/nicos/work/bio_db/dnloads/go/new/go-basic.obo',
    % write( 'fixme these are now maps....' ), nl,
    Url = 'http://purl.obolibrary.org/obo/go/go.obo',
    absolute_file_name( bio_db_build_downloads(gont), DnDir ),
    debug( url_local ),
    url_file_local_date_mirror( Url, DnDir, true ),
    debug( Self, 'Dnload done: ~w', true ),
    working_directory( Old, DnDir ), 
    File = 'go.obo',
    Pfx = 'is_a: '/edge_gont_is_a,
    go_obo_collect( File, Pfx, Edges ),
    debug_call( Self, length, edges/Edges ),
    IsaF = 'graphs/edge_gont_is_a.pl',
    sort( Edges, Sdges ),
    bio_db_dnt_times( 'go.obo', DnDt, _DnEnd ),
    portray_clauses( Sdges, file(IsaF) ),
    % record_edges( Edges ).
    link_to_bio_sub( hs, gont, graphs, IsaF ),
    InfoOpts = [header(row('GO Term','GO Term')),source(Url),datetime(DnDt)],
    bio_db_add_infos_to( InfoOpts, IsaF ),

    IncF = 'graphs/edge_gont_includes.pl',
    findall( edge_gont_includes(Pa,Ch), member(edge_gont_is_a(Ch,Pa),Edges), Incs ),
    debug_call( Self, length, incs/Incs ),
    sort( Incs, Sncs ),
    debug( Self, 'Portraying: ~w', IncF ),
    portray_clauses( Sncs, file(IncF) ),
    link_to_bio_sub( hs, gont, graphs, IncF ),
    bio_db_add_infos_to( InfoOpts, IncF ),

    RegPfx = 'relationship: regulates '/edge_gont_regulates,
    go_obo_collect( File, RegPfx, RegEdges ),
    sort( RegEdges, RegSdges ),
    RegF = 'graphs/edge_gont_regulates.pl',
    debug( Self, 'Portraying: ~w', RegF ),
    portray_clauses( RegSdges, file(RegF) ),
    link_to_bio_sub( hs, gont, graphs, RegF ),
    bio_db_add_infos_to( InfoOpts, RegF ),

    PofPfx = 'relationship: part_of '/edge_gont_part_of,
    go_obo_collect( File, PofPfx, PofEdges ),
    sort( PofEdges, PofSdges ),
    PofF = 'graphs/edge_gont_part_of.pl',
    debug( Self, 'Portraying: ~w', PofF ),
    portray_clauses( PofSdges, file(PofF) ),
    link_to_bio_sub( hs, gont, graphs, PofF ),
    bio_db_add_infos_to( InfoOpts, PofF ),

    ConsF = 'graphs/edge_gont_consists_of.pl',
    findall( edge_gont_consists_of(Whole,Part), 
                     member(edge_gont_part_of(Part,Whole),PofEdges)
                , Conss ),
    debug_call( Self, length, consists_of/Conss ),
    sort( Conss, Sonss ),
    debug( Self, 'Portraying: ~w', ConsF ),
    portray_clauses( Sonss, file(ConsF) ),
    link_to_bio_sub( hs, gont, graphs, ConsF ),
    bio_db_add_infos_to( InfoOpts, ConsF ),

    PosRPfx = 'relationship: positively_regulates '/edge_gont_positively_regulates,
    go_obo_collect( File, PosRPfx, PosREdges ),
    sort( PosREdges, PosRSdges ),
    PosRF = 'graphs/edge_gont_positively_regulates.pl',
    debug( Self, 'Portraying: ~w', PosRF ),
    portray_clauses( PosRSdges, file(PosRF) ),
    link_to_bio_sub( hs, gont, graphs, PosRF ),
    bio_db_add_infos_to( InfoOpts, PosRF),

    NegRPfx = 'relationship: negatively_regulates '/edge_gont_negatively_regulates,
    go_obo_collect( File, NegRPfx, NegREdges ),
    sort( NegREdges, NegRSdges ),
    NegRF = 'graphs/edge_gont_negatively_regulates.pl',
    debug( Self, 'Portraying: ~w', NegRF ),
    portray_clauses( NegRSdges, file(NegRF) ),
    link_to_bio_sub( hs, gont, graphs, NegRF ),
    bio_db_add_infos_to( InfoOpts, NegRF),

    working_directory( _, Old ).

% display all the type of relationships defined by a prefix of "^relationship"
% 
go_obo_relationships :-
    % 
    absolute_file_name( bio_db_build_downloads(go), GoDir ),
    directory_file_path( GoDir, 'go.obo', GoF ),
    open( GoF, read, In ),
    read_line_to_codes( In, Cs ),
    go_obo_relationships( Cs, [], In, Rels ),
    write( relationships(Rels) ), nl,
    close( In ).

go_obo_relationships( end_of_file, Rels, _In, Rels ) :- !.
go_obo_relationships( Codes, Seen, In, Rels ) :-
    atom_codes( Atom, Codes ),
    ( atom_concat('relationship: ',Right,Atom) ->
        atomic_list_concat( [Rel|_], ' ', Right ),
        ord_add_element( Seen, Rel, Acc )
        ;
        Acc = Seen
    ),
    read_line_to_codes( In, Next ),
    go_obo_relationships( Next, Acc, In, Rels ).

go_obo_collect( File, Pfx, Edges ) :-
    open( File, read, In ),
    os_make_path( graphs ),
    read_line_to_codes( In, Codes ), 
    go_obo_skip_to_first_term( Codes, In ),
    go_obo_is_a( `[Term]`, Pfx, In, Edges ),
    close( In ).

go_obo_is_a( end_of_file, _Pfx, _In, [] ) :- !.
go_obo_is_a( `[Term]`, Pfx, In, Edges ) :-
    !,
    read_line_to_codes( In, Codes ), 
    atom_string( Line, Codes ),
    atom_concat( 'id: ', GoT, Line ),
    % debug( go_obo, 'Term: ~w', [GoT] ),
    read_line_to_codes( In, Next ), 
    go_obo_is_a_term( Next, GoT, Pfx, In, Edges, Tdges ),
    read_line_to_codes( In, Follows ), 
    go_obo_is_a( Follows, Pfx, In, Tdges ).
go_obo_is_a( `[Typedef]`, _Pfx, _In, [] ).

go_obo_is_a_term( ``, _GoT, _Pfx, _In, Edges, Edges ) :- !.
go_obo_is_a_term( Codes, GoT, Pfx/Tname, In, Edges, Tdges ) :-
    atom_string( Line, Codes ),
    % atom_concat( 'is_a: ', Psfx, Line ),
    atom_concat( Pfx, Psfx, Line ),
    !,
    atomic_list_concat( [Par|_], ' ', Psfx ),
    maplist( go_id_strip, [GoT,Par], [GoTint,Parint] ),
    Edge =.. [Tname,GoTint,Parint],
    Edges = [Edge|Mdges],
    % Edges = [edge_gont_isa(GoT,Par)|Mdges],
    read_line_to_codes( In, Next ), 
    go_obo_is_a_term( Next, GoT, Pfx/Tname, In, Mdges, Tdges ).
go_obo_is_a_term( _Codes, GoT, Pfx, In, Edges, Tdges ) :-
    read_line_to_codes( In, Next ), 
    go_obo_is_a_term( Next, GoT, Pfx, In, Edges, Tdges ).

go_id_strip( GoT, InT ) :-
    atomic_list_concat( ['GO',IntAtm], ':', GoT ),
    atom_number( IntAtm, InT ).

go_obo_skip_to_first_term( `[Term]`, _In ) :-
    !.
go_obo_skip_to_first_term( _, In ) :-
    read_line_to_codes( In, Codes ), 
    go_obo_skip_to_first_term( Codes, In ).
