
:- set_prolog_flag(stack_limit, 17_179_869_184). % cluster complains

:- use_module(library(lists)).      % member/2.
:- use_module(library(apply)).      % maplist/3.
:- use_module(library(readutil)).   % read_line_to_codes/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(debug_call).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:io_sections/3).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases' ).  % /1.

% local libs & sources
:- lib(go_obo/2).
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2

std_graphs_gont_defaults( [   db(gont),
                              debug(true),
                              debug_fetch(true),
                              debug_url(false),
                              iactive(true),
                              obo_base(gont_obo),
                              obo_file('go.obo'),
                              org(human)
                          ] ).

/** std_graphs_gont(+Opts)

Build data predicates for Gene Ontology graphs.

Opts
  * db(Db=gont)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * obo_base(OboB=gont_obo)
    the url base for the obo download
  * obo_file(OboF='go.obo')
    the file name for the obo download
  * org(Org=human)
    organism

OboF is passed to url_file_local_date_mirror/3 as option =url_file()=, and OboB as url_

@author nicos angelopoulos
@version  0:2 2023/09/22,  Iact and OboF
@see url_file_local_date_mirror/3

*/
% entry point
std_graphs_gont( Args ) :-
    Self = std_graphs_gont,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    build_dnload_loc( Self, DnDir, Opts ),
    RnmOpts = [obo_file-url_file,obo_base-url_base,debug_url-debug],
    bio_db_source_url( Url, RnmOpts, Opts ),
    options( debug_fetch(Fbg), Opts ),
    url_file_local_date_mirror( Url, DnDir, [file(OboF),debug(Fbg)|Opts] ),
    working_directory( Old, DnDir ),
    /* here new code */
    bio_db_dnt_times( OboF, DnDt, _DnEnd ),
    go_obo( OboF, GoObo),
    go_obo_non_obs( GoObo, GoOboCurr ),
    os_make_path( graphs ),
    go_obo_pl_relation( GoOboCurr, is_a, '', gont_homs_edge_gisa, Url, DnDt ),
    go_obo_pl_relation( GoOboCurr, relationship, regulates, gont_homs_edge_greg, Url, DnDt ),
    go_obo_pl_relation( GoOboCurr, relationship, part_of, gont_homs_edge_gpof, Url, DnDt ),
    go_obo_pl_relation( GoOboCurr, relationship, positively_regulates, gont_homs_edge_gprg, Url, DnDt ),
    go_obo_pl_relation( GoOboCurr, relationship, negatively_regulates, gont_homs_edge_gnrg, Url, DnDt ),
    working_directory( _, Old ).

% assumes SubD = graphs
% 
go_obo_pl_relation( GoObo, OboRlt, Pfx, PlRlt, Url, DnDt ) :-
    SubD = graphs,
    GoObo = obo(_,OboTerms),
    go_obo_terms_pl_relation( OboTerms, OboRlt, Pfx, PlRlt, PlTerms ),
    directory_file_path( SubD, PlRlt, Stem ),
    file_name_extension( Stem, pl, PlF ),
    sort( PlTerms, OrdPlTerms ),
    portray_clauses( OrdPlTerms, file(PlF) ),
    link_to_bio_sub( gont, PlF, [org(hs),type(graphs)] ),
    InfoOpts = [header(row('GO Term','GO Term')),source(Url),datetime(DnDt)],
    bio_db_add_infos_to( InfoOpts, PlF ).

/*
go_obo_terms_pl_relation( map_gont_gont_gonm, OboTerms, OboRlt, PlTerms ) :-
    !,
    go_obo_terms_arg_pl_relation( 
go_obo_terms_pl_relation( namespace, OboTerms, OboRlt, PlTerms ) :-
    !,
    */

go_obo_terms_pl_relation( [], _OboRlt, _Pfx, _PlRlt, [] ).
go_obo_terms_pl_relation( [obo_term(Id,_Name,_Nspc,_Obs,Props)|OboTerms], OboRlt, Pfx, PlRlt, PlTerms ) :-
    Term =.. [PlRlt,Id,ObjId],
    findall( Term, (member(OboRlt-Val,Props),go_obo_value_id(Val,Pfx,ObjId)), IdPlTerms ),
    append( IdPlTerms, TPlTerms, PlTerms ),
    go_obo_terms_pl_relation( OboTerms, OboRlt, Pfx, PlRlt, TPlTerms ).
    

go_obo_value_id( Val, Pfx, Id ) :-
    atomic_list_concat( Parts, ' ', Val ),
    ( Pfx == '' ->
        Parts = [GoId|_]
        ;
        Parts = [Pfx,GoId|_]
    ),
    go_id( GoId, Id ).

/*
    /* here old code */
    File = 'go.obo',
    Pfx = 'is_a: '/gont_homs_edge_gisa,
    go_obo_collect( File, Pfx, Edges ),
    debuc( Self, length, edges/Edges ),
    IsaF = 'graphs/gont_homs_edge_gisa.pl',
    sort( Edges, Sdges ),
    bio_db_dnt_times( 'go.obo', DnDt, _DnEnd ),
    portray_clauses( Sdges, file(IsaF) ),
    % record_edges( Edges ).
    % link_to_bio_sub( hs, gont, graphs, IsaF, [org(hs),type(graphs)] ),
    link_to_bio_sub( gont, IsaF, [org(hs),type(graphs)] ),
    InfoOpts = [header(row('GO Term','GO Term')),source(Url),datetime(DnDt)],
    bio_db_add_infos_to( InfoOpts, IsaF ),

    % IncF = 'graphs/edge_gont_includes.pl',
    % findall( edge_gont_includes(Pa,Ch), member(edge_gont_is_a(Ch,Pa),Edges), Incs ),
    % debuc( Self, length, incs/Incs ),
    % sort( Incs, Sncs ),
    % debuc( Self, 'Portraying: ~w', IncF ),
    % portray_clauses( Sncs, file(IncF) ),
    % link_to_bio_sub( gont, IncF, [org(hs),type(graphs)] ),
    % bio_db_add_infos_to( InfoOpts, IncF ),

    RegPfx = 'relationship: regulates '/gont_homs_edge_greg,
    go_obo_collect( File, RegPfx, RegEdges ),
    sort( RegEdges, RegSdges ),
    RegF = 'graphs/gont_homs_edge_greg.pl',
    debuc( Self, 'Portraying: ~w', RegF ),
    portray_clauses( RegSdges, file(RegF) ),
    link_to_bio_sub( gont, RegF, [org(hs),type(graphs)] ),
    bio_db_add_infos_to( InfoOpts, RegF ),

    PofPfx = 'relationship: part_of '/gont_homs_edge_gpof,
    go_obo_collect( File, PofPfx, PofEdges ),
    sort( PofEdges, PofSdges ),
    PofF = 'graphs/gont_homs_edge_gpof.pl',
    debuc( Self, 'Portraying: ~w', PofF ),
    portray_clauses( PofSdges, file(PofF) ),
    link_to_bio_sub( gont, PofF, [org(hs),type(graphs)] ),
    bio_db_add_infos_to( InfoOpts, PofF ),

    % ConsF = 'graphs/edge_gont_consists_of.pl',
    % findall( edge_gont_consists_of(Whole,Part), 
                     % member(edge_gont_part_of(Part,Whole),PofEdges)
                % , Conss ),
    % debuc( Self, length, consists_of/Conss ),
    % sort( Conss, Sonss ),
    % debuc( Self, 'Portraying: ~w', ConsF ),
    % portray_clauses( Sonss, file(ConsF) ),
    % link_to_bio_sub( gont, ConsF, [org(hs),type(graphs)] ),
    % bio_db_add_infos_to( InfoOpts, ConsF ),

    PosRPfx = 'relationship: positively_regulates '/edge_gont_positively_regulates,
    go_obo_collect( File, PosRPfx, PosREdges ),
    sort( PosREdges, PosRSdges ),
    PosRF = 'graphs/edge_gont_positively_regulates.pl',
    debuc( Self, 'Portraying: ~w', PosRF ),
    portray_clauses( PosRSdges, file(PosRF) ),
    link_to_bio_sub( gont, PosRF, [org(hs),type(graphs)] ),
    bio_db_add_infos_to( InfoOpts, PosRF),

    NegRPfx = 'relationship: negatively_regulates '/edge_gont_negatively_regulates,
    go_obo_collect( File, NegRPfx, NegREdges ),
    sort( NegREdges, NegRSdges ),
    NegRF = 'graphs/edge_gont_negatively_regulates.pl',
    debuc( Self, 'Portraying: ~w', NegRF ),
    portray_clauses( NegRSdges, file(NegRF) ),
    link_to_bio_sub( gont, NegRF, [org(hs),type(graphs)] ),
    bio_db_add_infos_to( InfoOpts, NegRF),

    working_directory( _, Old ).
*/

/*
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
*/


/*
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
% untested -> obsolete
go_obo_is_a_term( Codes, _GoT, _PfxTname, _In, Edges, Tdges ) :-
    atom_string( Line, Codes ),
    atom_concat( 'name: obsolete', _Psfx, Line ),
    % skip obsolete entries for everything 
    % formally you can ask for "^is_obsolete: true"
    % but name comes right after id: so it is easier to do this way
    !,
    Edges = Tdges.

go_obo_is_a_term( Codes, GoT, Pfx/Tname, In, Edges, Tdges ) :-
    atom_string( Line, Codes ),
    % atom_concat( 'is_a: ', Psfx, Line ),
    atom_concat( Pfx, Psfx, Line ),
    !,
    atomic_list_concat( [Par|_], ' ', Psfx ),
    maplist( go_id, [GoT,Par], [GoTint,Parint] ),
    Edge =.. [Tname,GoTint,Parint],
    Edges = [Edge|Mdges],
    % Edges = [edge_gont_isa(GoT,Par)|Mdges],
    read_line_to_codes( In, Next ), 
    go_obo_is_a_term( Next, GoT, Pfx/Tname, In, Mdges, Tdges ).
go_obo_is_a_term( _Codes, GoT, Pfx, In, Edges, Tdges ) :-
    read_line_to_codes( In, Next ), 
    go_obo_is_a_term( Next, GoT, Pfx, In, Edges, Tdges ).

go_obo_skip_to_first_term( `[Term]`, _In ) :-
    !.
go_obo_skip_to_first_term( _, In ) :-
    read_line_to_codes( In, Codes ), 
    go_obo_skip_to_first_term( Codes, In ).
*/

