
:- set_prolog_flag(stack_limit, 80 000 000 000).

:- use_module(library(csv)).    % csv_read_file/3.
:- use_module(library(apply)).  % maplist/2.
:- use_module(library(lists)).  % member/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(stoics_lib:map_succ_list/3).


% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% load necessary data that has already been generated
% :- ensure_loaded( hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc') ).

% local libs & sources
:- lib(go_obo/2).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).  % bio_db_add_infos_to/2
:- lib(go_id/2).

% :- debuc(std_maps_go).

std_maps_gont_defaults( Defs ) :-
                                   Defs = [ debug(true),
                                            debug_url(false),
                                            goa_base(gont_goa),
                                            goa_file('goa_human.gaf.gz'),
                                            obo_base('gont_obo'),
                                            obo_file('go.obo'),
                                            iactive(true)
                                          ].

% std_maps_gont( Opts ).
% 
% Set up some standard maps for gene ontology (GO, gont) data.
%
% Source:  http://geneontology.org/gene-associations/gene_association.goa_human.gz
% 
% Currrently this sets ups 
%  * map_gont_gont_symb( GOTERM, Symb ).
%  * map_gont_symb_gont( Symb, GOTERM ).
%
%   TermGz = 'go_daily-termdb-tables.tar.gz',
%  * map_gont_gont_gonm( GOTERM, GONM ).
%
% Opts
%  * debug(Dbg=true)
%    informational, progress messages
%  * debug_url(Ubg=false)
%    whether to debug the concatenation of the url (via bio_db_source_url/2)
%  * goa_base(GoaB=gont_goa)
%    bio_db_source_base_url/2, token or url to download from
%  * goa_file(GoaF='goa_human.gaf.gz')
%    the file name for the download (appended to Ufx@bio_db_source_base_url(gont_goa,Ufx))
%  * iactive(Iact=true)
%    whether the session is interactive, otherwise wget gets --no-verbose
%  * obo_base(OboB=gont_obo)
%    the url base for the obo download
%  * obo_file(OboF='go.obo')
%    the file name for the obo download
%
% @author  nicos angelopoulos
% @version 0.1 2015/3/26
% @version 0.2 2023/9/22, moved download location to options
% @see bio_db_source_base_url/2, bio_db_source_url/2.
%
std_maps_gont( Args ) :-
    Self = std_maps_gont,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    % DnDir = '/usr/local/users/nicos/work/db/data/go',
    % load necessary data that has already been generated
    ensure_loaded( hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_symb_hgnc') ),
    absolute_file_name( bio_db_build_downloads(gont), DnDir ),
    os_make_path( DnDir, debug(true) ),
    working_directory( Here, DnDir ),
    options( [goa_base(GoaB),goa_file(GoaF),debug_url(Ubg)], Opts ),
    Upts = [url_base(GoaB),url_file(GoaF),debug(Ubg)],
    bio_db_source_url( Url, Upts ),
    ( options(iactive(false),Opts) -> WgVerb=false; WgVerb=true ),
    url_file_local_date_mirror( Url, DnDir, verb(WgVerb) ),

    @ gunzip( --force, -k, GoaF ),
    file_name_extension( GoaHs, gz, GoaF ),
    % mtx( 'gene_association.goa_human.tsv', Mtx ),
    % mtx( GoaHs, Mtx, csv_read(sep=0'\t) ),
    csv_read_file( GoaHs, MtxPrv, [separator(0'\t),match_arity(false),convert(false)] ),
    clense_goa_hs( MtxPrv, Mtx ),

    debuc( Self, 'loaded data...', true ),
    make_directory_path( maps ),
    gaf_gont_symb( Mtx, Self, NonSymbs, MultSymbs, NewRows ),
    % fixme: make the messages more informative
    debuc( Self, length, [non_symboled,multi_symboled]/[NonSymbs,MultSymbs] ),

    sort( NewRows, OrdRows ),
    mtx( 'maps/gont_homs_gont_symb.csv', OrdRows ),
    GSopts = [predicate_name(gont_homs_gont_symb)],
    mtx_prolog( OrdRows, 'maps/gont_homs_gont_symb.pl', GSopts ),
    working_directory( AtHere, AtHere ),
    debuc( Self, 'Currently at: ~p', AtHere ),
    % bio_db_dnt_times( 'gene_association.goa_human.gz', UrlDntSt, _DntEnd ),
    bio_db_dnt_times( GoaF, UrlDntSt, _DntEnd ),
    AddOpts = [source(Url),datetime(UrlDntSt)],
    bio_db_add_infos_to( [header(row('GO Term','Evidence','HGNC Symbol'))|AddOpts], 'maps/gont_homs_gont_symb.pl' ),
    
    findall( row(Symb,Evid,Gont), member(row(Gont,Evid,Symb),OrdRows), SGRows ),
    sort( SGRows, OrdSGRows ),
    SGopts = [predicate_name(gont_homs_symb_gont)],
    mtx_prolog( OrdSGRows, 'maps/gont_homs_symb_gont.pl', SGopts ),
    bio_db_add_infos_to( [header(row('HGNC Symbol','Evidence','GO Term'))|AddOpts], 'maps/gont_homs_symb_gont.pl' ),
    
    debuc( Self, 'Building term to name map', true ),

    options( [obo_base(OboB),obo_file(OboF),debug_url(Ubg)], Opts ),
    Upts = [url_file(OboF),url_base(OboB),debug(Ubg)],
    bio_db_source_url( OboUrl, Upts ),
    absolute_file_name( bio_db_build_downloads(gont), DnDir ),
    url_file_local_date_mirror( OboUrl, DnDir, [debug(true),verb(WgVerb)] ),
    debuc( Self, 'Dnload done: ~w', [DnDir] ),
    go_obo( OboF, GoObo),
    go_obo_non_obs( GoObo, GoOboCurr ),
    GoOboCurr = obo(_,OboTerms),
    findall( gont_homs_gont_gonm(Gont,Gonm), member(obo_term(Gont,Gonm,_Nspc,_Obs,_Props),OboTerms), GontGonms ),
    sort( GontGonms, OrdGontGonms ),
    bio_db_dnt_times( 'go.obo', DnDt, _DnEnd ),
    portray_clauses( OrdGontGonms, file('maps/gont_homs_gont_gonm.pl') ),
    InfoOpts = [header(row('GO Term','GO Name')),source(OboUrl),datetime(DnDt)],
    bio_db_add_infos_to( InfoOpts, 'maps/gont_homs_gont_gonm.pl' ),
    
    working_directory( First, maps ),
    OutFs = ['gont_homs_gont_symb.pl','gont_homs_symb_gont.pl','gont_homs_gont_gonm.pl'],
    maplist( link_to_bio_sub(gont), OutFs ),

    working_directory( _, First ),
    delete_file( GoaHs ),
    % delete_file( TermTar ),
    working_directory( _, Here ).

% fixme: parse Go through grammar
gaf_gont_symb( [], _Self, [], [], [] ).
gaf_gont_symb( [Row|Rows], Self, NonSymbs, MultSymbs, NewRows ) :-
    arg( 5, Row, GoTermFull ),
    % go_term( GoTermFull, GoTerm ),
    go_id( GoTermFull, GoTerm ),
    arg( 11, Row, Bared ),
    go_bared_symbol( Bared, Symb, Self, NonSymbs, MultSymbs, TNonSymbs, TMultSymbs ),
    arg( 7, Row, Evid ),
    ( Symb == [] -> NewRows = TNewRows; NewRows= [row(GoTerm,Evid,Symb)|TNewRows] ),
    gaf_gont_symb( Rows, Self, TNonSymbs, TMultSymbs, TNewRows ).

go_term( GoTermFull, GoTerm ) :-
    (atom_concat('GO:',GoTermAtom,GoTermFull) -> 
        atom_number(GoTermAtom,GoTerm) 
        ; 
        throw(no_go(GoTermFull))
    ).

go_bared_symbol( Bared, Symb, Self, NonSymbs, MultSymbs, TNonSymbs, TMultSymbs ) :-
    atomic_list_concat( Parts, '|', Bared ),
    map_succ_list( user:hgnc_symb, Parts, NestSymbs ),
    flatten( NestSymbs, Symbs ),
    go_bared_symbol_single( Symbs, Bared, Symb, Self, NonSymbs, MultSymbs, TNonSymbs, TMultSymbs ).

hgnc_symb( Symb, Symb ) :-
    hgnc:hgnc_homs_symb_hgnc( Symb, _ ),
    !.
hgnc_symb( Part, Symbs ) :-
    atomic_list_concat( Subs, ':', Part ),
    Subs \= [Part],
    map_succ_list( hgnc_symb, Subs, Symbs ).

% This is a generic pattern...
go_bared_symbol_single( [], Bared, Symb, _Self, NonSymbs, MultSymbs, TNonSymbs, TMultSymbs ) :-
    debuc( std_maps_gont(details), 'GO bared:~w did not lead to a symbol...', Bared ),
    Symb = [],
    NonSymbs = [Bared|TNonSymbs],
    TMultSymbs = MultSymbs.
go_bared_symbol_single( [Symb], _Bared, Symb, _Self, NonSymbs, MultSymbs, TNonSymbs, TMultSymbs ) :-
    !,
    NonSymbs = TNonSymbs,
    MultSymbs = TMultSymbs.
go_bared_symbol_single( [S1,S2|Sail], Bared, Symb, _Self, NonSymbs, MultSymbs, TNonSymbs, TMultSymbs ) :-
    debuc( std_maps_gont(details), 'GO bared:~w did led to multiple symbols...,~w', [Bared,[S1,S2|Sail]] ),
    memberchk( Symb, [S1,S2|Sail] ),
    NonSymbs = TNonSymbs,
    MultSymbs = [Bared-[S1,S2|Sail]|TMultSymbs].

clense_goa_hs( [H|T], Mtx ) :-
    functor( H, _, 1 ),
    !,
    clense_goa_hs( T, Mtx ).
clense_goa_hs( Mtx, Mtx ).
