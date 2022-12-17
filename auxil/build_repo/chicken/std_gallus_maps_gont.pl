
:- use_module( library(lists) ).    % member/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knows how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).

% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% local libs & sources
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2.

% gont_gallus_url('http://geneontology.org/gene-associations/mgi.gaf.gz').
gont_gallus_url('http://current.geneontology.org/annotations/goa_chicken.gaf.gz').

std_gallus_maps_gont_defaults(debug(true)).

/**  std_gallus_maps_gont.

Build maps from gene ontology data.

==
?- std_gallus_maps_gont.
==

@author nicos angelopoulos
@version  0.1 2022/12/17

*/
std_gallus_maps_gont( Args ) :-
    Self = std_gallus_maps_gont,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    gont_gallus_url( Url ),
    absolute_file_name( bio_db_build_downloads(gont), Loc ),
    os_make_path( Loc ),  % fixme: ensure it complains not...
    debuc( Self, 'build directory: ~p', Loc ),
    working_directory( Old, Loc ),
    UrlOpts = [debug(true),interface(wget),file(GzGontF)],
    url_file_local_date_mirror( Url, Loc, UrlOpts ),
    @ gunzip( -k, GzGontF ),
    os_ext( gz, GontF, GzGontF ),
    mtx( GontF, GAs, [skip_heading('!'),sep(tab)] ),
    debuc( Self, dims, gaf/GAs ),
    findall( map_gont_gallus_symb_gont(Symb,Rel,Evid,Gont),
                    ( member(Row,GAs),
                      arg(3,Row,Symb), 
                      arg(4,Row,Rel),
                      arg(5,Row,GontPrv), at_con([_,GontAtm],':',GontPrv), atom_number(GontAtm,Gont),
                      arg(7,Row,Evid)
                    ),
                        FactsAll ),
    sort( FactsAll, Facts ),
    os_make_path( maps ),
    os_dir_stem_ext( maps, map_gont_gallus_symb_gont, pl, MapF ),
    portray_clauses( Facts, file(MapF) ),
    bio_db_dnt_times( GzGontF, DnDt, _SwDnEn ),
    InfoOpts = [header(row('Symbol','Relation','Evidence','GO_Term')),source(Url),datetime(DnDt)],
    % bio_db_add_infos_to( InfoOpts, 'maps/map_gont_mouse_mgim_gont.pl' ),
    bio_db_add_infos_to( InfoOpts, MapF ),
    link_to_bio_sub( gont, MapF, [org(mouse),type(maps)] ),
    @ rm( -f, GontF ),
    working_directory( _, Old ).
