
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

std_chicken_maps_gont_defaults( [ debug(true),
                                  iactive(true)
                                ]
                              ).

/**  std_chicken_maps_gont.

Build maps from gene ontology data.

Opts
  * debug(Dbg=true)
    progress, informational messages
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose

==
?- std_chicken_maps_gont([]).

ορέστης;dnloads/gont% date
Tue 27 Dec 12:04:28 GMT 2022
ορέστης;dnloads/gont% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/gont/maps
ορέστης;dnloads/gont% wc -l *
   99017 maps/map_gont_gallus_symb_gont.pl
   43279 maps/map_gont_gont_gonm.pl
  332010 maps/map_gont_gont_symb.csv
  332017 maps/map_gont_gont_symb.pl
  332017 maps/map_gont_symb_gont.pl
 1138340 total

==

@author nicos angelopoulos
@version  0.1 2022/12/17

*/
std_chicken_maps_gont( Args ) :-
    Self = std_chicken_maps_gont,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    gont_gallus_url( Url ),
    absolute_file_name( bio_db_build_downloads(gont), Loc ),
    os_make_path( Loc ),  % fixme: ensure it complains not...
    debuc( Self, 'build directory: ~p', Loc ),
    working_directory( Old, Loc ),
    ( options(iactive(false),Opts) -> WgVerb=false; WgVerb=true ),
    UrlOpts = [debug(true),interface(wget),file(GzGontF),verb(WgVerb)],
    url_file_local_date_mirror( Url, Loc, UrlOpts ),
    @ gunzip( -k, GzGontF ),
    os_ext( gz, GontF, GzGontF ),
    mtx( GontF, GAs, [skip_heading('!'),sep(tab)] ),
    debuc( Self, dims, gaf/GAs ),
    findall( gont_galg_symb_gont(Symb,Rel,Evid,Gont),
                    ( member(Row,GAs),
                      arg(3,Row,Symb), 
                      Symb \== '',
                      arg(4,Row,Rel),
                      arg(5,Row,GontPrv), at_con([_,GontAtm],':',GontPrv), atom_number(GontAtm,Gont),
                      arg(7,Row,Evid)
                    ),
                        FactsAll ),
    sort( FactsAll, Facts ),
    os_make_path( maps ),
    os_dir_stem_ext( maps, gont_galg_symb_gont, pl, MapF ),
    portray_clauses( Facts, file(MapF) ),
    bio_db_dnt_times( GzGontF, DnDt, _SwDnEn ),
    InfoOpts = [header(row('Symbol','Relation','Evidence','GO_Term')),source(Url),datetime(DnDt)],
    % bio_db_add_infos_to( InfoOpts, 'maps/map_gont_mouse_mgim_gont.pl' ),
    bio_db_add_infos_to( InfoOpts, MapF ),
    link_to_bio_sub( gont, MapF, [org(gallus),type(maps)] ),
    @ rm( -f, GontF ),
    working_directory( _, Old ).
