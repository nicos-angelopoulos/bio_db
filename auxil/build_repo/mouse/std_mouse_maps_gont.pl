

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).

% also sets lib alias to that dir
:- ensure_loaded( '../../lib/bio_db_build_aliases' ).  % /1.

% local libs & sources
:- lib(link_to_bio_sub/4).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2.

% gont_mouse_url('http://geneontology.org/gene-associations/gene_association.mgi.gz').
gont_mouse_url('http://geneontology.org/gene-associations/mgi.gaf.gz').

std_mouse_maps_gont_defaults([]).

/**  std_mouse_maps_gont.

Build maps from gene ontology data.

==
?-
==

@author nicos angelopoulos
@version  0.1 2018/11/12

*/
std_mouse_maps_gont( Args ) :-
    Self = std_mouse_maps_gont,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    gont_mouse_url( Url ),
    absolute_file_name( bio_db_build_downloads(gont), Loc ),
	os_make_path( Loc ),  % fixme: ensure it complains not...
	debug( Self, 'build directory: ~p', Loc ),
	working_directory( Old, Loc ),
	UrlOpts = [debug(url_local),interface(wget),file(GzGontF)],
    url_file_local_date_mirror( Url, Loc, UrlOpts ),
    @ gunzip( -k, GzGontF ),
    os_ext( gz, GontF, GzGontF ),
    mtx( GontF, GAs, [skip_heading('!'),sep(tab)] ),
    debug_call( Self, dims, gas/GAs ),
    findall( map_gont_mouse_mgim_gont(Mgim,Evid,Gont),
                    ( member(Row,GAs),
                      arg(2,Row,MgimPrv), at_con([_,MgimAtm],':',MgimPrv), atom_number(MgimAtm,Mgim),
                      arg(5,Row,GontPrv), at_con([_,GontAtm],':',GontPrv), atom_number(GontAtm,Gont),
                      arg(7,Row,Evid)
                    ),
                        Facts ),
    os_make_path( maps ),
    os_dir_stem_ext( maps, map_gont_mouse_mgim_gont, pl, MapF ),
    portray_clauses( Facts, file(MapF) ),

	bio_db_dnt_times( GzGontF, DnDt, _SwDnEn ),
	InfoOpts = [header(row('MGI Marker Accession ID','Evidence','GO_Term')),source(Url),datetime(DnDt)],
	bio_db_add_infos_to( InfoOpts, 'maps/map_gont_mouse_mgim_gont.pl' ),

    ensure_loaded( mgim:bio_db_build_downloads('mgim/maps/map_mgim_mouse_mgim_symb') ),
    
    findall( row(Gont1,Symb1), ( member(map_gont_mouse_mgim_gont(Mgim1,_,Gont1),Facts),
                                 map_mgim_mouse_mgim_symb(Mgim1,Symb1)
                               ), GSRowsAll ),
    sort( GSRowsAll, GSRows ),

    GSopts = [predicate_name(map_gont_mouse_gont_symb)],
	mtx_prolog( GSRows, 'maps/map_gont_mouse_gont_symb.pl', GSopts ),
    GsF = 'maps/map_gont_mouse_gont_symb.pl',
    GShdr = header(row('GO_Term','MGI Marker Accession ID')),
	bio_db_add_infos_to( [GShdr|GSopts], GsF ),
    % maplist( link_to_map_sub(gont), OutFs ),  % does this work ?
    link_to_bio_sub( mouse, gont, maps, GsF  ),
    link_to_bio_sub( mouse, gont, maps, MapF ),
    @ rm( -f, GontF ),
    working_directory( _, Old ).
