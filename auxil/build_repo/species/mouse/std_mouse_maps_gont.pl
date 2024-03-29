

:- use_module( library(lists) ).    % member/2.

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
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1). % bio_db_add_infos_to/2.

% gont_mouse_url('http://geneontology.org/gene-associations/gene_association.mgi.gz').
% gont_mouse_url('http://geneontology.org/gene-associations/mgi.gaf.gz').

std_mouse_maps_gont_defaults( Defs ) :-
                                   Defs = [ db(gont),
                                            debug(true),
                                            debug_fetch(true),
                                            debug_url(false),
                                            goa_base(gont_goa),
                                            goa_file('mgi.gaf.gz'),
                                            iactive(true),
                                            org(mouse)
                                   ].

/**  std_mouse_maps_gont.

Build maps from gene ontology data.

Opts
  * db(Db=gont)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * goa_base(GoaB=gont_goa)
    bio_db_source_base_url/2, token or url to download from
  * goa_file(GoaF='mgi.gaf.gz')
    the file name for the download (appended to Ufx@bio_db_source_base_url(gont_goa,Ufx))
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose

==
?- std_mouse_maps_gont([]).

ορέστης;build_repo/mouse% date; pupsh std_mouse_maps_gont.pl ; date 
Tue 27 Dec 16:13:38 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
% Creating dated local basename.
% Using local directory: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/gont'
....
% Symbolic linking, '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/gont/maps/gont_musm_gont_symb.pl', to '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/data/musm/maps/gont/gont_musm_gont_symb.pl'
% Symbolic linking, '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/gont/maps/gont_musm_mgim_gont.pl', to '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/data/musm/maps/gont/gont_musm_mgim_gont.pl'
Tue 27 Dec 16:15:14 GMT 2022

ορέστης;gont/maps% date 
Tue 27 Dec 16:17:19 GMT 2022
ορέστης;gont/maps% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/gont/maps

ορέστης;gont/maps% wc -l *_m*
  432093 gont_musm_gont_symb.pl
  432149 gont_musm_mgim_gont.pl
  864242 total

==

@author nicos angelopoulos
@version  0.1 2018/11/12
@version  0.2 2023/9/29,  options and move Url naming and fetching to helpers

*/
std_mouse_maps_gont( Args ) :-
     Self = std_mouse_maps_gont,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnDir, Opts ),
	working_directory( Old, DnDir ),
     UrlOpts = [debug(true),interface(wget),dnld_file(GzGontF)|Opts],
     SrcRnms = [debug_url-debug,goa_base-url_base,goa_file-url_file], 
     bio_db_source_url( Url, SrcRnms, Opts ),
     url_file_local_date_mirror( Url, DnDir, UrlOpts ),
     @ gunzip( -k, -f, GzGontF ),
     os_ext( gz, GontF, GzGontF ),
     mtx( GontF, GAs, [skip_heading('!'),sep(tab)] ),
     debuc( Self, dims, gas/GAs ),
     findall( gont_musm_mgim_gont(Mgim,Rel,Evid,Gont),
                    ( member(Row,GAs),
                      arg(2,Row,MgimPrv), at_con([_,MgimAtm],':',MgimPrv), atom_number(MgimAtm,Mgim),
                      arg(4,Row,Rel),
                      arg(5,Row,GontPrv), at_con([_,GontAtm],':',GontPrv), atom_number(GontAtm,Gont),
                      arg(7,Row,Evid)
                    ),
                        FactsAll ),
    sort( FactsAll, Facts ),
    os_make_path( maps ),
    os_dir_stem_ext( maps, gont_musm_mgim_gont, pl, MapF ),
    portray_clauses( Facts, file(MapF) ),

	bio_db_dnt_times( GzGontF, DnDt, _SwDnEn ),
	InfoOpts = [header(row('MGI Marker Accession ID','Relation','Evidence','GO_Term')),source(Url),datetime(DnDt)],
	bio_db_add_infos_to( InfoOpts, 'maps/gont_musm_mgim_gont.pl' ),

    ensure_loaded( mgim_tmp:bio_db_build_downloads('mgim/maps/mgim_musm_mgim_symb') ),
    
    findall( row(Gont1,Rel,Evid,Symb1), ( member(gont_musm_mgim_gont(Mgim1,Rel,Evid,Gont1),Facts),
                                 mgim_tmp:mgim_musm_mgim_symb(Mgim1,Symb1)
                               ), GSRowsAll ),
    sort( GSRowsAll, GSRows ),

    GSopts = [predicate_name(gont_musm_gont_symb)],
	mtx_prolog( GSRows, 'maps/gont_musm_gont_symb.pl', GSopts ),
    GsF = 'maps/gont_musm_gont_symb.pl',
    GShdr = header(row('GO_Term','Evidence','MGI Marker Accession ID')),
	bio_db_add_infos_to( [GShdr|GSopts], GsF ),
    % maplist( link_to_map_sub(gont), OutFs ),  % does this work ?
    link_to_bio_sub( gont, [GsF,MapF], [org(mouse),type(maps)]  ),
    @ rm( -f, GontF ),
    working_directory( _, Old ).
