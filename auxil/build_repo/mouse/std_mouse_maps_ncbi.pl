
% from human:   :- set_prolog_flag(stack_limit, 10 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
% :- lib(stoics_lib:prefix_atom/2).


% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/4).
:- lib(url_file_local_date_mirror/3).

% load necessary data that has already been generated
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')).

ncbi_mouse_gene_info_url( 'ftp://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/Mammalia/Mus_musculus.gene_info.gz' ).
ncbi_dnload( Loc ) :-
	absolute_file_name( bio_db_build_downloads(ncbi), Loc ),
	os_make_path( Loc, debug(true) ).

std_mouse_maps_ncbi_defaults([]).

/** std_mouse_maps_ncb(Args) :-
    
    Create a map of synonms to symbols according to NCBI.

    map_ncbi_mouse_syno_symb( Syno, Symb ).

@author nicos angelopoulos
@version  0.1 2019/02/12
@tbd double check symbols, are MGI known symbols....

*/
std_mouse_maps_ncbi( Args ) :-
    Self = std_mouse_maps_ncbi,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
	% Dir = '/usr/local/users/nicos/work/db/data/ncbi',
	ncbi_dnload( DnDir ),
	ncbi_mouse_gene_info_url( Url ),

    UrlOpts = [debug(true),interface(wget),file(GzF),dnt_stamp(DntStamp)],
    url_file_local_date_mirror( Url, DnDir, UrlOpts ),

	working_directory( Old, DnDir ),
    % os_base( Url, GzF ),  % GzF = Mus_musculus.gene_info.gz
	@ gunzip( -f, -k, GzF ),
    os_ext( gz, GeneInfoF, GzF ),
    mtx( GeneInfoF, Mtx, sep(tab) ),
    MapOpts = [     prefix(ncbi_mouse),
                    cnm_transform(ncbi_cnm),
                    to_value_2(=),
                    to_value_1(non_dash_sep_by('|')),
                    datetime(DntStamp),
                    source(Url),
                    header(row('Entrez Synonym','Symbol'))
        ],
    os_make_path( maps ),
    working_directory( _, maps ),
	csv_ids_map( _, 'Synonyms', 'Symbol', Mtx, EntzSynoF, MapOpts ),
    link_to_bio_sub( ncbi, EntzSynoF, [org(mouse),type(maps)] ),
	working_directory( _, Old ).

ncbi_cnm( 'Synonyms', syno ).
ncbi_cnm( 'Symbol', symb ).

non_dash_sep_by( _, '', _ ) :- !, fail. % do not include empties
non_dash_sep_by( _, '-', _ ) :- !, fail. % do not include empties
non_dash_sep_by( Sep, Atom, List ) :-
    atomic_list_concat(  List, Sep, Atom ).
