
% from human:   :- set_prolog_flag(stack_limit, 10 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/4).
:- lib(bio_db_source_url/2).
:- lib(build_dnloads_loc/3).
:- lib(url_file_local_date_mirror/3).

std_mouse_maps_ncbi_defaults( Defs ) :-
                                        Defs = [ db(ncbi),
                                                 debug(true),
                                                 debug_url(false),
                                                 debug_fetch(true),
                                                 gene_info_file('Mammalia/Mus_musculus.gene_info.gz'),
                                                 org(mouse)
                                        ].

/** std_mouse_maps_ncb(+Args).
    
Create a map of synonms to symbols according to NCBI.

    ncbi_musm_esyn_symb( Syno, Symb ).

Opts
  * db(ense)
    source database
  * debug(Dbg=true)
    bio_db_cnm_token( cust, Tkn ).
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/2)
  * debug_fetch(Ubg=true)
    whether to debug the fetch of the URL
  * org(Org=mouse)
    organism

==
?- std_mouse_maps_ncbi([]).
ορέστης;build_repo/mouse% date; pupsh std_mouse_maps_ncbi.pl ; date 
Tue 27 Dec 15:21:20 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
...
% Symbolic linking, '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/ncbi/maps/ncbi_musm_syno_symb.pl', to '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/data/musm/maps/ncbi/ncbi_musm_syno_symb.pl'
Tue 27 Dec 15:21:30 GMT 2022

ορέστης;ncbi/maps% date
Tue 27 Dec 15:31:28 GMT 2022
ορέστης;ncbi/maps% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/ncbi/maps
ορέστης;ncbi/maps% wc -l *_m*
70025 ncbi_musm_syno_symb.pl

==

@author nicos angelopoulos
@version  0.1 2019/02/12
@version  0.2 2022/12/27,   v4.0 naming conventions
@tbd double check symbols, are MGI known symbols....

*/
std_mouse_maps_ncbi( Args ) :-
     Self = std_mouse_maps_ncbi,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnloads_loc( Self, DnDir, Opts ),
     options_rename( Opts, [debug_url-debug,gene_info_file-url_file], Spts, true ),
     bio_db_source_url( Url, Spts ),
     options_rename( [interface(wget),file(GzF),dnt_stamp(DntStamp)|Opts], debug_url-debug, Fpts, true ),
     url_file_local_date_mirror( Url, DnDir, Fpts ),
     working_directory( Old, DnDir ),
     @ gunzip( -f, -k, GzF ),
     os_ext( gz, GeneInfoF, GzF ),
     mtx( GeneInfoF, Mtx, sep(tab) ),
     MapOpts = [     
                    to_value_1(non_dash_sep_by('|')),
                    to_value_2(=),
                    datetime(DntStamp),
                    source(Url),
                    header(row('Entrez Synonym','Symbol'))
                    |Opts
        ],
     os_make_path( maps ),
     working_directory( _, maps ),
     csv_ids_map( _, 'Synonyms', 'Symbol', Mtx, EntzSynoF, MapOpts ),
     link_to_bio_sub( ncbi, EntzSynoF, [org(mouse),type(maps)] ),
     working_directory( _, Old ).

non_dash_sep_by( _, '', _ ) :- !, fail. % do not include empties
non_dash_sep_by( _, '-', _ ) :- !, fail. % do not include empties
non_dash_sep_by( Sep, Atom, List ) :-
    atomic_list_concat(  List, Sep, Atom ).
