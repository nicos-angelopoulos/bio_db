
:- use_module(library(filesex)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
% :- lib(bio_db).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:arg_add/4).
:- lib(stoics_lib:portray_clauses/2).

% also sets lib alias that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(de_semi/3).
:- lib(mtx_map/4).
:- lib(bio_db_dnt_times/3).
:- lib(link_to_bio_sub/2).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).

std_multi_maps_vgnc_defaults( Defs ) :-
                                   Defs = [ db(vgnc),
                                            debug(true),
                                            debug_fetch(true),
                                            debug_url(false),
                                            download(true),
                                            iactive(true),
                                            vgnc_genes_file('vgnc_gene_set_All.txt.gz'),
                                            maps_sub_dir(maps),
                                            org(multi)
                                          ].
/** std_multi_maps_vgnc(+Opts).

Create some maps from HGNC's "complete" data file.

Opts
  * db(Db=vgnc)
    source database
  * debug(Dbg=true)
    debugging, informational messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * download(Dn=true)
    set to false to skip downloading a fresh copy of the HGNC file(s)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * maps_sub_dir(MsubD=maps)
    relative name for generated maps within downloads directory
  * org(Org=multi)
    organism, multi covers the case of relations over multiple organisms
  * vgnc_genes_file(VgncF='vgnc_gene_set_All.txt.gz')
    the file name for the URL download

==
?- std_multi_maps_vgnc.
==

@author nicos angelopoulos
@version  0.1 2023/8/16,  based on hgnc support- but modernised

*/
std_multi_maps_vgnc :-
    std_multi_maps_vgnc( [] ).

std_multi_maps_vgnc( Args ) :-
     Self = std_multi_maps_vgnc,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnlD, Opts ),
     bio_db_source_url( SrcUrl, [vgnc_genes_file-url_file,debug_url-debug], Opts ),
     options( debug_fetch(Fbg), Opts ),
     url_file_local_date_mirror( SrcUrl, DnlD, [debug(Fbg),dnld_file(GzF)|Opts] ),
     working_directory( Old, DnlD ),
     @ gunzip( -f, -k, GzF ),
     bio_db_dnt_times( GzF, DnDt, _DnEnd ),
     options( maps_sub_dir(MapsD), Opts ),
     os_make_path( MapsD ),
     % options_propagate( map_prefix, Opts, StdOT, true ),
     MOpts = [mtx(_Mtx),sep(tab),db(vgnc),org(multi),odir(MapsD),
              datetime(DnDt),source(SrcUrl)|Opts],
     os_ext( gz, TxtF, GzF ),
     std_multi_maps_vgnc_fix_dnload( Self, TxtF ),
     mtx_map( TxtF, [vgnc_id:vgnc:de_semi('VGNC'),taxon_id,symbol], VgncSymbF, MOpts ),
     mtx_map( TxtF, [vgnc_id:vgnc:de_semi('VGNC'),taxon_id,name], VgncNameF, MOpts ),
     % debuc( Self, 'doing links...', [] ),
     Files = [VgncSymbF,VgncNameF],
     link_to_bio_sub( multi, Files, [org(multi),type(maps)] ),
     % file_name_extension( TxtF, gz, GzF ),
     % delete_file( TxtF ),
     working_directory( _, Old ).

% I reported that in September, but never heard from them.
std_multi_maps_vgnc_fix_dnload( Self, TxtF ) :-
     mtx( TxtF, Mtx, [match(false),sep(tab)] ),
     Mtx = [Faulty,Row|Rows],
     functor( Faulty, _, Farity ),
     functor( Row, _, Rarity ),
     ( Farity =:= Rarity ->   % this will always be false, as the caller gzip afresh...
          debuc( Self, 'File: ~p had already been corrected.', [TxtF] )
          ;
          debuc( Self, 'Correcting header of file: ~p. Arities were ~d/~d .', [TxtF,Farity,Rarity] )
     ),
     arg_add( -2, Faulty, '', Naulty ),
     mtx( TxtF, [Naulty|Rows], sep(tab) ).
