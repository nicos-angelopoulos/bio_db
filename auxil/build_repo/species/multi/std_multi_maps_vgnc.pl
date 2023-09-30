
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
:- lib(url_file_local_date_mirror/3).
:- lib(link_to_bio_sub/2).

vgnc_repo( 'https://ftp.ebi.ac.uk/pub/databases/genenames/vgnc/tsv/', 'vgnc_gene_set_All.txt.gz' ).

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
  * debug(Dbg=true)
    debugging, informational messages
  * download(Dn=true)
    set to false to skip downloading a fresh copy of the HGNC file(s)
  * maps_sub_dir(MsubD=maps)
    relative name for generated maps within downloads directory

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
     vgnc_repo( VgncUrlDir, GzF ),
     atom_concat( VgncUrlDir, GzF, SrcUrl ),
     absolute_file_name( bio_db_build_downloads(vgnc), VgncDnlD ),
     os_make_path( VgncDnlD ),
     url_file_local_date_mirror( SrcUrl, VgncDnlD, interface(wget) ),
     working_directory( Old, VgncDnlD ),
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
     maplist( link_to_bio_sub(multi), Files ),
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
