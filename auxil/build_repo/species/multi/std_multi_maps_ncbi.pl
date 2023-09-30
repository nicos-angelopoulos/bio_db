
% :- set_prolog_flag(stack_limit, 10 000 000 000).

:- use_module(library(process)).    % process_create/3.
:- use_module(library(readutil)).   % read_line_to_codes/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).
:- lib(stoics_lib:prefix_atom/2).
% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')).

% local libs & sources
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).

% ncbi_taxonomy_repo('https://ftp.ncbi.nih.gov/pub/taxonomy/', 'taxdmp.zip', 'names.dmp'). % the first is the download the second is the local name we unzip to

std_multi_maps_ncbi_defaults( Defs ) :-
                                   Defs = [ db(vgnc),
                                            debug(true),
                                            debug_fetch(true),
                                            debug_url(false),
                                            iactive(true),
                                            download(true),
                                            ncbi_taxo_base(ncbi_taxo),
                                            ncbi_taxo_file('taxdmp.zip'),
                                            ncbi_uzip_file('names.dmp'),
                                            maps_sub_dir(maps),
                                            org(pig)
                                          ].

/** std_multi_maps_ncbi(+Opts).

Download latest NCBI taxonomy map file and convert it to two standard maps.

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
  * ncbi_taxo_base(TxB=ncbi_taxo),
    token identifying the Dir Url prefix for the remote URL (defined in bio_db_source_base_url/2)
  * ncbi_taxo_file(TxF='taxdmp.zip'),
    file name of the remote URL 
  * ncbi_uzip_file(TxU='names.dmp'),
    local filename for the unzip download
  * org(Org=pig)
    organism
  * vgnc_genes_file(VgncF='vgnc_gene_set_All.txt.gz')
    the file name for the URL download
==
  ?- std_multi_maps_ncbi([]).
==
@author nicos angelopoulos
@version  0.1 2023/9/15
*/
std_multi_maps_ncbi( Args ) :-
     Self = std_multi_maps_ncbi,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnlD, Opts ),
     SrcRnms = [ncbi_taxo_base-url_base,ncbi_taxo_file-url_file,debug_url-debug],
     bio_db_source_url( SrcUrl, SrcRnms, Opts ),
     options( [debug_fetch(Fbg),ncbi_uzip_file(DmpF)], Opts ),
     url_file_local_date_mirror( SrcUrl, DnlD, [debug(Fbg),dnld_file(ZipF)|Opts] ),
     working_directory( Old, DnlD ),
     @unzip( '-o', ZipF, DmpF),
     mtx( DmpF, Ntx, sep(0'|) ),
     debuc( Self, dims, names_dump/Ntx ),
     maplist( taxon_names_args, Ntx, Atx ),
     taxon_names_map( Atx, Snms, Gnms ),
     sort( Snms, OSnms ),
     sort( Gnms, OGnms ),
     debuc( Self, length, [sc_names,gb_names]/[OSnms,OGnms] ),
     ScnmF = 'ncbi_mult_taxo_scnm.pl',
     GbnmF = 'ncbi_mult_taxo_gbnm.pl',
     bio_db_dnt_times( ZipF, DnDt, _DnEn ),
     debuc( Self, 'Downladed at: ~w' , [DnDt] ),
     MapsD = maps,
     make_directory_path( MapsD ),
     working_directory( _ParentD, MapsD ),
     portray_clauses( OSnms, file(ScnmF) ),
     debuc( Self, wrote, ScnmF ),
     portray_clauses( OGnms, file(GbnmF) ),
     debuc( Self, wrote, GbnmF ),
     bio_db_add_infos( [ScnmF,GbnmF,source(SrcUrl),datetime(DnDt)] ),
     debuc( Self, 'Done: ~w', [bio_db_add_infos/1] ),
     link_to_bio_sub( ncbi, ScnmF, org(multi) ),
     link_to_bio_sub( ncbi, GbnmF, org(multi) ),
     working_directory( _, Old ).

taxon_names_args( Term1, Term2 ) :-
     % debuc( taxon_names, 'Term1: ~w.', [Term1] ),
     maparg( taxon_names_arg, Term1, Term2 ).

taxon_names_arg( Arg1, Arg2 ) :-
     % debuc( taxon_names, 'Arg1: ~w.', [Arg1] ),
     % sleep( 3 ),
     ( atom_concat(' ',ArgX,Arg1)-> true;
        ( atom_concat(ArgX,' ',Arg1) -> true;
          ( atom_concat('\t',ArgX,Arg1) -> true;
            ( atom_concat(ArgX,'\t',Arg1) -> true; 
               ArgX = Arg1
            )
          )
       )
     ),
     ( ArgX == Arg1 -> ( catch(atom_number(Arg1,Arg2),_,fail) -> true;  Arg2 = Arg1) ; taxon_names_arg(ArgX,Arg2) ).

taxon_names_map( [], [], [] ).
taxon_names_map( [row(I,Sec,_Thr,Frt,_Fif)|T], Snms, Gnms ) :-
     ( Frt == 'scientific name' -> 
          Snms = [ncbi_mult_taxo_gbnm(I,Sec)|TSnms],
          Gnms = TGnms
          ;
          Snms = TSnms,
          ( Frt == 'genbank common name' ->
               Gnms = [ncbi_mult_taxo_gbnm(I,Sec)|TGnms]
               ;
               Gnms = TGnms
          )
     ),
     taxon_names_map( T, TSnms, TGnms ).

/*
     bio_db_dnt_times( 'gene2unigene', UgDnDt, _DnEn ),
     csv_ids_map( _, 'unig', 'ncbi', [Hdr|Csv], OutF, MOpts ),
     link_to_bio_sub( ncbi, OutF ), 
     */
