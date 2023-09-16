
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
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).

:- debuc(url_file).

ncbi_taxonomy_repo('https://ftp.ncbi.nih.gov/pub/taxonomy/', 'taxdmp.zip', 'names.dmp').

ncbi_dnload( Loc ) :-
     absolute_file_name( bio_db_build_downloads(ncbi), Loc ),
     os_make_path( Loc, debug(true) ).

std_multi_maps_ncbi_defaults(debug(true)).

%% std_multi_maps_ncbi(+Opts).
%
% Download latest NCBI taxonomy map file and convert it to two standard maps.
%
%==
% std_multi_maps_ncbi([]).
%==
% @author nicos angelopoulos
% @version  0.1 2023/9/15
%
std_multi_maps_ncbi( Args ) :-
     Self = std_multi_maps_ncbi,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     % load necessary data that has already been generated
     % ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_symb_hgnc')),
     ncbi_taxonomy_repo( TaxD, ZipF, DmpF ),
     atom_concat( TaxD, ZipF, Url ),
     ncbi_dnload( NcbiD ),
     url_file_local_date_mirror( Url, NcbiD, interface(wget) ),
     working_directory( Old, NcbiD ),
     MapsD = maps,
     make_directory_path( MapsD ),
     @unzip( '-f', '-o', ZipF, DmpF ),
     % directory_file_path( MapsD, DmpF, ToP ),
     mtx( DmpF, Ntx, sep(0'|) ),
     debuc( Self, dims, names_dump/Ntx ),
     maplist( taxon_names_args, Ntx, Atx ),
     taxon_names_map( Atx, Snms, Gnms ),
     sort( Snms, OSnms ),
     sort( Gnms, OGnms ),
     debuc( Self, length, [sc_names,gb_names]/[OSnms,OGnms] ),
     @ls(),
     @pwd(),
     ScnmF = 'maps/ncbi_mult_taxo_scnm.pl',
     GbnmF = 'maps/ncbi_mult_taxo_gbnm.pl',
     portray_clauses( OSnms, file(ScnmF) ),
     portray_clauses( OGnms, file(GbnmF) ),
     bio_db_dnt_times( ZipF, DnDt, _DnEn ),
     working_directory( _ParentD, MapsD ),
     bio_db_add_infos( [ScnmF,GbnmF,source(Url),datetime(DnDt)] ),
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
