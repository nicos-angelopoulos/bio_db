
:- set_prolog_flag(stack_limit, 10 000 000 000).

:- use_module(library(csv)).        % csv_read_file/2.
:- use_module(library(process)).    % process_create/3.
:- use_module(library(readutil)).   % read_line_to_codes/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).
:- lib(ncbi_species_grep/3).
:- lib(stoics_lib:prefix_atom/2).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')).

% local libs & sources
:- lib(de_semi/3).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(ens_fa_peptide_gene_rows/2).     % /2, fixme: should be more local
:- lib(url_file_local_date_mirror/3).

std_chicken_maps_ncbi_defaults( Defs ) :-
                               Defs = [ db(ncbi),
                                        debug(true),
                                        debug_fetch(true),
                                        debug_url(false),
                                        iactive(true),
                                        ncbi_genes_file('gene2ensembl.gz'),
                                        org(chicken)
                               ].

/** std_chicken_maps_ncbi(+Opts).

Download latest NCBI gene to ensembl map file and convert it to 
a few standard maps.

Opts
  * db(Db)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * ncbi_genes_file(GnsF='')
    the url base for the genes download
  * org(Org=human)
    organism

==
?- std_maps_ncbi([]).
==

@author nicos angelopoulos
@version  0.1 2014/7/23
@version  0.2 2022/12/26, entz-> ncbi, url via wget, csv without R
@version  0.3 2023/9/30,  new style opts and helpers
@tbd  can we implement for chicken maps_ncbi_rnuc_symb/3, maps_ncbi_ensp_ensg/0 and maps_ncbi_ncbi_gont/0 from human ?

*/
std_chicken_maps_ncbi( Args ) :-
     Self = std_chicken_maps_ncbi,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnDir, Opts ),
     bio_db_source_url( Url, [ncbi_genes_file-url_file,debug_url-debug], Opts ),
     options( debug_fetch(Fbg), Opts ),
     url_file_local_date_mirror( Url, DnDir, [debug(Fbg),dnld_file(GnsF)|Opts] ),
     working_directory( Old, DnDir ),
     MapsD = maps,
     make_directory_path( MapsD ),
     directory_file_path( MapsD, GnsF, ToP ),
     copy_file( GnsF, ToP ),
     bio_db_dnt_times( GnsF, DnDt, _DnEn ),
     working_directory( _ParentD, MapsD ),
     @ gunzip( -k, -f, GnsF ),
     file_name_extension( RemS, gz, GnsF ),
     ncbi_species_grep( RemS, ChickG2NF, Opts ),
     debuc( Self, 'Grepped chicken gene2ensembl into: ~p', [ChickG2NF] ),
     std_chicken_maps_ncbi( Self, ChickG2NF, Url, DnDt, Opts ),
     delete_file( RemS ),
     %chicken?: maps_ncbi_rnuc_symb( Self ),
     % maps_ncbi_unig_ncbi,  % unigene is no longer maintained as of Feb.2019
     working_directory( _, Old ).

std_chicken_maps_ncbi( Self, ChickF, Url, DnDt, Opts ) :-
     TsvOpts = [match_arity(false),separator(0'\t)],
     csv_read_file( ChickF, Mtx, TsvOpts ),
     Mtx = [_Comment|Rows],
     Chick = [row(tax_id,ncbi,ensg,nucl_acc,ensr,prot_acc,ensp)|Rows],
     % GEnsGF = entrez_gene_id_ensg.pl,
     % csv_filter_by_column( New, tax_id, =(9606), HS ),
     % mtx_column_values_select( New, tax_id, 9823, Chick, _, true ),
     debuc( Self, length, hs_len/Chick ),
     Lens = [to_value_1(pos_integer),to_value_2(pfx_by('ENS')),datetime(DnDt),source(Url)|Opts],
     Rens = [to_value_2(pos_integer),to_value_1(pfx_by('ENS')),datetime(DnDt),source(Url)|Opts],
     csv_ids_map( ChickF, ncbi, ensg, Chick, GEnsGF, [header(row('Entrez ID','Ensembl Gene'))|Lens] ),
     csv_ids_map( ChickF, ensg, ncbi, Chick, EnsGGF, [header(row('Ensembl Gene','Entrez ID'))|Rens] ),
     % need to ensure prots are of ENSP  there are - in some entries
     Lenp = [to_value_1(pos_integer),to_value_2(pfx_by_de_v('ENS')),datetime(DnDt),source(Url)|Opts],
     csv_ids_map( ChickF, ncbi, ensp, Chick, GEnsPF, [header(row('Entrez ID','Ensembl Protein'))|Lenp] ),
     Renp = [to_value_2(pos_integer),to_value_1(pfx_by_de_v('ENS')),datetime(DnDt),source(Url)|Opts],
     csv_ids_map( ChickF, ensp, ncbi, Chick, EnsPGF, [header(row('Ensembl Protein','Entrez ID'))|Renp] ),
     link_to_bio_sub( ncbi, [GEnsGF,EnsGGF,GEnsPF,EnsPGF], [type(maps)|Opts] ).

pos_integer( Numb, Numb ) :-
     integer( Numb ),
     !,
     Numb > 0.
pos_integer( Atom, Numb ) :-
     atom_number( Atom, Numb ),
     !,
     integer( Numb ), 
     Numb > 0.

pfx_by_de_v( Pfx, Full, UnV ) :-
     prefix_atom( Pfx, Full ),
    ( atomic_list_concat([UnV,_],'.',Full) ->
        true
        ;
        UnV = Full
    ).

pfx_by( Pfx, Full, Full ) :-
     prefix_atom( Pfx, Full ).

write_lines_out(Out, Write) :-
        read_line_to_codes( Out, Line1 ),
        write_lines(Line1, Out, Write ).

write_lines(end_of_file, _, _) :- !.
write_lines(Codes, Out, Write) :-
        atom_codes(Line, Codes),
        write( Write, Line ), nl( Write ),
        read_line_to_codes(Out, Line2),
        write_lines(Line2, Out, Write).
