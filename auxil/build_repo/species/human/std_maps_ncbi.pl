
:- set_prolog_flag(stack_limit, 20 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% stoics packs, lib knowns how to deal with these (will install if missing)
:- lib(options).
:- lib(debug_call).

% also sets lib alias to thadir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local
:- lib(ncbi_std_maps/1).

std_human_maps_ncbi_defaults( [org(human)] ).

/** std_human_maps_ncbi(+Opts).

Build starndard NCBI maps for human. 

Opts
 * org(Org=human)
   organism

All code has moved to lib(ncbi_std_maps.pl) as it is can be used for other species.

@author nicos 
@version  0.1 2024/03/27
@see ncbi_std_maps/1
*/
std_maps_ncbi( Args ) :-
     Self = std_human_maps_ncbi,
     options_append( Self, Args, Opts ),
     ncbi_std_maps( Opts ).

/* code that is not currently used from here on
*/

is_a_symbol( Symb, Symb ) :-
     hgnc:hgnc_homs_symb_hgnc( Symb, _ ),
     !.


% maps_ncbi_ensp_ensg.
%
% This is a later addition.
% See ens_fa_peptide_gene_rows..pl
%
maps_ncbi_ensp_ensg :-
     % Dir = '/usr/local/users/nicos/work/db/data/ncbi',
     fixme,
     ncbi_dnload( Dir ),
     EnsF= 'Homo_sapiens.GRCh38.pep.all.fa',
     working_directory( Old, Dir ),
     ens_fa_peptide_gene_rows( EnsF, EnsRows ),
     csv_ids_map( _CsvF, ensp, ensg, EnsRows, OutF, [prefix(ncbi),header(row('Ensembl Protein','Ensembl Gene'))] ),
     link_to_bio_sub(ncbi, OutF ),
     working_directory( _, Old ).

% fixme: this is not in the loop ? either fix or remove
maps_ncbi_ncbi_gont( Opts ) :-
     % Dir = '/usr/local/users/nicos/work/db/data/ncbi',
     ncbi_dnload( Dir ),
     ncbi_repo( Repo ),
     os_path( Repo, 'gene2go.gz', Url ),
     url_file_local_date_mirror( Url, Dir, [debug(true),interface(wget)] ),
     working_directory( Old, Dir ),
     @ rm( -f, gene2go_hs ),
     @ rm( -f, gene2go ),
     @ gunzip( -f, -k, 'gene2go.gz' ),
     % debuc( by_unix ),
     ncbi_species_grep( gene2go, _HsStem, Opts ),  % pass sep(tab)
     % os_grep_mtx(gene2go, '^9606', gene2go_hs, true ),
     % system( 'grep "^9606" gene2go | cat gene2go_hs' ),
     working_directory( _, Old ).

/** retired code, from here on
     ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_symb_hgnc')),
     */
/** %unigene is no longer maintained as of Feb.2019
maps_ncbi_unig_ncbi :-
     ncbi_dnload( Dir ),
     ncbi_repo( Repo ),
     os_path( Repo, 'gene2unigene', Url ),
     url_file_local_date_mirror( Url, Dir, [debug(true),interface(wget)] ),
     working_directory( Old, Dir ),
     bio_db_dnt_times( 'gene2unigene', UgDnDt, _DnEn ),
     csv_read_file( gene2unigene, [_|Csv], [separator(0'\t),match_arity(false)] ),
     Hdr = row(ncbi,unig),
     MOpts = [prefix(ncbi),to_value_1(hs_unig),datetime(UgDnDt),source(Url),header(row('Uni Gene','Entrez ID'))], 
     csv_ids_map( _, 'unig', 'ncbi', [Hdr|Csv], OutF, MOpts ),
     os_make_path( maps ),
     @ mv( -f, OutF, maps ),
     working_directory( _, maps ),
     link_to_bio_sub( ncbi, OutF ), 
     working_directory( _, Old ).

hs_unig( In, In ) :-
     atom_concat( 'Hs.', _, In ).

*/
