
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

std_chicken_maps_ncbi_defaults( Defs ) :-
               Defs = [
                         ncbi_gene_info('GENE_INFO/Non-mammalian_vertebrates/Gallus_gallus.gene_info.gz'),
                         org(chicken)
                      ].

/** std_chicken_maps_ncbi(+Opts).

Build starndard NCBI maps for chicken. 

Opts
 * ncbi_gene_info(GnInf='GENE_INFO/Non-mammalian_vertebrates/Gallus_gallus.gene_info.gz'),
   ncbi genes file for chicken
 * org(Org=chicken)
   organism

All code has moved to lib(ncbi_std_maps.pl) as it is can be used for other species.

@author nicos 
@version  0.1 2024/03/27
@see ncbi_std_maps/1
*/
std_chicken_maps_ncbi( Args ) :-
     Self = std_chicken_maps_ncbi,
     options_append( Self, Args, Opts ),
     ncbi_std_maps( Opts ).

