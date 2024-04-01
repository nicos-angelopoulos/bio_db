
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

std_pig_maps_ncbi_defaults( Defs ) :-
          Defs = [
                    ncbi_gene_info('GENE_INFO/Mammalia/Sus_scrofa.gene_info.gz'),
                    org(pig)
                 ].

/** std_pig_maps_ncbi(+Opts).

Build standard NCBI maps for pig. 

Opts
 * ncbi_gene_info(GnInf='GENE_INFO/Mammalia/Sus_scrofa.gene_info.gz')
   genes file for pig
 * org(Org=pig)
   organism

All code has moved to lib(ncbi_std_maps.pl) as it is can be used for other species.

@author nicos 
@version  0.1 2024/03/27
@see ncbi_std_maps/1
*/
std_pig_maps_ncbi( Args ) :-
     Self = std_pig_maps_ncbi,
     options_append( Self, Args, Opts ),
     ncbi_std_maps( Opts ).
