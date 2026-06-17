
:- set_prolog_flag(stack_limit, 20 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% stoics packs, lib knowns how to deal with these (will install if missing)
:- lib(options).
:- lib(debug_call).

% sets libs and download aliases
:- ensure_loaded(pack(bio_db/src/bio_db_build_aliases)).    % /1

% local libs
:- lib(maps_ncbi/1).

std_mouse_maps_ncbi_defaults( Defs ) :-
          Defs = [
                    ncbi_gene_info('GENE_INFO/Mammalia/Mus_musculus.gene_info.gz'),
                    org(mouse)
                 ].

/** std_mouse_maps_ncbi(+Opts).

Build starndard NCBI maps for mouse. 

Opts
 * ncbi_gene_info(GnInf='GENE_INFO/Mammalia/Mus_musculus.gene_info.gz')
   genes synonyms file
 * org(Org=mouse)
   organism

All code has moved to lib(maps_ncbi.pl) as it is can be used for other species.

@author nicos 
@version  0.1 2024/03/27
@see maps_ncbi/1
*/
std_mouse_maps_ncbi( Args ) :-
     Self = std_mouse_maps_ncbi,
     options_append( Self, Args, Opts ),
     maps_ncbi( Opts ).

