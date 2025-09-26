
:- use_module(library(debug)).
:- use_module(library(options)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).

% also sets lib alias to that dir
% :- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.
:- ensure_loaded('../../lib/std_org').   % /2.

% local libs & sources
% :- lib(debug_colours/1).

std_pig_defaults(  [ debug(true),
                     iactive(true),
                     org(pig)
                   ]
                ).

/** std_chicken.

Build all the standard datasets for bio_db for pig.

There doesn't seem to be a nonmeclature committee for pig.
Currently supported database are ense, gont ncbi and strg.
There are no building dependencies between the datasets.

Opts
  * debug(Dbg=true)
    progress, informational messages
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * org(Org=pig)
    organism

@author nicos angelopoulos
@version  0.1 2023/06/02
@version  0.2 2025/09/26,  option(iactive(Iact))

*/
std_pig( Args ) :-
    Self = std_pig,
    options_append( Self, Args, Opts ),
    options( org(Org), Opts ),
    std_org( Org, Opts ).

std_pig_debug( Org ) :-
     debug( pig, 'finished all standards for: ~w', [Org] ).

std(pig, graphs, strg).
%
% std(chicken, maps, cgnc). % haven't found a resource for pig gene names yet.
std(pig, maps, ense).
std(pig, maps, gont).
std(pig, maps, ncbi).
std(pig, maps, vgnc).
std(pig, maps, reac).
% std(pig, maps, unip).  % this is not avaiable at: https://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/
