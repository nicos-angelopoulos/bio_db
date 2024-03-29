
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

std_chicken_defaults( [ debug(true),
                        iactive(true),
                        org(chicken)
                      ]
                    ).

/** std_chicken.

Build all the standard datasets for bio_db for chicken.

There are no building dependencies between the datasets.

Opts
  * debug(Dbg=true)
    progress, informational messages
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * org(Org=chicken)
    organism

@author nicos angelopoulos
@version  0.1 2022/12/20

*/
std_chicken( Args ) :-
    Self = std_chicken,
    options_append( Self, Args, Opts ),
    options( org(Org), Opts ),
    std_org( Org, Opts ).

std_chicken_debug( Org ) :-
     debug( chicken, 'finished all standards for: ~w', [Org] ).

std(chicken, graphs, strg).
%
std(chicken, maps, cgnc).
std(chicken, maps, ense).
std(chicken, maps, ncbi).
std(chicken, maps, gont).
std(chicken, maps, reac).
std(chicken, maps, unip).
