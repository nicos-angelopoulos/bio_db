
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).

% also sets lib alias to that dir
% :- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.
:- ensure_loaded('../../lib/std_org').   % /2.

% local libs & sources
% :- lib(debug_colours/1).

std_multi_defaults([debug(true),org(multi)]).

/** std_multi.

Build all the standard bio_db datasets that hold data for multiple organisms.

@author nicos angelopoulos
@version  0.1 2023/09/15

*/
std_multi( Args ) :-
    Self = std_multi,
    options_append( Self, Args, Opts ),
    options( org(Org), Opts ),
    std_org( Org, Opts ).

std_multi_debug( Org ) :-
     debuc( std_multi, 'finished all standards for: ~w', [Org] ).

% std(pig, graphs, strg).
%
% std(chicken, maps, cgnc). % haven't found a resource for pig gene names yet.
% std(pig, maps, ense).
% std(pig, maps, gont).
std(multi, maps, ncbi).
% std(pig, maps, reac).
std(pig, maps, vgnc).
