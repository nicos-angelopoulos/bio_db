:- use_module(library(debug)).  % make std_org/2 stop complaining of missing token
:- use_module(library(options)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).

% organism(human).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(options).

% also sets lib alias to that dir
% :- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.
:- ensure_loaded('../../lib/std_org').   % /2.

% local libs & sources
% :- lib(debug_colours/1).  % /1.

std_human_defaults( [ org(human),
                      iactive(true),
                      debug(true)
                    ]
                 ).

/** std_human(+Opts).

Build all the standard datasets for bio_db.

Opts
  * debug(Dbg=true)
    debug this level
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * pass(Term)
    Terms to pass to the upsh calls in lower level

Dependencies
  * on([])
    maps_hgnc, maps_unip, maps_reac, graphs_gont
  * on(hgnc)
    maps_ncbi, maps_ense, maps_gont
  * on(hgnc,ncbi)
    graphs_strg
  * on([hgnc,unip])
    maps_unip_seqs
  * on([hgnc,unip,unip_seqs])
    maps_pros

==
% date
Thu 10 Sep 19:34:38 BST 2020
% upsh std_human pass=assembly=38 release=101
==

@author nicos angelopoulos
@version  0.1 2017/10/12
@version  0.2 2018/03/30
@version  0.3 2018/11/05, everything tested, and working
@version  0.4 2022/12/28, hs-> human + factored out std_org/2
@tbd add timings (in file)

*/
std_human( Args ) :-
    Self = std_human,
    options_append( Self, Args, Opts ),
    % bio_db_build_aliases( Opts ),
    options( org(Org), Opts ),
    std_org( Org, Opts ).

% not used. just makes human a valid debug token
std_human_debug( Org ) :-
    debug( human, 'finished all standards for: ~w', [Org] ).

std(human, maps, hgnc).
std(human, maps, ense).
std(human, maps, unip).
std(human, maps, unip_seqs).
std(human, maps, ncbi).
% needs "unip_seqs" above to have ran
std(human, maps, pros).
% requires, maps-hgnc
std(human, maps, gont).
std(human, maps, reac).

std(human, graphs, gont).
std(human, graphs, strg).   % needs hgnc and ncbi
