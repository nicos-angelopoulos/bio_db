
:- use_module(library(debug)).  %  simply to stop std_org/2 from complaining
:- use_module(library(options)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib) ).

% organism(mouse).

% stoics.org.uk packs, lib knowns how to deal with these (will install if missing)
:- lib(options).
% :- lib(debug_call).

% also sets lib alias to that dir
% :- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.
:- ensure_loaded('../../lib/std_org.pl').  % /2.

% local libs & sources
% :- lib(debug_colours/1).

std_mouse_defaults([debug(true),org(mouse)]).

/** std_mouse.

Build all the standard datasets for bio_db.

Dependencies
 * on([])
   maps_mgim,maps_ncbi,maps_reac,maps_unip
 * on(maps_mgim)
   maps_ense, maps_gont
 * on([maps_mgim,maps_unip])
   mouse_graphs_strg

@author nicos angelopoulos
@version  0.1 2017/10/12
@version  0.2 2018/03/30
@version  0.3 2018/11/05, everything tested, and working
@version  0.4 2020/09/11, added ense
@version  0.5 2022/12/27, added reac
@tbd add timings (in file)

*/
std_mouse( Args ) :-
    Self = std_mouse,
    options_append( Self, Args, Opts ),
    options( org(Org), Opts ),
    % bio_db_build_aliases( Opts ),
    % organism( Org ),
    std_org( Org, Opts ).

% not used. just makes human a valid debug token

std_mouse_debug( Org ) :-
    debug( mouse, 'finished all standards for: ~w', [Org] ).

std(mouse, maps, mgim).
std(mouse, maps, ense).
std(mouse, maps, ncbi).
std(mouse, maps, unip).
std(mouse, maps, gont).
std(mouse, maps, reac).

% std( mouse, graphs, gont ).
std(mouse, graphs, strg).
