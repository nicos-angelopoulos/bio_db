
:- use_module(library(lib)).
:- lib(by_unix).    % @/1.
:- lib(upsh).       % you need to run upsh_make.
:- lib(debug_call).
:- debug(mouse).
:- ensure_loaded( '../lib/debug_colours' ).  % /2.

/**  std_mouse.

Run all build scripts for mouse datasets.

==
?- std_mouse.
==

@author nicos angelopoulos
@version  0.1 2018/11/7, fully tested on that date (added colours !)

*/
std_mouse :-
    debug_colours( Dlrs ),
    std_mouse_maps( Db ),
    debug_consec( mouse, Dlrs, 'Starting mouse maps for: ~ ... ', Db ),
    atomic_list_concat( [std,mouse,maps,Db], '_', Script ),
    @ upsh( f, Script ),
    debug_consec( mouse, Dlrs, '... finished mouse maps for: ~.', Db ),
    fail.
std_mouse :-
    debug_colours( Dlrs ),
    std_mouse_graphs( Db ),
    debug_consec( mouse, Dlrs, 'Starting mouse graphs for: ~ ... ', Db ),
    atomic_list_concat( [std,mouse,graphs,Db], '_', Script ),
    @ upsh( f, Script ),
    debug_consec( mouse, Dlrs, '... finished mouse graphs for: ~.', Db ),
    fail.
std_mouse :-
    debug_consec( mouse, [red,red], '... finished mouse standards.', [] ).

std_mouse_maps(unip).
std_mouse_maps(mgim).

std_mouse_gpaphs(strg).
