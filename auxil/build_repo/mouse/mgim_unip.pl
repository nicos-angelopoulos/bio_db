
:- lib(os_lib).
:- lib(debug_call).

:- debug(mgim_unip).

:- ensure_loaded( '../bio_db_build_aliases' ).
:- bio_db_build_aliases([]).

/**  mgim_unip.

Compare mgim/map_mgim_mouse_mgim_uni.pl to unip/map_unip_mouse_mgim_unip.pl.

Opts
  * opt(Opt=_)
     is a...

==
?- mgim_unip.
==

@author nicos angelopoulos
@version  0.1 2018/11/3

*/
mgim_unip :-
    absolute_file_name( bio_db_build_data('mouse/maps'), MMapsD ),
    write( mouse_maps_dir(MMapsD) ), nl,
    os_path( MMapsD, mgim, MgimD ),
    os_path( MMapsD, unip, UnipD ),
    os_path( MgimD, 'map_mgim_mouse_mgim_unip.pl', MgimF ),
    os_path( UnipD, 'map_unip_mouse_mgim_unip.pl', UnipF ),
    debug( mgim_unip, 'MgimF: ~w', MgimF ),
    debug( mgim_unip, 'UnipF: ~w', UnipF ),
    ensure_loaded( MgimF ),
    ensure_loaded( UnipF ),
    findall( M:U, (map_mgim_mouse_mgim_unip(M,U),\+ map_unip_mouse_mgim_unip(M,U)), MUs ),
    debug_call( mgim_unip, length, mus/MUs ),  % 34
    findall( U:M, (map_unip_mouse_mgim_unip(M,U),\+ map_mgim_mouse_mgim_unip(M,U)), UMs ),
    debug_call( mgim_unip, length, ums/UMs ),  % 62415
    debug_call( mgim_unip, end, true ).
