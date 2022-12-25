
:- ensure_loaded('../hs/std_maps_reac').

std_mouse_maps_reac_defaults( Defs ) :-
     Defs = [
               org(mouse)
     ].

/** std_mouse_maps_reac( Args ).

Spercialisation of reactome maps for mouse.

@author nicos angelopoulos
@version  0:1 2022/12/24
@see std_maps_reac/1

*/
std_mouse_maps_reac( Args ) :-
     Self = std_mouse_maps_reac,
     options_append( Self, Args, Opts ),
     std_maps_reac( Opts ).
