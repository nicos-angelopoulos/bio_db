
:- ensure_loaded('../hs/std_maps_reac').

std_gallus_maps_reac_defaults( Defs ) :-
     Defs = [
               org(gallus)
     ].

/** std_gallus_maps_reac( Args ).

Spercialisation of reactome maps for chicken.

@author nicos angelopoulos
@version  0:1 2022/12/24
@see std_maps_reac/1

*/
std_gallus_maps_reac( Args ) :-
     Self = std_gallus_maps_reac,
     options_append( Self, Args, Opts ),
     std_maps_reac( Opts ).
