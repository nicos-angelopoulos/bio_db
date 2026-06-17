
% sets libs and download aliases
:- ensure_loaded(pack(bio_db/src/bio_db_build_aliases)).    % /1

% local libs
:- lib(maps_reac/1).

std_maps_reac_defaults([]).

/** std_chicken_maps_reac( Args ).

Specialisation of reactome maps for human.

This is the default for maps_reac/1, so no further options are needed.

@author nicos angelopoulos
@version  0:1 2022/12/24
@see maps_reac/1
*/

std_maps_reac( Args ) :-
     Self = std_maps_reac,
     options_append( Self, Args, Opts ),
     maps_reac( Opts ).
