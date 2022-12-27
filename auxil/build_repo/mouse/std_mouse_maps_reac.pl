
:- ensure_loaded('../human/std_maps_reac').

std_mouse_maps_reac_defaults( Defs ) :-
     Defs = [
               org(mouse)
     ].

/** std_mouse_maps_reac( Args ).

Spercialisation of reactome maps for mouse.

==
?- std_mouse_maps_reac([]).

ορέστης;build_repo/mouse% date ; pupsh std_mouse_maps_reac.pl ; date
Tue 27 Dec 16:21:05 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
% Url: 'https://reactome.org/download/current/NCBI2Reactome_PE_All_Levels.txt'
...
% Starting enumeration of list: files
% 1./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_musm_ncbi_reac.pl
% 2./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_musm_ncbi_reap.pl
% 3./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_musm_reac_reap.pl
% 4./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_musm_reac_recn.pl
% 5./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_musm_reac_recl.pl
% 6./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_musm_reap_repn.pl
% Ended enumeration of list: files
% Finished: std_maps_reac
Tue 27 Dec 16:22:12 GMT 2022

ορέστης;reac/maps% date
Tue 27 Dec 16:22:35 GMT 2022
ορέστης;reac/maps% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps
ορέστης;reac/maps% wc -l *_m*
   16762 reac_musm_ncbi_reac.pl
   79839 reac_musm_ncbi_reap.pl
  116139 reac_musm_reac_reap.pl
   15871 reac_musm_reac_recl.pl
   15871 reac_musm_reac_recn.pl
    1721 reac_musm_reap_repn.pl
  246203 total


==

@author nicos angelopoulos
@version  0:1 2022/12/24
@see std_maps_reac/1

*/
std_mouse_maps_reac( Args ) :-
     Self = std_mouse_maps_reac,
     options_append( Self, Args, Opts ),
     std_maps_reac( Opts ).
