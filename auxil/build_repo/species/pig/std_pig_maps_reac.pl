
:- ensure_loaded('../human/std_maps_reac').

std_pig_maps_reac_defaults( Defs ) :-
     Defs = [
               org(pig)
     ].

/** std_pig_maps_reac( Args ).

Spercialisation of reactome maps for pig.

ορέστης;build_repo/pig% date; !!; date 
date ; pupsh std_pig_maps_reac.pl ; date
Tue 27 Dec 12:07:14 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
...
% Starting enumeration of list: files
% 1./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_galg_ncbi_reac.pl
% 2./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_galg_ncbi_reap.pl
% 3./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_galg_reac_reap.pl
% 4./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_galg_reac_recn.pl
% 5./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_galg_reac_recl.pl
% 6./home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps/reac_galg_reap_repn.pl
% Ended enumeration of list: files
% Finished: std_maps_reac
Tue 27 Dec 12:08:13 GMT 2022

ορέστης;dnloads/reac% date
Tue 27 Dec 12:12:18 GMT 2022
ορέστης;dnloads/reac% pwd 
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/reac/maps
ορέστης;dnloads/reac% wc -l *
    7150 maps/reac_galg_ncbi_reac.pl
   34935 maps/reac_galg_ncbi_reap.pl
   49728 maps/reac_galg_reac_reap.pl
    6931 maps/reac_galg_reac_recl.pl
    6931 maps/reac_galg_reac_recn.pl
    1643 maps/reac_galg_reap_repn.pl
   29703 maps/reac_homs_ncbi_reac.pl
  137767 maps/reac_homs_ncbi_reap.pl
  231215 maps/reac_homs_reac_reap.pl
   28564 maps/reac_homs_reac_recl.pl
   28564 maps/reac_homs_reac_recn.pl
    2572 maps/reac_homs_reap_repn.pl
  565703 total

@author nicos angelopoulos
@version  0:1 2022/12/24
@see std_maps_reac/1

*/
std_pig_maps_reac( Args ) :-
     Self = std_pig_maps_reac,
     options_append( Self, Args, Opts ),
     std_maps_reac( Opts ).
