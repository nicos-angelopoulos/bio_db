
:- lib(bio_db_build_organism/0).
:- lib(bio_db_build_messages/0).             % loads the error messages pretty printing

/** org_ense_dir( +Org, -EnseDir, +Opts ).

Map bio_db Org-anism to Ensembl directory name.

Opts are only used for reporting calling paths in errors.

==
?- org_ense_dir( human, Dir, true ).
Dir = 'Homo_sapiens'.

?- org_ense_dir( cow, Dir, true ).
ERROR: Not a valid organism, for mapping to an Ensembl directory: cow (2nd arg: _47038)
==

@author nicos angelopoulos
@version  0:1 2023/9/24
@tbd the error in the examples should also show the caller

*/
org_ense_dir( Org, Dir, Stem, Opts ) :-
     % bio_db_organism( Org, Canon ),  % this should throw an error
     (  (bio_db_organism(Org,Canon),org_ense_dir_known(Canon,Eir)) -> 
                        true
                        ; 
                        ( org_ense_dir_known(Org,Dir,Stem) ->
                              true
                              ; 
                              throw(org_ense_miss(Org,Eir), [bio_db:org_ense_dir/3|Opts]) 
                        )
     ).
     
% gaallus_gallus/Gallus_gallus.bGalGal1.mat.broiler.GRCg7b.110.abinitio.gtf.gz
org_ense_dir_known(chicken,'gallus_gallus','Gallus_gallus.bGalGal1.mat.broiler').
org_ense_dir_known(galg,'gallus_gallus','Gallus_gallus.bGalGal1.mat.broiler').
org_ense_dir_known(gg6a,'gallus_gallus_gca000002315v5','Gallus_gallus_gca000002315v5').
org_ense_dir_known(human,homo_sapiens,'Homo_sapiens').
org_ense_dir_known(mouse,mus_musculus,'Mus_musculus').
