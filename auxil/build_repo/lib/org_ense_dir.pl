
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
org_ense_dir( Org, Eir, Opts ) :-
     % bio_db_organism( Org, Canon ),  % this should throw an error
     ( (bio_db_organism(Org,Canon),org_ense_dir_known(Canon,Eir)) -> true; throw(org_ense_miss(Org,Eir), [bio_db:org_ense_dir/3|Opts]) ).
     
org_ense_dir_known(human,'Homo_sapiens').
org_ense_dir_known(mouse,'Mus_musculus').
