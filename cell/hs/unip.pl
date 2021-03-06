:- module( bio_db_hs_unip, [
                bio_db_hs_unip/0,
                %       + uni prot db
                map_unip_hgnc_unip/2,
                map_unip_ensp_unip/2,
                map_unip_trem_nucs/2,
                map_unip_unip_entz/2,
                map_unip_unip_hgnc/2,
                % map_unip_unip_unig/2,  % Unigene database was retired in 2019
                map_unip_sprt_seqn/2,
                map_unip_trem_seqn/2
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_hs_unip.

Documentation predicate for Homo sapiens data from Uniprot database.

==
?- lib( & bio_db(hs(unip)) ).
?- [ pack('bio_db/cell/hs/unip') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs_unip.

/**  map_unip_hgnc_unip( +Hgnc, -UniP ).

Map predicate from HGNC gene ids to Uniprot proteins.

== 
?- map_unip_hgnc_unip( Hgnc, Unip ).
==

*/
map_unip_hgnc_unip( X, Y ) :-
    bio_db:bio_db_serve( map_unip_hgnc_unip(X,Y) ).

/**  map_unip_unip_hgnc( ?UniP, ?Hgnc ).

Map predicate from Uniprot proteins to HGNC ids.

==
?-  map_unip_unip_hgnc( 'Q96Q04', Hgnc ).
Hgnc = 19295.

?- map_unip_unip_hgnc( 'A0A0A0MQW5', Hgnc ).
Hgnc = 19295.

==
*/
map_unip_unip_hgnc( X, Y ) :-
    bio_db:bio_db_serve( map_unip_unip_hgnc(X,Y) ).

/*  map_unip_unip_unig( ?UniP, ?UniG).

NO LONGER IN SOURCE DATA. 2020/03/07

Map predicate from Uniprot proteins to Uniprot genes.

==
?- map_unip_unip_unig( 'Q96Q04', UniG ).
UniG = 'Hs.207426'.
==
map_unip_unip_unig( X, Y ) :-
    bio_db:bio_db_serve( map_unip_unip_unig(X,Y) ).
*/

/**  map_unip_sprt_seqn( ?Swissprot, ?Seqn ).

Map predicate from Uniprot (Swiprot, the curated parts) to its sequence.

==
?- map_unip_sprt_seqn( 'Q96Q04', Seqn ).

UniG = 'MPAPGALI....'.

==
*/
map_unip_sprt_seqn( Sprt, Seqn ) :-
    bio_db:bio_db_serve( map_unip_sprt_seqn(Sprt,Seqn) ).
    
/**  map_unip_trem_seqn( ?Trem, ?Seqn ).

Map predicate from Uniprot (Trembl, the un-curated parts) to its sequence.

==
?- map_unip_trem_seqn('A0A023HHK9', Seqn).

Seqn = 'MSRSRHARPSRLVRKEDVNKKKKNSQLRKTTKGANKNVASVKTLSPGKLKQLIQERDVKKKTEPKPPVPVRSLLTRAGAARMNLDRTEVLFQNPESLTCNGFTMALRSTSLSRRLSQPPLVVAKSKKVPLSKGLEKQHDCDYKILPALGVKHSENDSVPMQDTQVLPDIETLIGVQNPSLLKGKSQETTQFWTQRVEDSKINIPTHSGPAAEILPGPLEGTRCGEGLFSEETLNDTSGSPKMFAQDTVCAPFPQRVTPKVTSQGNPSIQLEELGSRVESLKLSDSYLDPIKSEHDCYPTSSLNKVIPDLNLRNCLALGGSTSPTSVIKFLLAGSKQATLGAKPDHQEAFEATANQQEVSDTTSFLGQAFGAIPHQWELPGADPVHGEALGETPDLPEIPGAIPVQGEVFGTILDQQETLGMSGSVVPDLPVFLPVPPNPIATFNAPSKWPEPQSTVSYGLAVQGAIQILPLGSGHTPQSSSNSEKNSLPPVMAISNVENEKQVHISFLPANTQGFPLAPERGLFHASLGIAQLSQAGPSKSDRGSSQVSVTSTVHVVNTTVVTMPVPMVSTSSSSYTTLLPTLEKKKRKRCGVCEPCQQKTNCGECTYCKNRKNSHQICKKRKCEELKKKPSVVVPLEVIKENKRPQREKKPKVLKVLRRSSDEEKVLCLVRQRTGHHCPTAVMVVLIMVWDGIPLPMADRLYTELTENLKSYNGHPTDRRCTLNENRTCTCQGIDPETCGASFSFGCSWSMYFNGCKFGRSPSPRRFRIDPSSPLHEKNLEDNLQSLATRLAPIYKQYAPVAYQNQVEYENVARECRLGSKEGRPFSGVTACLDFCAHPHRDIHNMNNGSTVVCTLTREDNRSLGVIPQDEQLHVLPLYKLSDTDEFGSKEGMEAKIKSGAIEVLAPRRKKRTCFTQPVPRSGKKRAAMMTEVLAHKIRAVEKKPIPRIKRKNNSTTTNNSKPSSLPTLGSNTETVQPEVKSETEPHFILKSSDNTKTYSLMPSAPHPVKEASPGFSWSPKTASATPAPLKNDATASCGFSERSSTPHCTMPSGRLSGANAAAADGPGISQLGEVAPLPTLSAPVMEPLINSEPSTGVTEPLTPHQPNHQPSFLTSPQDLASSPMEEDEQHSEADEPPSDEPLSDDPLSPAEEKLPHIDEYWSDSEHIFLDANIGGVAIAPAHGSVLIECARRELHATTPVEHPNRNHPTRLSLVFYQHKNLNKPQHGFELNKIKFEAKEAKNKKMKASEQKDQAANEGPEQSSEVNELNQIPSHKALTLTHDNVVTVSPYALTHVAGPYNHWV' .
==

*/
map_unip_trem_seqn( Trem, Seqn ) :-
    bio_db:bio_db_serve( map_unip_trem_seqn(Trem,Seqn) ).

/*  map_unip_usyn_unip( ?USyn, ?UniP ).

Map predicate from Uniprot synonym to uniprot canonical.

==
?- map_unip_usyn_unip( 'Q60FE2', UniP ).
*/
/*
map_unip_usyn_unip( X, Y ) :-
    bio_db_serve( map_unip_usyn_unip(X,Y) ).
    */

/**  map_unip_ensp_unip( ?EnsP, ?UniP ).

Map predicate from Ensembl proteins to Uniprot proteins.

==
?- map_unip_ensp_unip( 'ENSP00000472020', UniP ).
UniP = 'Q96Q04'.
==
*/
map_unip_ensp_unip( X, Y ) :-
    bio_db:bio_db_serve( map_unip_ensp_unip(X,Y) ).

/**  map_unip_trem_nucs( ?Trem, ?Nucs ).

Map predicate from treMBLE protein to Nucleotide sequence (ENA).
This is an Many to Many relation.

==
?- map_unip_trem_nucs( 'B2RTS4', Nucs ).
Nucs = 'BC140794'.

?- map_unip_trem_nucs( 'B4E273', Nucs ), map_unip_trem_nucs( Trem, Nucs ), write( Trem-Nucs ), nl, fail.

B4E273-AK304141
B4E273-BC143676
A0A0A0MTC0-CH471056
A0A0A0MTJ2-CH471056
A0A0C4DG83-CH471056
A2VDJ0-CH471056
A6NFD8-CH471056
B2RTX2-CH471056
....
==
*/
map_unip_trem_nucs( X, Y ) :-
    bio_db:bio_db_serve( map_unip_trem_nucs(X,Y) ).

/**  map_unip_unip_entz( ?UniP, ?Entz ).

Map predicate from Uniprot proteins to Entrez ids.

==
?- map_unip_unip_entz( 'Q96Q04', Entz ).
Entz = 114783.
==
*/
map_unip_unip_entz( X, Y ) :-
    bio_db:bio_db_serve( map_unip_unip_entz(X,Y) ).
