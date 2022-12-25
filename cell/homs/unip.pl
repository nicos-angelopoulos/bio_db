:- module( bio_db_homs_unip, [
                bio_db_homs_unip/0,
                %       + uni prot db
                unip_homs_hgnc_unip/2,
                unip_homs_ensp_unip/2,
                unip_homs_trem_nucs/2,
                unip_homs_unip_entz/2,
                unip_homs_unip_hgnc/2,
                unip_homs_sprt_seqn/2,
                unip_homs_trem_seqn/2
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_unip.

Documentation predicate for Homo sapiens data from Uniprot database.

==
?- lib( & bio_db(homs(unip)) ).
?- [ pack('bio_db/cell/homs/unip') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/24

*/
bio_db_homs_unip.

                unip_homs_hgnc_unip/2,
/**  unip_homs_hgnc_unip( +Hgnc, -UniP ).

Map predicate from HGNC gene ids to Uniprot proteins.

== 
?- unip_homs_hgnc_unip( Hgnc, Unip ).
==

*/
unip_homs_hgnc_unip( X, Y ) :-
    bio_db:bio_db_serve( unip_homs_hgnc_unip(X,Y) ).

/**  unip_homs_unip_hgnc( ?UniP, ?Hgnc ).

Map predicate from Uniprot proteins to HGNC ids.

==
?-  unip_homs_unip_hgnc( 'Q96Q04', Hgnc ).
Hgnc = 19295.

?- unip_homs_unip_hgnc( 'A0A0A0MQW5', Hgnc ).
Hgnc = 19295.

==
*/
unip_homs_unip_hgnc( X, Y ) :-
    bio_db:bio_db_serve( unip_homs_unip_hgnc(X,Y) ).

/**  unip_homs_sprt_seqn( ?Swissprot, ?Seqn ).

Map predicate from Uniprot (Swiprot, the curated parts) to its sequence.

==
?- unip_homs_sprt_seqn( 'Q96Q04', Seqn ).

UniG = 'MPAPGALI....'.

==
*/
unip_homs_sprt_seqn( Sprt, Seqn ) :-
    bio_db:bio_db_serve( unip_homs_sprt_seqn(Sprt,Seqn) ).

/**  unip_homs_trem_seqn( ?Trem, ?Seqn ).

Map predicate from Uniprot (Trembl, the un-curated parts) to its sequence.

==
?- unip_homs_trem_seqn('A0A023HHK9', Seqn).

Seqn = 'MSRSRHARPSRLVRKEDVNKKKKNSQLRKTTKGANKNVASVKTLSPGKLKQLIQERDVKKKTEPKPPVPVRSLLTRAGAARMNLDRTEVLFQNPESLTCNGFTMALRSTSLSRRLSQPPLVVAKSKKVPLSKGLEKQHDCDYKILPALGVKHSENDSVPMQDTQVLPDIETLIGVQNPSLLKGKSQETTQFWTQRVEDSKINIPTHSGPAAEILPGPLEGTRCGEGLFSEETLNDTSGSPKMFAQDTVCAPFPQRVTPKVTSQGNPSIQLEELGSRVESLKLSDSYLDPIKSEHDCYPTSSLNKVIPDLNLRNCLALGGSTSPTSVIKFLLAGSKQATLGAKPDHQEAFEATANQQEVSDTTSFLGQAFGAIPHQWELPGADPVHGEALGETPDLPEIPGAIPVQGEVFGTILDQQETLGMSGSVVPDLPVFLPVPPNPIATFNAPSKWPEPQSTVSYGLAVQGAIQILPLGSGHTPQSSSNSEKNSLPPVMAISNVENEKQVHISFLPANTQGFPLAPERGLFHASLGIAQLSQAGPSKSDRGSSQVSVTSTVHVVNTTVVTMPVPMVSTSSSSYTTLLPTLEKKKRKRCGVCEPCQQKTNCGECTYCKNRKNSHQICKKRKCEELKKKPSVVVPLEVIKENKRPQREKKPKVLKVLRRSSDEEKVLCLVRQRTGHHCPTAVMVVLIMVWDGIPLPMADRLYTELTENLKSYNGHPTDRRCTLNENRTCTCQGIDPETCGASFSFGCSWSMYFNGCKFGRSPSPRRFRIDPSSPLHEKNLEDNLQSLATRLAPIYKQYAPVAYQNQVEYENVARECRLGSKEGRPFSGVTACLDFCAHPHRDIHNMNNGSTVVCTLTREDNRSLGVIPQDEQLHVLPLYKLSDTDEFGSKEGMEAKIKSGAIEVLAPRRKKRTCFTQPVPRSGKKRAAMMTEVLAHKIRAVEKKPIPRIKRKNNSTTTNNSKPSSLPTLGSNTETVQPEVKSETEPHFILKSSDNTKTYSLMPSAPHPVKEASPGFSWSPKTASATPAPLKNDATASCGFSERSSTPHCTMPSGRLSGANAAAADGPGISQLGEVAPLPTLSAPVMEPLINSEPSTGVTEPLTPHQPNHQPSFLTSPQDLASSPMEEDEQHSEADEPPSDEPLSDDPLSPAEEKLPHIDEYWSDSEHIFLDANIGGVAIAPAHGSVLIECARRELHATTPVEHPNRNHPTRLSLVFYQHKNLNKPQHGFELNKIKFEAKEAKNKKMKASEQKDQAANEGPEQSSEVNELNQIPSHKALTLTHDNVVTVSPYALTHVAGPYNHWV' .
==

*/
unip_homs_trem_seqn( Trem, Seqn ) :-
    bio_db:bio_db_serve( unip_homs_trem_seqn(Trem,Seqn) ).

/**  unip_homs_ensp_unip( ?EnsP, ?UniP ).

Map predicate from Ensembl proteins to Uniprot proteins.

==
?- unip_homs_ensp_unip( 'ENSP00000472020', UniP ).
UniP = 'Q96Q04'.
==
*/
unip_homs_ensp_unip( X, Y ) :-
    bio_db:bio_db_serve( unip_homs_ensp_unip(X,Y) ).

/**  unip_homs_trem_nucs( ?Trem, ?Nucs ).

Map predicate from treMBLE protein to Nucleotide sequence (ENA).
This is an Many to Many relation.

==
?- unip_homs_trem_nucs( 'B2RTS4', Nucs ).
Nucs = 'BC140794'.

?- unip_homs_trem_nucs( 'B4E273', Nucs ), unip_homs_trem_nucs( Trem, Nucs ), write( Trem-Nucs ), nl, fail.

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
unip_homs_trem_nucs( X, Y ) :-
    bio_db:bio_db_serve( unip_homs_trem_nucs(X,Y) ).

/**  unip_homs_unip_entz( ?UniP, ?Entz ).

Map predicate from Uniprot proteins to Entrez ids.

==
?- unip_homs_unip_entz( 'Q96Q04', Entz ).
Entz = 114783.
==
*/
unip_homs_unip_entz( X, Y ) :-
    bio_db:bio_db_serve( unip_homs_unip_entz(X,Y) ).
