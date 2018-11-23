
:- use_module(library(lib)).
:- lib(bio_db).

/** gene_map_compare :-

    Quantify differences in the data held by 

==
map_hgnc_ensg_hgnc/2
==

and

==
map_ense_ensg_hgnc/2
==

@author nicos angelopoulos
@version  0.1 2017/06/27

*/
gene_map_compare :-
    findall( EnsG, ( map_hgnc_ensg_hgnc(EnsG,Hgnc),
                  \+ map_ense_ensg_hgnc(EnsG,Hgnc)
                            ), NotInEnse ),
    findall( EnsG, ( map_ense_ensg_hgnc(EnsG,Hgnc),
                     \+map_hgnc_ensg_hgnc(EnsG,Hgnc) ),
                        NotInHgnc ),
    findall( EnsG, map_hgnc_ensg_hgnc(EnsG,_HgncH), EnsGsH ),
    sort( EnsGsH, EnsGsHo ),
    length( EnsGsHo, NofGsH ),
    findall( EnsG, map_ense_ensg_hgnc(EnsG,_HgncE), EnsGsE ),
    sort( EnsGsE, EnsGsEo ),
    length( EnsGsEo, NofGsE ),
    length( NotInEnse, NofNIE ),
    length( NotInHgnc, NofNIH ),
    write( not_in_ensemble(NofNIE/NofGsH) ), nl,
    write( not_in_hgnc(NofNIH/NofGsE) ), nl,
    findall( EnsG-(HgncH,HgncE), (
                                    map_hgnc_ensg_hgnc(EnsG,HgncH),
                                    map_ense_ensg_hgnc(EnsG,HgncE),
                                    HgncH \== HgncE
                                 ),
                                    Trips ),
    length( Trips, NofTrips ),
    write( nof_diffs(NofTrips) ), nl,
    write( diffs(Trips) ), nl.
