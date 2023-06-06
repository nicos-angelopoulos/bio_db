bio_db

An SWI-Prolog pack/library [4] that provides a plethora
of biological data in the form of Prolog predicates. 
The data can reside either in Prolog fact files 
(which are auto-compiled to fast loading .qlf files),
or to back-end databases. The method of storage
is trasparent to the user. 

Installation :

   ?- pack_install(bio_db).

The library itself does not come with any data, these
can be either downloaded en-masse via, 

?- pack_install(bio_db_repo).

Or will be installed on demand at the first invocation of 
each data predicate. For instance if there have been 
no data installed the example below will ask permission
to contact the server and install the specific dataset.

Minimal example:

?- map_homs_symb_hgnc( 'LMTK3', Hgnc ).

The served tables include data from
   * HGNC
   * ENSEMBL
   * NCBI
   * UNIPROT
   * PROSITE
   * STRING
   * GENE ONTOLOGY
   * MGI (mouse)
   * CGNC (chicken)

Organisms

   The data are organised by organism, currently these are human (homs), mouse (musm),
   chicken (galg) and pig (suss).

Links

   [1] project page:  http://stoics.org.uk/~nicos/sware/bio_db
   [2] docs:     http://stoics.org.uk/~nicos/sware/bio_db/doc/html/bio_db.html
   [3] github:   https://github.com/nicos-angelopoulos/bio_db
   [4] SWI-Prolog packs: http://eu.swi-prolog.org/pack/list

Nicos Angelopoulos
---
London, (2016-2023)
