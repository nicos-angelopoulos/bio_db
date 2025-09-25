/** bio_db_source_base_url(+Db, -Url).

Url is the base Url of the database identified by Db.

@author nicos angelopoulos
@version  0:1 2023/09/21
@version  0:2 2025/09/25,  http://birdgenenames.org/cgnc/downloads.jsp?file= -> https://www.birdgenenames.org/downloads.jsp?file=standard

*/
bio_db_source_base_url(     cgnc, 'https://www.birdgenenames.org/downloads.jsp?file=standard').
bio_db_source_base_url(     ense, 'ftp://ftp.ensembl.org/pub/current_gtf/').
bio_db_source_base_url( gont_goa, 'https://geneontology.org/gene-associations/').
bio_db_source_base_url( gont_obo, 'https://purl.obolibrary.org/obo/').
% bio_db_source_base_url(     hgnc, 'https://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/').
% 24.10.16 above doesnot exist, neither does: ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/
% 24.10.16 using:
bio_db_source_base_url(     hgnc, 'https://storage.googleapis.com/public-download-files/hgnc/tsv/tsv/' ).
% bio_db_source_base_url    ncbi, 'https://ftp.ncbi.nih.gov/gene/DATA/GENE_INFO/').
bio_db_source_base_url(     ncbi, 'https://ftp.ncbi.nih.gov/gene/DATA/').
bio_db_source_base_url(ncbi_taxo, 'https://ftp.ncbi.nih.gov/pub/taxonomy/').
bio_db_source_base_url(     mgim, 'http://www.informatics.jax.org/downloads/reports/').
bio_db_source_base_url(     pros, 'https://ftp.expasy.org/databases/prosite/').
bio_db_source_base_url(     reac, 'https://reactome.org/download/current/' ).
bio_db_source_base_url(     strg, 'https://stringdb-downloads.org/download/').
bio_db_source_base_url(     unip, 'https://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/').
bio_db_source_base_url(unip_taxo, 'https://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/taxonomic_divisions/').
bio_db_source_base_url(    useqs, 'https://ftp.ebi.ac.uk/pub/databases/uniprot/current_release/knowledgebase/taxonomic_divisions/').
bio_db_source_base_url(     vgnc, 'https://ftp.ebi.ac.uk/pub/databases/genenames/vgnc/tsv/' ).
