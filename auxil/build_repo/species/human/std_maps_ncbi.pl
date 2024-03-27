
:- set_prolog_flag(stack_limit, 20 000 000 000).

:- use_module(library(csv)).        % csv_read_file/2.
:- use_module(library(process)).    % process_create/3.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).
:- lib(stoics_lib:prefix_atom/2).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')).

% local libs & sources
:- lib(de_semi/3).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(ncbi_species_grep/3).
:- lib(ens_fa_peptide_gene_rows/2).  % /2, fixme: should be more local
:- lib(url_file_local_date_mirror/3).

/*
ncbi_dnload( Loc ) :-
     absolute_file_name( bio_db_build_downloads(ncbi), Loc ),
     os_make_path( Loc, debug(true) ).
     */

% maps_ncbi_ensp_ensg.
%
% This is a later addition.
% See ens_fa_peptide_gene_rows..pl
%
maps_ncbi_ensp_ensg :-
     % Dir = '/usr/local/users/nicos/work/db/data/ncbi',
     fixme,
     ncbi_dnload( Dir ),
     EnsF= 'Homo_sapiens.GRCh38.pep.all.fa',
     working_directory( Old, Dir ),
     ens_fa_peptide_gene_rows( EnsF, EnsRows ),
     csv_ids_map( _CsvF, ensp, ensg, EnsRows, OutF, [prefix(ncbi),header(row('Ensembl Protein','Ensembl Gene'))] ),
     link_to_bio_sub(ncbi, OutF ),
     working_directory( _, Old ).

% fixme: this is not in the loop ? either fix or remove
maps_ncbi_ncbi_gont( Opts ) :-
     % Dir = '/usr/local/users/nicos/work/db/data/ncbi',
     ncbi_dnload( Dir ),
     ncbi_repo( Repo ),
     os_path( Repo, 'gene2go.gz', Url ),
     url_file_local_date_mirror( Url, Dir, [debug(true),interface(wget)] ),
     working_directory( Old, Dir ),
     @ rm( -f, gene2go_hs ),
     @ rm( -f, gene2go ),
     @ gunzip( -f, -k, 'gene2go.gz' ),
     % debuc( by_unix ),
     ncbi_species_grep( gene2go, _HsStem, Opts ),  % pass sep(tab)
     % os_grep_mtx(gene2go, '^9606', gene2go_hs, true ),
     % system( 'grep "^9606" gene2go | cat gene2go_hs' ),
     working_directory( _, Old ).

maps_ncbi_rnuc_symb( Self, DnDir, Opts ) :-
     debuc( by_unix ),
     bio_db_source_base_url( ncbi, NcbiRepo ),
     ncbi_humanise_data( gene2accession, DnDir, NcbiRepo, Old, HsStem, HsUrl, HsDnDt, Opts ),
     CIMOpts = [ db(ncbi),
                 to_value_1(de_versionise), to_value_2(is_a_symbol),
                 datetime(HsDnDt), source(HsUrl), 
                 header(row('RNA Nucleotide','HGNC Symbol'))
                 | Opts
     ],
     RNAnucl = 'RNA_nucleotide_accession.version',
     debuc( Self, 'Csv Map for: ~w vs ~w', [RNAnucl,'Symbol'] ),
     csv_ids_map( HsStem, RNAnucl, 'Symbol', _Csv1, OutF, CIMOpts ),


     DNAOpts = [ db(ncbi),
                 to_value_1(de_versionise), to_value_2(is_a_symbol),
                 datetime(HsDnDt), source(HsUrl), 
                 header(row('DNA Nucleotide','HGNC Symbol')) 
                 | Opts
     ],
     GENnucl = 'genomic_nucleotide_accession.version', 
     debuc( Self, 'Csv Map for: ~w vs ~w', [GENnucl,'Symbol'] ),
     csv_ids_map( HsStem, GENnucl, 'Symbol', _Csv2, DNAF, DNAOpts ),
     NcbiSymbOpts = [ db(ncbi),
                 to_value_1(pos_integer), to_value_2(is_a_symbol),
                 datetime(HsDnDt), source(HsUrl), 
                 header(row(ncbi,symbol)) 
                 | Opts
     ],
     csv_ids_map( HsStem, 'GeneID', 'Symbol', _Csv3, NcbiSymbF, NcbiSymbOpts ),
     % delete_file( TmpF ),
     os_make_path( maps ),
     @ mv( -f, OutF, maps ),
     @ mv( -f, DNAF, maps ),
     @ mv( -f, NcbiSymbF, maps ),
     working_directory( _, maps ),
     link_to_bio_sub(ncbi, OutF ),
     link_to_bio_sub(ncbi, DNAF ),
     link_to_bio_sub(ncbi, NcbiSymbF ),
     working_directory( _, Old ).

/** %unigene is no longer maintained as of Feb.2019
maps_ncbi_unig_ncbi :-
     ncbi_dnload( Dir ),
     ncbi_repo( Repo ),
     os_path( Repo, 'gene2unigene', Url ),
     url_file_local_date_mirror( Url, Dir, [debug(true),interface(wget)] ),
     working_directory( Old, Dir ),
     bio_db_dnt_times( 'gene2unigene', UgDnDt, _DnEn ),

     csv_read_file( gene2unigene, [_|Csv], [separator(0'\t),match_arity(false)] ),
     Hdr = row(ncbi,unig),
     MOpts = [prefix(ncbi),to_value_1(hs_unig),datetime(UgDnDt),source(Url),header(row('Uni Gene','Entrez ID'))], 
     csv_ids_map( _, 'unig', 'ncbi', [Hdr|Csv], OutF, MOpts ),
     os_make_path( maps ),
     @ mv( -f, OutF, maps ),
     working_directory( _, maps ),
     link_to_bio_sub( ncbi, OutF ), 
     working_directory( _, Old ).
*/

ncbi_humanise_data( Stem, Dir, Repo, Old, HsStem, Url, DnDt, Opts ) :-
     file_name_extension( Stem, gz, GzF ),
     os_path( Repo, GzF, Url ),
     url_file_local_date_mirror( Url, Dir, [debug(true),interface(wget)|Opts] ),
     os_path( Dir, GzF, DnlF ),
     bio_db_dnt_times( DnlF, DnDt, _DnEn ),
     working_directory( Old, Dir ),
     % atomic_list_concat( [Stem,hs], '_', HsStem ),
     % @ rm( -f, HsStem ),
     @ rm( -f, Stem ),
     @ gunzip( -f, -k, GzF ),
     ncbi_species_grep( Stem, HsStem, Opts ),
     @ rm( -f, Stem ).

hs_unig( In, In ) :-
     atom_concat( 'Hs.', _, In ).

de_versionise( ProductVersion, Product ) :-
     atomic_list_concat( [Product,_Version], '.', ProductVersion ),
     !.

is_a_symbol( Symb, Symb ) :-
     hgnc:hgnc_homs_symb_hgnc( Symb, _ ),
     !.

std_maps_ncbi_defaults( Defs ) :-
                                   Defs = [ db(ncbi),
                                            debug(true),
                                            debug_fetch(true),
                                            debug_url(false),
                                            iactive(true),
                                            ncbi_genes_file('gene2ensembl.gz'),
                                            org(human),
                                            sep(tab)
                                          ].

/** std_maps_ncbi(+Opts).

Download latest NCBI gene to ensembl map file and convert it to a few standard maps.

Opts
  * db(Db)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * ncbi_genes_file(GnsF='')
    the url base for the genes download
  * org(Org=human)
    organism
  * sep(tab)
    the download files are in tsv format

==
?- std_maps_ncbi([]).
==
@author nicos angelopoulos
@version  0.1 2014/7/23
@version  0.2 2022/12/26, entz-> ncbi, url via wget, csv without R
@version  0.3 2023/9/22,  move url locations to options

*/
std_maps_ncbi( Args ) :-
     Self = std_maps_ncbi,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     % load necessary data that has already been generated
     ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/hgnc_homs_symb_hgnc')),
     build_dnload_loc( Self, DnDir, Opts ),
     bio_db_source_url( Url, [ncbi_genes_file-url_file,debug_url-debug], Opts ),
     options( debug_fetch(Fbg), Opts ),
     url_file_local_date_mirror( Url, DnDir, [debug(Fbg),interface(wget),dnld_file(GnsF)|Opts] ),
     % file_base_name( Url, RemB ),
     working_directory( Old, DnDir ),
     MapsD = maps,
     make_directory_path( MapsD ),
     directory_file_path( MapsD, GnsF, ToP ),
     copy_file( GnsF, ToP ),
     bio_db_dnt_times( GnsF, DnDt, _DnEn ),
     working_directory( _ParentD, MapsD ),
     @ gunzip( GnsF ),
     file_name_extension( RemS, gz, GnsF ),
     ncbi_species_grep( RemS, HsStem, [hdr(row(tax_id,ncbi,ensg,nucl_acc,ensr,prot_acc,ensp)),sep(tab)|Opts] ),
     std_maps_ncbi_1( Self, HsStem, Url, DnDt, Opts ),
     delete_file( RemS ),
     maps_ncbi_rnuc_symb( Self, DnDir, Opts ),
     % maps_ncbi_unig_ncbi,  % unigene is no longer maintained as of Feb.2019
     working_directory( _, Old ).

std_maps_ncbi_1( Self, File, Url, DnDt, Opts ) :-
     mtx( File, Mtx, sep(tab) ),
     debuc( Self, length, hs_len/Mtx ),
     Lens = [prefix(ncbi),to_value_1(pos_integer),to_value_2(pfx_by('ENS')),datetime(DnDt),source(Url)|Opts],
     Rens = [prefix(ncbi),to_value_2(pos_integer),to_value_1(pfx_by('ENS')),datetime(DnDt),source(Url)|Opts],
     csv_ids_map( File, ncbi, ensg, Mtx, GEnsGF, [header(row('Entrez ID','Ensembl Gene'))|Lens] ),
     csv_ids_map( File, ensg, ncbi, Mtx, EnsGGF, [header(row('Ensembl Gene','Entrez ID'))|Rens] ),
     % need to ensure prots are of ENSP  there are - in some entries
     Lenp = [prefix(ncbi),to_value_1(pos_integer),to_value_2(pfx_by_de_v('ENS')),datetime(DnDt),source(Url)],
     append( Lenp, Opts, ALenp ),
     csv_ids_map( File, ncbi, ensp, Mtx, GEnsPF, [header(row('Entrez ID','Ensembl Protein'))|ALenp] ),
     Renp = [prefix(ncbi),to_value_2(pos_integer),to_value_1(pfx_by_de_v('ENS')),datetime(DnDt),source(Url)],
     append( Renp, Opts, ARenp ),
     csv_ids_map( File, ensp, ncbi, Mtx, EnsPGF, [header(row('Ensembl Protein','Entrez ID'))|ARenp] ),
     % Nens = [to_value_1(pos_integer),datetime(DnDt),source(Url)|Opts],
     % csv_ids_map( File, ncbi, 'Symbol', Mtx, NcbiSymbF, [header(row(ncbi,symbol))|Nens] ),
     maplist( link_to_bio_sub(ncbi), [GEnsGF,EnsGGF,GEnsPF,EnsPGF] ).

pos_integer( Numb, Numb ) :-
     integer( Numb ),
     !,
     Numb > 0.
pos_integer( Atom, Numb ) :-
     atom_number( Atom, Numb ),
     !,
     integer( Numb ), 
     Numb > 0.

pfx_by_de_v( Pfx, Full, UnV ) :-
     prefix_atom( Pfx, Full ),
    ( atomic_list_concat([UnV,_],'.',Full) ->
        true
        ;
        UnV = Full
    ).

pfx_by( Pfx, Full, Full ) :-
     prefix_atom( Pfx, Full ).

ncbi_cname_known( 'HGNC Symbol', symb ).
ncbi_cname_known( 'Ensembl Gene', ensg ).
ncbi_cname_known( 'Ensembl Protein', ensp ).
ncbi_cname_known( 'Entrez ID', ncbi ).
ncbi_cname_known( 'Uni Gene', unig ).
ncbi_cname_known( 'RNA Nucleotide', rnuc ).
ncbi_cname_known( 'DNA Nucleotide', dnuc ).
