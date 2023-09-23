
:- set_prolog_flag(stack_limit, 20 000 000 000).

:- use_module(library(csv)).        % csv_read_file/2.
:- use_module(library(process)).    % process_create/3.
:- use_module(library(readutil)).   % read_line_to_codes/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
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
:- lib(bio_db_source_url/2).
:- lib(url_file_local_date_mirror/3).
:- lib(ens_fa_peptide_gene_rows/2).  % /2, fixme: should be more local

:- debuc(url_file).

% ncbi_repo( 'ftp://ftp.ncbi.nih.gov/gene/DATA/' ).

ncbi_dnload( Loc ) :-
     absolute_file_name( bio_db_build_downloads(ncbi), Loc ),
     os_make_path( Loc, debug(true) ).

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

maps_ncbi_ncbi_gont :-
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
     grep(gene2go, '^9606', gene2go_hs),
     % system( 'grep "^9606" gene2go | cat gene2go_hs' ),
     working_directory( _, Old ).

maps_ncbi_rnuc_symb( Self ) :-
     debuc( by_unix ),
     ncbi_dnload( Dir ),
     % ncbi_repo( Repo ),
     bio_db_source_base_url( ncbi, NcbiRepo ),
     ncbi_humanise_data( gene2accession, Dir, NcbiRepo, Old, HsStem, HsUrl, HsDnDt ),

     file_name_extension( HsStem, tmp, TmpF ),
     @ mv( -f, HsStem, TmpF ),
     open( HsStem, write, HsOut ),

     Cnms = [ 'tax_id','GeneID','status','RNA_nucleotide_accession.version','RNA_nucleotide_gi','protein_accession.version','protein_gi',
              'genomic_nucleotide_accession.version','genomic_nucleotide_gi','start_position_on_the_genomic_accession','end_position_on_the_genomic_accession',
               'orientation','assembly','mature_peptide_accession.version','mature_peptide_gi','Symbol'],
     at_con( Cnms, '\t', HdrLn ),
     write( HsOut, HdrLn ),
     nl( HsOut ),
     close( HsOut ),
     atomic_list_concat( [cat,TmpF,'>>',HsStem], ' ', Cat ),
     debuc( Self, 'Shelling: ~w', [Cat] ),
     shell( Cat ),
     % @ mv( -f, HsStem, HsStem ),

     CIMOpts = [ cnm_transform(ncbi_gene2asseccion_cnms), prefix(ncbi),
                 to_value_1(de_versionise),
                 to_value_2(is_a_symbol),
                 datetime(HsDnDt), source(HsUrl), header(row('RNA Nucleotide','HGNC Symbol'))
                 | Opts
     ],
     RNAnucl = 'RNA_nucleotide_accession.version',
     debuc( Self, 'Csv Map for: ~w vs ~w', [RNAnucl,'Symbol'] ),
     csv_ids_map( HsStem, RNAnucl, 'Symbol', _Csv1, OutF, CIMOpts ),


     DNAOpts = [ cnm_transform(ncbi_gene2asseccion_cnms), prefix(ncbi),
                 to_value_1(de_versionise),
                 to_valuse_2(is_a_symbol),
                 datetime(HsDnDt), source(HsUrl), header(row('DNA Nucleotide','HGNC Symbol')) | Opts
     ],
     GENnucl = 'genomic_nucleotide_accession.version', 
     debuc( Self, 'Csv Map for: ~w vs ~w', [GENnucl,'Symbol'] ),
     csv_ids_map( HsStem, GENnucl, 'Symbol', _Csv2, DNAF, DNAOpts ),
     delete_file( TmpF ),
     os_make_path( maps ),
     @ mv( -f, OutF, maps ),
     @ mv( -f, DNAF, maps ),
     working_directory( _, maps ),
     link_to_bio_sub(ncbi, OutF ),
     link_to_bio_sub(ncbi, DNAF ),
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

ncbi_humanise_data( Stem, Dir, Repo, Old, HsStem, Url, DnDt ) :-
     file_name_extension( Stem, gz, GzF ),
     os_path( Repo, GzF, Url ),
     url_file_local_date_mirror( Url, Dir, [debug(true),interface(wget)] ),
     os_path( Dir, GzF, DnlF ),
     bio_db_dnt_times( DnlF, DnDt, _DnEn ),

     working_directory( Old, Dir ),
     atomic_list_concat( [Stem,hs], '_', HsStem ),
     @ rm( -f, HsStem ),
     @ rm( -f, Stem ),
     @ gunzip( -f, -k, GzF ),
     grep( Stem, '^9606', HsStem ),
    @ rm( -f, Stem ). % fixme: untested

hs_unig( In, In ) :-
     atom_concat( 'Hs.', _, In ).

de_versionise( ProductVersion, Product ) :-
     atomic_list_concat( [Product,_Version], '.', ProductVersion ),
     !.

is_a_symbol( Symb, Symb ) :-
     hgnc:hgnc_homs_symb_hgnc( Symb, _ ),
     !.

ncbi_gene2asseccion_cnms( 'RNA_nucleotide_accession.version', rnuc ).
ncbi_gene2asseccion_cnms( 'genomic_nucleotide_accession.version', dnuc ).
ncbi_gene2asseccion_cnms( 'Symbol', symb ).

std_maps_ncbi_defaults( Defs ) :-
                                   Defs = [ db(ncbi),
                                            debug(true),
                                            debug_url(false),
                                            iactive(true),
                                            ncbi_base(ncbi),
                                            ncbi_genes_file('gene2ensembl.gz')
                                          ].

/** std_maps_ncbi(+Opts).

Download latest NCBI gene to ensembl map file and convert it to a few standard maps.

  * debug(Dbg=true)
    informational, progress messages
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/2)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * obo_base(OboB=gont_obo)
    the url base for the obo download
  * obo_file(OboF='go.obo')
    the file name for the obo download

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
     ncbi_dnload( NcbiD ),

     % Url = 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz',
     options( [ncbi_base(NcbiB),ncbi_genes_file(GnsF),debug_url(Ubg)], Opts ),
     Upts = [url_base(NcbiB),url_file(GnsF),debug(Ubg)],
     bio_db_source_url( Url, Upts ),
     url_file_local_date_mirror( Url, NcbiD, [interface(wget)|Opts] ),
     % file_base_name( Url, RemB ),
     working_directory( Old, NcbiD ),
     MapsD = maps,
     make_directory_path( MapsD ),
     directory_file_path( MapsD, GnsF, ToP ),
     copy_file( GnsF, ToP ),
     bio_db_dnt_times( GnsF, DnDt, _DnEn ),
     working_directory( _ParentD, MapsD ),
     @ gunzip( GnsF ),
     file_name_extension( RemS, gz, GnsF ),
     std_maps_ncbi( Self, RemS, Url, DnDt ),
     delete_file( RemS ),
     maps_ncbi_rnuc_symb( Self ),
     % maps_ncbi_unig_ncbi,  % unigene is no longer maintained as of Feb.2019
     working_directory( _, Old ).

std_maps_ncbi( Self, File, Url, DnDt ) :-
     TsvOpts = [match_arity(false),separator(0'\t)],
     csv_read_file( File, Csv, TsvOpts ),
     Csv = [_Comment|Rows],
     New = [row(tax_id,ncbi,ensg,nucl_acc,ensr,prot_acc,ensp)|Rows],
     % GEnsGF = entrez_gene_id_ensg.pl,
     % csv_filter_by_column( New, tax_id, =(9606), HS ),
     mtx_column_values_select( New, tax_id, 9606, HS, _, true ),
    debuc( Self, length, hs_len/HS ),
     Lens = [prefix(ncbi),to_value_1(pos_integer),to_value_2(pfx_by('ENS')),datetime(DnDt),source(Url)|Opts],
     Rens = [prefix(ncbi),to_value_2(pos_integer),to_value_1(pfx_by('ENS')),datetime(DnDt),source(Url)|Opts],
     csv_ids_map( File, ncbi, ensg, HS, GEnsGF, [header(row('Entrez ID','Ensembl Gene'))|Lens] ),
     csv_ids_map( File, ensg, ncbi, HS, EnsGGF, [header(row('Ensembl Gene','Entrez ID'))|Rens] ),
     % need to ensure prots are of ENSP  there are - in some entries
     Lenp = [prefix(ncbi),to_value_1(pos_integer),to_value_2(pfx_by_de_v('ENS')),datetime(DnDt),source(Url)],
     append( Lenp, Opts, ALenp ),
     csv_ids_map( File, ncbi, ensp, HS, GEnsPF, [header(row('Entrez ID','Ensembl Protein'))|ALenp] ),
     Renp = [prefix(ncbi),to_value_2(pos_integer),to_value_1(pfx_by_de_v('ENS')),datetime(DnDt),source(Url)],
     append( Renp, Opts, ARenp ),
     csv_ids_map( File, ensp, ncbi, HS, EnsPGF, [header(row('Ensembl Protein','Entrez ID'))|ARenp] ),
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

grep(File, Pattern, OutF) :-
        process_create(path(grep), [ Pattern, file(File) ],
                       [ stdout(pipe(Out))
                       ]),
        % read_lines(Out, Lines).
        open( OutF, write, Write ),
        write_lines_out(Out, Write),
        close( Write ).

write_lines_out(Out, Write) :-
        read_line_to_codes( Out, Line1 ),
        write_lines(Line1, Out, Write ).

write_lines(end_of_file, _, _) :- !.
write_lines(Codes, Out, Write) :-
        atom_codes(Line, Codes),
        write( Write, Line ), nl( Write ),
        read_line_to_codes(Out, Line2),
        write_lines(Line2, Out, Write).

ncbi_cname_known( 'HGNC Symbol', symb ).
ncbi_cname_known( 'Ensembl Gene', ensg ).
ncbi_cname_known( 'Ensembl Protein', ensp ).
ncbi_cname_known( 'Entrez ID', ncbi ).
ncbi_cname_known( 'Uni Gene', unig ).
ncbi_cname_known( 'RNA Nucleotide', rnuc ).
ncbi_cname_known( 'DNA Nucleotide', dnuc ).
