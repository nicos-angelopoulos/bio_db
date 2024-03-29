
:- use_module( library(lib) ).
:- lib(csv).            % !
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).
:- lib( stoics_lib:en_list/2 ).

map_uniprot_defaults( [   db(unip),
					 destination(maps),
					 ext([]),
					 f_call(=),
                          id_map('HUMAN_9606_idmapping.dat'),
                          interface(prolog),
					 reverse(false)
					    ] ).

% :- lib( options_append/3 ).
% :- lib( gunzip/2 ).
% :- lib( true/2 ).
:- lib(map_predicate_name/4).
% see above,... :- lib( map_predicate_name_stem/3 ).
:- lib( csv_ids_map/6 ). % :- lib( csv_ids_interface_map/9 ).

:- debuc(uniprot).

/** map_uniprot( +Foreign ).
    map_uniprot( +Foreign, +Opts ).
    map_uniprot( +Foreign, ?UniprotCsv, -Written, +Opts ).

Create a map file from Uniprot identifiers to a Foreign table. UniprotCsv allows multiple
calls to map_uniprot/3 to utilise the read Csv.
Written is the list of all files written on.

Options
  * id_map('HUMAN_9606_idmapping.dat.gz')
    file- only used if UniprotCsv is an unbound variable
  * f_call(=)
    call to transform input Foreign accession to storable accession
  * interface(prolog)
    which interfaces to save- a list if more than one required (sqlite,prolog)
  * predicate(Predicate)
    predicate name, default is uniprot_<low_case(Foreign)>
  * stem(Stem)
    stem of the output file(s) (defaults to Predicate
  * destination('.')
    destination directory
  * ext([])
    default depends on Interface (sqlite-> sqlite, prolog -> pl) (can be a list if Interface is one)
  * reverse(Rev=false) 
    reverse the order of 'Protein' and 'Foreign'

==
  ?- uniprot_sqlite_map( 'GeneID' ).
==

@author nicos angelopoulos
@version  0.1 2014/7/2
@see map_predicate_name/4
*/
map_uniprot( Foreign ) :- 
	map_uniprot( Foreign, _, _, [] ).
map_uniprot( Foreign, Opts ) :- 
	map_uniprot( Foreign, _, _, Opts ).
map_uniprot( Foreign, Uniprot, Fouts, Args ) :-
	options_append( map_uniprot, Args, Opts ),
	memberchk( id_map(UniprotFile), Opts ),
	memberchk( f_call(Fcall), Opts ),
	uniprot_load( UniprotFile, Uniprot ),
	findall( row(Prot,FAcc),  (
							member(row(Prot,Foreign,FAccPrv),Uniprot),
							call( Fcall, FAccPrv, FAcc )
						 ),
									PFRows ),
	debuc( uniprot, length, '', pf_rows/PFRows ),
	memberchk( interface(FcePrv), Opts ),
	en_list( FcePrv, Fces ),
	memberchk( destination(Dst), Opts ),
	memberchk( reverse(Rev), Opts ),
	reverse_column_names( Rev, uniprot, Foreign, Cnm1, Cnm2 ),
	maplist( uniprot_cname, [Cnm1,Cnm2], [Tnm1,Tnm2] ),
	map_predicate_name( Tnm1, Tnm2, Pname, Opts ),
	map_predicate_name_stem( Pname, Stem, Opts ),
	output_extensions( Opts, Fces, FExtsPrv ),
	sort( FExtsPrv, FExts ),
	maplist( map_uniprot_interface(Foreign,Pname,Stem,Dst,PFRows,Rev), FExts, Fouts ).

map_uniprot_interface( Foreign, Pname, Stem, Dst, PFRows, Rev, Fce-Ext, Fout ) :-
	map_uniprot_save( Fce, Ext, Foreign, Pname, Stem, Dst, PFRows, Rev, Fout ).

uniprot_load( _Partial, Rows ) :-
	ground( Rows ),
	!.
uniprot_load( Partial, Rows ) :-
	member( Ext, ['',dat,'dat.gz','gz'] ),
	file_name_extension( Partial, Ext, File ),
	exists_file( File ),
	!,
	uniprot_load_file( File, Rows ).
uniprot_load( Partial, Rows ) :-
	absolute_file_name( bio_db_downloads(unip), UnipD ),
	working_directory( Old, UnipD ),
	Old \== UnipD,
	!,
	uniprot_load( Partial, Rows ),
	working_directory( _, Old ).
uniprot_load( Partial, _Rows ) :-
	throw( could_not_locate_uniprot_id_mappings_file(Partial) ). % fixme

uniprot_load_file( File, Rows ) :-
	file_name_extension( Stem, gz, File ),
	!,
	@ gunzip( --keep, --force, File ),
	\+ file_name_extension( _, gz, Stem ), % just in case
	uniprot_load_file( Stem, Rows ),
	delete_file( Stem ).
uniprot_load_file( File, Rows ) :-
	debuc( uniprot, 'Reading UniProt source at:~p', File ),
	csv_read_file( File, Rows, [separator(0'\t)] ).

map_uniprot_save( sqlite, Ext, Foreign, Pname, Stem, Dst, PFRows, Rev, File ) :-
	% fixme: Rev not implemented here
	( Rev == false -> true; throw( unimplemented_rev_flag_in_sqlite_interface(Rev) ) ),
	use_module( library(prosqlite) ),
	use_module( library(db_facts) ),
	os_dir_stem_ext( Dst, Stem, Ext, File ),
	sqlite_connect( File, uniprot, exists(false) ),
	debuc( uniprot, 'Opened sqlite connection to: ~p', File ),
	Create =.. [Pname,uniprot+text,Foreign+text],
	db_create( uniprot, Create ),
	debuc( uniprot, time(start), 'write on file' ),
	findall( _, (member(row(UniP,Fid),PFRows),Assert=..[Pname,UniP,Fid],db_assert(Assert)), _ ),
	debuc( uniprot, time(finish), 'write on file' ),
	sqlite_disconnect( uniprot ).
map_uniprot_save( prolog, Ext, Foreign, Pname, Stem, Dst, PFRows, Rev, File ) :-
	os_dir_stem_ext( Dst, Stem, Ext, File ),
	os_dir_stem_ext( Dst, Stem, '', DstStem ),
	debuc( uniprot, 'Opened prolog output to: ~p', File ),
	findall( U, member(row(U,_),PFRows), Us ),
	findall( N, member(row(_,N),PFRows), Ns ),
	IfcOpts = [to_value_1(=),to_value_2(=)],
	reverse_columns( Rev, 'Uniprot', Foreign, Us, Ns, Cnm1, Cnm2, Clm1, Clm2 ),
	csv_ids_interface_map( prolog, Pname, DstStem, Cnm1, Cnm2, Clm1, Clm2, _PlFile, IfcOpts ).

reverse_column_names( false, Cnm1, Cnm2, Cnm1, Cnm2 ).
reverse_column_names( true, Cnm1, Cnm2, Cnm2, Cnm1 ).

reverse_columns( false, Cnm1, Cnm2, Clm1, Clm2, Cnm1, Cnm2, Clm1, Clm2 ).
reverse_columns(  true, Cnm2, Cnm1, Clm2, Clm1, Cnm1, Cnm2, Clm1, Clm2 ).

map_uniprot_save( pl, Ext, Foreign, Pname, Stem, Dst, PFRows, File ) :-
	map_uniprot_save( prolog, Ext, Foreign, Pname, Stem, Dst, PFRows, File ).

output_extensions( Opts, Fces, Fexts ) :-
	memberchk( ext(ExtPrv), Opts ),
	en_list( ExtPrv, Exts ),
	findall( Fce-Ext, (nth1(N,Fces,Fce),nth_interface_ext(N,Exts,Fce,Ext)), Fexts ).

nth_interface_ext( N, Exts, _Fce, Ext ) :-
	nth1( N, Exts, Ext ),
	!.
nth_interface_ext( _N, _Exts, Fce, Ext ) :-
	interface_extension( Fce, Ext ).

interface_extension( sqlite, sqlite ).
interface_extension( prolog, pl ).
interface_extension( pl, pl ).

uniprot_cname( A, B ) :-
	debuc( uniprot, 'trying ~w', uniprot_cname_known(A, B) ),
	uniprot_cname_known( A, B ),
	!.
uniprot_cname( A, A ).

uniprot_cname_known(  'Ensembl_PRO', ensp ).
uniprot_cname_known(  'uniprot', unip ).
uniprot_cname_known(  'HGNC', hgnc ).
uniprot_cname_known(  'GeneID', ncbi ).
uniprot_cname_known(  'UniGene', unig ).
% mouse
uniprot_cname_known(  'MGI', mgim ).
uniprot_cname_known(  'Gene_Name', symb ).
uniprot_cname_known(  'Gene_Synonym', gyno ).
% chicken
uniprot_cname_known(  'STRING', strp ).
