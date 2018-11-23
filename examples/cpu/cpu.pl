:- module( cpu, [cpu/2,install_dependencies_clean/0] ).

:- ensure_loaded( library(bio_db) ).
:- ensure_loaded( src/times ).
:- debug(cpu).

/** <module> Time tests for bio_db.

Main test is via cpu/2. If you have lib(real) installed is highly recommended you use it.

This is a complex example that exercises all back-ends.

Not for the faint hearted.

==

?- cpu( res_dir_A, true ).

==

Results will be created in directcory res_dir_A. 

If you do not have lib(real) test with

==
?- cpu( res_dir_B, false ).

==

To recretate file gen.txt which is the timings for producing
the backends, do: 

==
?- install_dependencies_clean.
==

*/

/** cpu( +Dir ).
    cpu( +Dir, +PdfBool ).

	Creates plots of cpu comparisons for the 4 bio_db backends.
	All results are place in Dir, which should not exist.

	If PdfBool is present and is true, lib(real) is used 
	along with R library "reshape". to draw the data in cison.csv
	into pdf cpu_backends_blank.pdf  (see gg_plot).

*/
cpu( Dir ) :- 
	cpu( Dir, false ).

cpu( Dir, PdfBool ) :- 
	check_dependencies,
	make_directory( Dir ),
	working_directory( Old, Dir ),
	shell( 'swipl -g "test_iface(prolog)" -l ../src/test_iface.pl' ),
	shell( 'swipl -g "test_iface(berkeley)" -l ../src/test_iface.pl' ),
	shell( 'swipl -g "test_iface(rocks)" -l ../src/test_iface.pl' ),
	shell( 'swipl -g "test_iface(prosqlite)" -l ../src/test_iface.pl' ),
	cison,
	gen_times,
	db_sizes,
	cpu_pdf( PdfBool ),
	working_directory( _,   Old ).

/** install_dependencies_clean.

Run this to time the regeneration of all back ends. Generates/rewrites file gen.txt.

*/
install_dependencies_clean :-
	open( gen.txt, write, Out ),
	install_dependencies_clean( Out ),
	close( Out ).

cpu_pdf( false ) :-
	write( 'Skipping graph creation. Use cpu(Dir,true) to rerun and generate them.' ), nl.
cpu_pdf( true ) :-
	ensure_loaded( '../src/gg_draw' ),
	gg_draw,
	gg_draw_pdf,
	gg_retrieved,
	gg_gen,
	gg_sizes.

check_dependencies :-
	bio_db_interface( Iface, _ ),
	\+ bio_db_info( Iface, edge_string_hs/3, _, _ ),
	!,
	throw( to_run_this_script_install(Iface,edge_string_hs/3) ).
check_dependencies :-
	bio_db_interface( prolog ),
	once( edge_string_hs(_,_,_) ). % ensures the .qlf is saved

install_dependencies :-
	bio_db_interface( Iface ),
	install_dependency( Iface ),
	fail.
install_dependencies :-
	debug( cpu, 'Making sure .qlf is generated', true ),
	bio_db_interface( prolog ),
	once( edge_string_hs(_,_,_) ).

install_dependency( Iface ) :-
	bio_db_info( edge_string_hs/3, Iface ),
	!,
	debug( cpu, 'Interface: ~w already installed', Iface ).
install_dependency( Iface ) :-
	debug( cpu, 'Installing interface: ~w', Iface ),
	( bio_db_install(edge_string_hs/3,Iface) -> true ; throw( installation_failed(Iface) ) ).

install_dependencies_clean( Out ) :-
	String = pack('bio_db_repo/data/graphs/string'),
	absolute_file_name( String, AbsString, [access(exist),file_type(directory)] ),
	bio_db_interface( Iface, _Status ),
	Iface == rocks,
	write( doing(Iface) ), nl,
	install_dependency_clean( Iface, AbsString, Out ),
	write( done(Iface) ), nl,
	fail.
install_dependencies_clean( _Out ).

install_dependency_clean( Iface, String, Out ) :-
	Pname = edge_string_hs,
	debug( cpu, 'Cleaning interface: ~w', Iface ),
	!,
	interface_extension_type( Iface, Ext, OsObjType ),
	file_name_extension( Pname, Ext, Base ),
	directory_file_path( String, Base, Path ),
	( type_exists(OsObjType,Path) -> 
		type_delete(OsObjType,Path),
		( memberchk(Iface,[berkeley,rocks]) ->
			file_name_extension( edge_string_hs_info, Ext, InfoBase ),
			directory_file_path( String, InfoBase, InfoPath ),
			type_delete(OsObjType,InfoPath)
			;
			( Iface == prolog ->
				file_name_extension( Pname, qlf, QlfBase ),
				directory_file_path( String, QlfBase, QlfPath ),
				type_delete(file,QlfPath)
				;
				true
			)
		)
		;
		true
	),
	debug( cpu, 'Stand back, install for: ~w, might take a long time', Iface ),
	stream_property( Err, alias(user_error) ),
	set_stream( Out, alias(user_error) ),
	( Iface == prolog ->
		bio_db_install( Pname, Iface, interactive(false) ),
		(  bio_db:bio_db_pname_source(string,Pname,read,qlf,QlF) ->
			true
			;
			throw( no_qlf )
		),
		delete_file( QlF ),
		debug( cpu, 'timing: ~w', qlf ),
		% write( Out, '% timing: qlf' ), nl( Out ),
		file_name_extension( QStem, qlf, QlF ),
		file_name_extension( QStem, pl,  QplF ),
		time( bio_db:bio_db_load_call(false,Pname,3,prolog,QplF,true) )
		;
		debug( cpu, 'timing: ~w', Iface ),
		% write( Out, '% timing: ' ), write( Out, Iface ), nl( Out ),
		time( bio_db_install(Pname,Iface, interactive(false) ) )
	),
	set_stream( Err, alias(user_error) ).

type_exists( dir, Os) :-
	exists_directory( Os ).
type_exists( file, Os) :-
	exists_file( Os ).

type_delete( dir, Os ) :-
	delete_directory_and_contents( Os ).
type_delete( file, Os ) :-
	delete_file( Os ).

interface_extension_type( berkeley, db, file ).
interface_extension_type( prolog, qlf, file ).
interface_extension_type( prosqlite, sqlite, file ).
interface_extension_type( rocks, rocks, dir ).

/*
install_dependencies_clean :-
	absolute_file_name( pack('bio_db_repo/data/graphs/string/edge_string_hs.db'), Bdb ),
	( exists_file( Bdb ) -> 
		debug( cpu, 'Deleting: ~p', Bdb )
		;
		true
	),
	write( bdb(Bdb) ), nl.
	*/
