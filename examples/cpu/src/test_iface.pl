
:- lib(bio_db).
:- lib(mtx).
:- lib(os_lib).

:- prolog_load_context( directory, Dir ),
   directory_file_path( BaseD, src, Dir ),
   assert( test_dir(BaseD) ).

:- debug( test_iface ).

test_iface_prepare :-
	time( once(edge_string_hs(a,a,_)) -> true; true ),
	true.

test_iface(Iface) :-
	bio_db_interface( Iface ),
	atom_concat( out_, Iface, OutStem ),
	file_name_extension( OutStem, txt, OutF ),
	open( OutF, write, Out ),
	set_prolog_IO( user_input, Out, Out ),
	test_iface_prepare,

	test_call( 100 ), garbage_collect, sleep(1),
	test_call( 200 ), garbage_collect, sleep(1),
	test_call( 300 ), garbage_collect, sleep(1),
	test_call( 400 ), garbage_collect, sleep(1),
	test_call( 500 ), garbage_collect, sleep(1),
	test_call( 600 ), garbage_collect, sleep(1),
	test_call( 700 ), garbage_collect, sleep(1),
	test_call( 800 ), garbage_collect, sleep(1),
	test_call( 900 ), garbage_collect, sleep(1),
	test_call(1000 ),

	close( Out ),
	halt.

test_call( Pop ) :-
	atomic_list_concat( [test,Pop], '_', Stem ),
	os_ext( csv, Stem, File ),
	test_dir( Dir ),
	os_path( Dir, test_sets, AbsDir ),
	os_path( AbsDir, File, TestF ),
	mtx( TestF, Mtx ),
	maplist( arg(1), Mtx, Prots ),
	time( findall(Y-W,(member(Prot,Prots),
	                     edge_string_hs(Prot,Y,W)
					 % portray_clause( t(Prot,Y,W) )
                       ),YWs) ),
	length( YWs, Len ),
	debug( test_iface, 'Length: ~d', Len ).
