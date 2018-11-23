
cison :-    % create cison.csv and compo.csv
	retrieved( R ),
	times( out_berkeley.txt, [_|Bdb] ),
	times( out_prolog.txt, [_|Pl] ),
	times( out_prosqlite.txt, [_|Sqlite] ),
	times( out_rocks.txt, [_|Rocks] ),
	findall( Row, ( nth1(N,Bdb,Nbdb),
	                nth1(N,Pl,Npl),
				 nth1(N,Sqlite,Nsqlite),
				 nth1(N,Rocks,Nrocks),
				 nth1(N,R,Nr),
				 Row =.. [row,Npl,Nbdb,Nsqlite,Nrocks,Nr]
			   ),
			   	Rows ),
	Hdr =.. [hdr,prolog,berkeley,sqlite,rocks,retrieved],
	csv_write_file( cison.csv, [Hdr|Rows] ),
	directory_files( '../test_sets', Files ),
	test_files_samples( Files, Samples ),
	sort( Samples, Order ),
	length( Order, Length ),
	length( R, Length ),
	findall( CompoRow,  (nth1(N1,Order,Pop),nth1(N1,R,R1),CompoRow=..[row,Pop,R1]), CompoRows ),
	csv_write_file( 'compo.csv', [row('sample','retrieved')|CompoRows] ).

db_sizes :-
	% Hs   = pack(bio_db_repo/data/graphs/string/edge_string_hs.pl),
	Hs   = pack(bio_db_repo/data/graphs/string),
	Opts = [file_type(directory),access(exist)],
	absolute_file_name( Hs, Dir, Opts ),
	working_directory( Here, Dir ),
	Pfx = 'du -h -s -L -D ',
	atomic_list_concat( [Pfx,'* >',Here,'/sizes.txt'], '', Shell ),
	shell( Shell ),
	/*
     setup_call_cleanup(
            % process_create(path(du), ['-h',-s -L -D',file('.')], [stdout(pipe(Out))] ),
            process_create(path(du), ['-h','-s','-L','-D','\\*'], [stdout(pipe(Out))] ),
            read_lines(Out, Lines),
            close(Out) ),
	maplist( writeln, Lines ),
	*/
	working_directory( _, Here ),
	open( sizes.txt, read, In ),
	read_line_to_codes( In, Line ),
	db_sizes_stream( Line, In, Sizes ),
	csv_write_file( 'sizes.csv', [hdr('size','interface')|Sizes] ),
	true.

db_sizes_stream( end_of_file, _In, [] ) :- !.
db_sizes_stream( Line, In, Sizes ) :-
	break_list_on_list( Line, `edge_string_hs.`, Left, Right ),
	atom_codes( Ext, Right ),
	% fixme: check for known ext here if to exclude .pl.zip ?
	!,
	once( append(SizeCs,[0'\t|_],Left) ),
	reverse( SizeCs, RevSizeCs ),
	db_size_codes( RevSizeCs, Size ),
	read_line_to_codes( In, NewLine ),
	Sizes = [row(Size,Ext)|Tizes],
	db_sizes_stream( NewLine, In, Tizes ).
db_sizes_stream( _Line, In, Sizes ) :-
	read_line_to_codes( In, NewLine ),
	db_sizes_stream( NewLine, In, Sizes ).

db_size_codes( [0'M|Mcs], Size ) :-
	!,
	reverse( Mcs, SizeCs ),
	number_codes( Size, SizeCs ).
db_size_codes( [0'G|Gcs], Size ) :-
	reverse( Gcs, SizeCs ),
	number_codes( Mize, SizeCs ),
	Size is Mize * 1000.
db_size_codes( [0'K|Kcs], Size ) :-
	reverse( Kcs, SizeCs ),
	number_codes( Kize, SizeCs ),
    Size is Kize / 1000.

read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).

gen_times :-    % create gen_times.csv and db_sizes.csv
	open( '../gen.txt', read, In ),
	read_line_to_codes( In, Line ),
	gen_times_stream( Line, In, GenTimes ),
	close( In ),
	csv_write_file( gen_times.csv, [hdr(iface,cpu_times,all_times)|GenTimes] ).

gen_times_stream( end_of_file, _In, [] ) :- !.
gen_times_stream( Line, In, GenTimes ) :-
	append( `% timing: `, IfaceCs, Line ),
	atom_codes( Iface, IfaceCs ),
	read_line_to_codes( In, NxtLine ),
	gen_times_stream_cpu_all( NxtLine, In, Cpu, All ),
	read_line_to_codes( In, NewLine ),
	GenTimes = [row(Iface,Cpu,All)|TenTimes],
	gen_times_stream( NewLine, In, TenTimes ).

gen_times_stream_cpu_all( Line, _In, Cpu, All ) :-
	break_list_on_list( Line, `inferences, `, _Pfx, Right ),
	!,
	break_list_on_list( Right, ` CPU in `, CpuCs, Right2 ),
	break_list_on_list( Right2, ` seconds `, AllCs, _Right3 ),
	maplist( number_codes, [Cpu,All], [CpuCs,AllCs] ).
gen_times_stream_cpu_all( _Line, In, Cpu, All ) :-
	read_line_to_codes( In, NewLine ),
	gen_times_stream_cpu_all( NewLine, In, Cpu, All ).

times( File, Times ) :-
	open( File, read, In ),
	read_line_to_codes( In, Line ),
	times_stream( Line, In, Times ),
	write( -File ), nl,
	maplist( writeln, Times ),
	close( In ).

times_stream( end_of_file, _In, Times ) :- !, Times = [].
times_stream( Line, In, Times ) :-
	times_line( Line, Times, TTimes ),
	read_line_to_codes( In, NewLine ),
	times_stream( NewLine, In, TTimes ).

times_line( Line, Times, Tail ) :-
	break_list_on_list( Line, `inferences, `, _Pfx, Right ),
	!,
	break_list_on_list( Right, ` CPU`, Left, _Psfx ),
	number_codes( Time, Left ),
	Times = [Time|Tail].
times_line( _Line, Tail, Tail ).

test_files_samples( [], [] ).
test_files_samples( [File|Fs], Samples ) :-
	write( file(File) ), nl,
	( (atom_concat(test_,_,File),file_name_extension(_Stem,csv,File)) ->
			directory_file_path( '../test_sets', File, Path ),
			csv_read_file( Path, Rows ),
			length( Rows, FSamples ),
			Samples = [FSamples|Tamples]
			;
			Tamples = Samples
	),
	test_files_samples( Fs, Tamples ).

break_list_on_list( [X|Xs], [X|Ys], [], Rs ) :-
	append( Ys, Rs, Xs ),
	!.
break_list_on_list( [X|Xs], Ys, [X|Ls], Rs ) :-
	break_list_on_list( Xs, Ys, Ls, Rs ).

retrieved( R ):-  % checks that are all identical
	retrieved( out_prolog.txt, R ),
	retrieved( out_berkeley.txt, R ),
	retrieved( out_rocks.txt, R ),
	retrieved( out_prosqlite.txt, R ).

retrieved( File, R ) :-
	open( File, read, In ),
	read_line_to_codes( In, Line ),
	retrieved_stream( Line, In, R ),
	close( In ).

retrieved_stream( end_of_file, _In, [] ) :- !.
retrieved_stream( Line, In, R ) :-
	retrieved_line( Line, R, Rtail ),
	read_line_to_codes( In, NewLine ),
	retrieved_stream( NewLine, In, Rtail ).

retrieved_line( Line, R, Tail ) :-
	append( `% Length: `, Codes, Line ),
	!,
	number_codes( Numb, Codes ),
	R = [Numb|Tail].
retrieved_line( _Line, R, R ).
