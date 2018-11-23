
:- use_module( library(real) ).
:- r_library("reshape").
:- ensure_loaded( lib/gg_lines ).

gg_draw :-
	% mtx_df( "cison.csv", df ),
	df <- data.frame( read.csv("cison.csv") ),
	dfm <- melt( df, variable_name="backend", id="retrieved" ),
	% Gterm = ylab("CPU (Sec)")+xlab("Seek operations")+theme(panel.background=element_blank()),
	Gterm = ylab("CPU (Sec)")+xlab("Seek operations")+theme_bw(),
	Opts = [df(dfm),x(retrieved),y(cpu),group('backend'),gg_term(Gterm)],
	% theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),

	colnames(dfm) <- ["retrieved","backend","cpu"],
	gg_lines( dfm, Opts ).

gg_draw_pdf :-
	<- ggsave( filename="cpu_backends_blank.pdf" ).

gg_retrieved :-
	% mtx_df( "compo.csv", combo ),
	% mtx( "compo.csv", Combo ),
	csv_read_file( "compo.csv", Combo ),
	maplist( arg(2), Combo, [_|Seeks] ),
	combo <- Seeks,
	maplist( arg(1), Combo, [_|Samples] ),
	names(combo) <- Samples,
	<- barplot( combo, xlab="Protein Sampled", ylab="Records retrieved" ),
	<- pdf("cpu_retrieved.pdf"),
	<- barplot( combo, xlab="Proteins Sampled", ylab="Records retrieved" ),
	<- dev.off().

gg_sizes :-
	csv_read_file( 'sizes.csv', CsvSizes ),
	maplist( arg(2), CsvSizes, [_|IfaceExts] ),
	maplist( iface_ext, Ifaces, IfaceExts ),
	maplist( arg(1), CsvSizes, [_|Sizes] ),
	dbszs <- Sizes,
	names(dbszs) <- Ifaces,
	dbszs <- sort(dbszs),
	<- pdf("sizes.pdf"),
	<- barplot( dbszs, xlab="Interface", ylab="Size in Mbs" ),
	<- dev.off().

gg_gen :-
	( exists_file('gen_times.csv') -> true; copy_file('../gen_times.csv','gen_times.csv') ),
	csv_read_file( 'gen_times.csv', GenTimes ),
	maplist( arg(1), GenTimes, [_|Ifaces] ),
	maplist( arg(2), GenTimes, [_|CpuTimes] ),
	maplist( arg(3), GenTimes, [_|AllTimes] ),
	maplist( iface_gen, Ifaces, Gfaces ),
	maplist( psfx_atom('(cpu)'), Gfaces, CpuIfaces ),
	maplist( psfx_atom('(all)'), Gfaces, AllIfaces ),
	kv_compo( CpuTimes, CpuIfaces, Cpus ),
	kv_compo( AllTimes, AllIfaces, Alls ),
	append( Cpus, Alls, Pairs ),
	sort( Pairs, Order ),
	kv_compo( OKs, OVs, Order ),
	tms <- OKs, 
	names(tms) <- OVs,
	<- pdf("gen_times.pdf"),
	<- par(mar=c(8,8,1,1)),
	<- barplot( tms, xlab="", ylab="Generation timings", las=2 ),
	<- title( xlab="Interface", mgp=c(7,1,0) ),
	<- dev.off().

psfx_atom( Postfix, Atom, Postfixed ) :-
	atomic_list_concat( [Atom,Postfix], ' ', Postfixed ).

kv_compo( [], [], [] ).
kv_compo( [K|T], [V|R], [K-V|Pairs] ) :-
	kv_compo( T, R, Pairs ).

iface_gen( qlf, 'QCompile' ).
iface_gen( berkeley, 'Berkeley' ).
iface_gen( prosqlite, 'proSQLite' ).
iface_gen( rocks, 'RocksDB' ).

iface_ext( 'Prolog (zip)', pl.zip ).
iface_ext( 'Prolog', pl ).
iface_ext( 'QCompile', qlf ).
iface_ext( 'proSQLite', sqlite ).
iface_ext( 'Berkeley', db ).
iface_ext( 'RocksDB', rocks ).

/*   add plot of db file size
	and plot of conversion times
	*/
