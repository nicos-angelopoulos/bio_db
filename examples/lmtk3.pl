
:- lib( bio_db ).
:- lib( wgraph ).
:- lib( mtx ).

:- requires( string_symbols_graph/3 ).

:- debug( lmtk3 ).

lmtk3_plot :-
	Gont = 'GO:0010923',
	findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
	Mess = '~a membership: ~w', 
	debug( lmtk3, Mess, [Gont,[Symbs]] ),
	string_symbols_graph( Symbs, G, [min_w(1)] ),
	Mess1 = '~a graph: ~w', 
	debug( lmtk3, Mess1, [Gont,[G]] ),
	Popts = [plotter(igraph),vertex.size=4],
	wgraph_plot( G, Popts ).

lmtk3_ltx :-
    tell('lmtk3_table.tex'),
	map_gont_symb_gont( 'LMTK3', Gont ),
	findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
	map_gont_gont_gonm( Gont, Gonm ),
	sort( Symbs, Oymbs ),
	length( Oymbs, Len ),
	Mess = 'lmtk3 gene ontology term: ~a-~a, with population of: ~d',
    write( Gont ), write( ' & \\verb+' ), write( Gonm ), write( '+ & ' ),
    write( Len ), write( ' \\\\ ' ), nl,
    fail.
lmtk3_ltx :-
    told.

lmtk3_go :-
	map_gont_symb_gont( 'LMTK3', Gont ),
	findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
	map_gont_gont_gonm( Gont, Gonm ),
	sort( Symbs, Oymbs ),
	length( Oymbs, Len ),
	Mess = 'lmtk3 gene ontology term: ~a-~a, with population of: ~d',
	debug( lmtk3, Mess, [Gont,Gonm,Len] ),
	fail.
lmtk3_go.

gene_pops( GeneIn ) :-
	upcase_atom( GeneIn, Gene ),
	findall( row(GoT,GoN,GoP), gene_pops(Gene,GoT,GoN,GoP), Rows ),
	atomic_list_concat( [pop,Gene], '_', Stem ),
	file_name_extension( Stem, csv, MtxF ),
	mtx( MtxF, Rows ).

gene_pops( Gene, Gont, Gonm, Len ) :-
	map_gont_symb_gont( Gene, Gont ),
	findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
	map_gont_gont_gonm( Gont, Gonm ),
	sort( Symbs, Oymbs ),
	length( Oymbs, Len ),
	Mess = '~w gene ontology term: ~a-~a, with population of: ~d',
	debug( lmtk3, Mess, [Gene,Gont,Gonm,Len] ).
	
go_plot( Gont ) :-
	findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
	string_symbols_graph( Symbs, G, min_w(500) ),
	Popts = [plotter(igraph),vertex.size=4,orphan_edge_weight(0.001),
	         stem(Gont)],
	wgraph_plot( G, Popts ).
	
go_length( Gont, Len ) :-
	findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
	length( Symbs, Len ).

