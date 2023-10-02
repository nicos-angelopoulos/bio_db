
:- lib(options).
:- lib(pack_errors).

bio_db_cnm_token_defaults([db([]),cnm_ctx([]),org([])]).

/**  bio_db_cnm_token(+Cnm, -Tkn).
     bio_db_cnm_token(+Opts, +Cnm, -Tkn ).
     bio_db_cnm_token(+Cnm, ?Ctx, -Tkn, +Opts).

Converts a column name to a predicate token. 

If a table entry in cnm_token( Cnm, [Ctx], Tkn ) exists, it is returned.
Else, Cnm is returned, but only if it is an atom of length 4. 
Else, an error is invoked.

cnm_token/3 is looked first, before cnm_token/2. C
tx tested against options: Ctx, Org-Db, Org, Db in that order.

Opts
  * db(Db=[])
    source database
  * cnm_ctx(CnmCtx=[])
    context, overriden by Ctx, if ground value is passed in. 
    If CnmCtx is unbound, it is unified to the context found (it can still be unbound at end as many defining contexts do not bind this.)
  * org(Org=[])
    organism

Please note that none of the defaults match any specific definition, values are expected to be passed from caller.

==
?- bio_db_cnm_token( symbol, Tkn ).
Tkn = symb.

?- bio_db_cnm_token( 'Symbolic', Tkn ).
ERROR: bio_db:bio_db_cnm_token/3: Not a valid column name or db predicate token (len=4): Symbolic, (context: _53428).

?- bio_db_cnm_token( cust, Tkn ).
Tkn = cust.

?- bio_db_cnm_token( 'Synonyms', Ctx, Tkn, true ).
Ctx = ncbi,
Tkn = esyn.

?- bio_db_cnm_token('Marker Name', Ctx, Tkn, true).
Ctx = mgim,
Tkn = mnme.


?- bio_db_cnm_token('Marker Name', withdrawn, Tkn, true ).
Tkn = wdra.

?- bio_db_cnm_token('Marker Name', Wdra, Tkn, cnm_ctx(withdrawn) ).
Wdra = withdrawn,
Tkn = wdra.
==

@author nicos angelopoulos
@version  0.1 2023/9/24

*/
bio_db_cnm_token( Cnm, Tkn ) :-
     bio_db_cnm_token( Cnm, _Ctx, Tkn, [] ).

bio_db_cnm_token( Opts, Cnm, Tkn ) :-
     bio_db_cnm_token( Cnm, _Ctx, Tkn, Opts ).

bio_db_cnm_token( Cnm, Ctx, Tkn, Args ) :-
     Self = bio_db_cnm_token,
     options_append( Self, Args, Opts ),
     options( db(Db), Opts ),
     options( cnm_ctx(CnmCtx), Opts ),
     options( org(Org), Opts ),
     bio_db_cnm_token_opts( CnmCtx, Org, Db, Cnm, Ctx, Tkn ),
     !,
     (var(CnmCtx) -> Ctx = CnmCtx; true).
bio_db_cnm_token( Cnm, _Ctx, Tkn, _Opts ) :-
     atom_length( Cnm, 4 ),
     !,
     Tkn = Cnm.
bio_db_cnm_token( Cnm, Ctx, _Tkn, Opts ) :-
     throw( not_a_cnm(Cnm,Ctx), [bio_db:bio_db_cnm_token/3|Opts] ).

bio_db_cnm_token_opts( _CnmCtx, _Org, _Db, Cnm, Ctx, Tkn ) :-
     ground( Ctx ),
     cnm_token( Cnm, Ctx, Tkn ),
     !.
bio_db_cnm_token_opts( CnmCtx, _Org, _Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, CnmCtx, Tkn ),
     !,
     Ctx = CnmCtx.
bio_db_cnm_token_opts( _CnmCtx, Org, Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Org-Db, Tkn ),
     !,
     Ctx = Org-Db.
bio_db_cnm_token_opts( _CnmCtx, Org, _Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Org, Tkn ),
     !,
     Ctx = Org.
bio_db_cnm_token_opts( _CnmCtx, _Org, Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Db, Tkn ),
     !,
     Ctx = Db.
bio_db_cnm_token_opts( _CnmCtx, _Org, _Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Ctx, Tkn ),
     !.

/** cnm_token(?Cnm, +Ctx, ?Tkn ).

Convert between database column names and bio_db tokens.

Ctx is used for disambiguation for identically named Cnm(s).
When there is no need for that, Ctx remains untouched by the call.
Tokens would be unique as they are controlled by bio_db- but not column names (Cnm).

==
?- cnm_token_table(symbol, Ctx, Symb).
Symb = symb.
==

@author nicos angelopoulos
@version  0.1 2023/9/16

*/

cnm_token( Cnm, Tkn ) :-
     cnm_token( Cnm, _, Tkn ).

% human
cnm_token(alias_symbol, hgnc, syno).
cnm_token('Approved Name', _, name).
cnm_token('Approved Symbol', _, symb).
cnm_token('CCDS IDs', _, ccds).
cnm_token('ccds_id', _, ccds).
cnm_token('Chromosome', _, chrb).      % chromosome base eg 2p24.1  % old
cnm_token('ensembl_gene_id', _, ensg ).                   
cnm_token('Ensembl gene', _, ensg ).                   
cnm_token('Ensembl ID + supplied by Ensembl', hgnc, ensg).
cnm_token('Ensembl Protein', _, ensp).                   
cnm_token('Entrez Gene ID', hgnc, 'ncbi-appv').   % fixme: check this is not ncbi, and if not give it something like ncba
cnm_token('Entrez ID', _, ncbi).
cnm_token('entrez_id', _, ncbi).
cnm_token('HGNC ID', _, hgnc).
cnm_token('hgnc_id', _, hgnc).
cnm_token(location, _, chrb).          % chromosome base eg 2p24.1
cnm_token(name, _, name).
cnm_token(prev_symbol, hgnc, prev).
cnm_token(symbol, _, symb).
cnm_token('Symbol', _, symb).
% human.ncbi
cnm_token('RNA_nucleotide_accession.version',_,rnuc).
cnm_token('genomic_nucleotide_accession.version', _, dnuc).
% ncbi (probably not used)
cnm_token('Uni Gene', unig).
cnm_token('RNA Nucleotide', rnuc).
cnm_token('DNA Nucleotide', dnuc).
% hgnc_cname_known( 'Entrez Gene ID (supplied by NCBI)', 'entz-ncbi' ).
% multi
cnm_token(taxon_id, _, taxo).
% vgnc
cnm_token(vgnc_id, _, vgnc).
% mouse
cnm_token('GenBank IDs', mgim, genb).
cnm_token('MGI Accession ID', mgim, mgim).
cnm_token('MGI Marker Accession ID', mgim, mgim).
cnm_token('Marker Name', mgim, mnme ).
cnm_token('Marker Name', withdrawn, wdra ).

cnm_token('Marker Symbol', mgim, mrks).
cnm_token('Marker Synonyms (pipe-separated)', mgim, msyn).
cnm_token('UniProt IDs', mgim, unip).
% across multiple settings
% this is used in mouse, any other ones ?
cnm_token('Synonyms', ncbi, nsyn).

