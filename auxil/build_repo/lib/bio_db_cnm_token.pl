
:- lib(options).
:- lib(pack_errors).

bio_db_cnm_token_defaults( [db(hgnc),org(human)] ).

/**  bio_db_cnm_token(+Cnm, -Tkn).
     bio_db_cnm_token(+Opts, +Cnm, -Tkn ).
     bio_db_cnm_token(+Cnm, ?Ctx, -Tkn, +Opts).

Converts a column name to a predicate token. 

If a table entry in cnm_token( Cnm, [Ctx], Tkn ) exists, it is returned.
Else, Cnm is returned, but only if it is an atom of length 4. 
Else, an error is invoked.

cnm_token/3 is looked first, before cnm_token/2. Ctx tested against options: Org-Db, org(Org), db(Db) in that order.

==
?- bio_db_cnm_token( symbol, Tkn ).
Tkn = symb.

?- bio_db_cnm_token( 'Symbol', Tkn ).
ERROR: bio_db:bio_db_cnm_token/3: Not a valid column name or db predicate token (len=4): Symbol, (context: _53428).

?- bio_db_cnm_token( cust, Tkn ).
Tkn = cust.

?- bio_db_cnm_token( 'Synonym', Ctx, Tkn, true ).
Ctx = ncbi,
Tkn = esyn.
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
     options( org(Org), Opts ),
     bio_db_cnm_token_opts( Org, Db, Cnm, Ctx, Tkn ),
     !.
bio_db_cnm_token( Cnm, _Ctx, Tkn, _Opts ) :-
     atom_length( Cnm, 4 ),
     !,
     Tkn = Cnm.
bio_db_cnm_token( Cnm, Ctx, _Tkn, Opts ) :-
     throw( not_a_cnm(Cnm,Ctx), [bio_db:bio_db_cnm_token/3|Opts] ).

bio_db_cnm_token_opts( _Org, _Db, Cnm, Ctx, Tkn ) :-
     ground( Ctx ),
     cnm_token( Cnm, Ctx, Tkn ),
     !.
bio_db_cnm_token_opts( Org, Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Org-Db, Tkn ),
     !,
     Ctx = Org-Db.
bio_db_cnm_token_opts( Org, _Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Org, Tkn ),
     !,
     Ctx = Org.
bio_db_cnm_token_opts( _Org, Db, Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Db, Tkn ),
     !,
     Ctx = Db.
bio_db_cnm_token_opts( _Org, _Db, Cnm, Ctx, Tkn ) :-
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
cnm_token(name, _, name).
cnm_token(symbol, _, symb).
cnm_token(symbol, _, symb).
% multi
cnm_token(taxon_id, _, taxo).
% vgnc
cnm_token(vgnc_id, _, vgnc).
% mouse
cnm_token('GenBank IDs', mgim, genb).
cnm_token('MGI Accession ID', mgim, mgim).
cnm_token('MGI Marker Accession ID', mgim, mgim).
cnm_token('Marker Name', mgim, mnme ).
cnm_token('Marker Symbol', mgim, mrks).
cnm_token('Marker Synonyms (pipe-separated)', mgim, msyn).
cnm_token('UniProt IDs', mgim, unip).
% across multiple settings
% this is used in mouse, any other ones ?
cnm_token('Synonym',ncbi,esyn).
