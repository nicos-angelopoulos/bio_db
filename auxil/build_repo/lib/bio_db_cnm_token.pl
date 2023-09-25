/**  bio_db_cnm_token(?Cnm, ?Tkn).
     bio_db_cnm_token(?Cnm, ?Ctx, ?Tkn ).

Converts a column name to a predicate token. 

If a table entry in cnm_token( Cnm, [Ctx], Tkn ) exists, it is returned.
Else, Cnm is returned, but only if it is an atom of length 4. 
Else, an error is invoked.

==
?- bio_db_cnm_token( symbol, Tkn ).
Tkn = symb.

?- bio_db_cnm_token( 'Symbol', Tkn ).
ERROR: bio_db:bio_db_cnm_token/3: Not a valid column name or db predicate token (len=4): Symbol, (context: _53428).

?- bio_db_cnm_token( cust, Tkn ).
Tkn = cust.
==

@author nicos angelopoulos
@version  0.1 2023/9/24

*/
bio_db_cnm_token( Cnm, Tkn ) :-
     bio_db_cnm_token( Cnm, _Ctx, Tkn ).

bio_db_cnm_token( Cnm, Ctx, Tkn ) :-
     cnm_token( Cnm, Ctx, Tkn ),
     !.
bio_db_cnm_token( Cnm, _Ctx, Tkn ) :-
     atom_length( Cnm, 4 ),
     !,
     Tkn = Cnm.
bio_db_cnm_token( Cnm, Ctx, _Tkn ) :-
     throw( not_a_cnm(Cnm,Ctx), [bio_db:bio_db_cnm_token/3] ).

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

cnm_token(name, _, name).
cnm_token(symbol, _, symb).
cnm_token(taxon_id, _, taxo).
cnm_token(vgnc_id, _, vgnc).
cnm_token('MGI Accession ID', mgim, mgim).
cnm_token('MGI Marker Accession ID', mgim, mgim).
cnm_token('GenBank IDs', mgim, genb).
cnm_token('UniProt IDs', mgim, unip).
cnm_token('Marker Symbol', mgim, mrks).
cnm_token('Marker Synonyms (pipe-separated)', mgim, msyn).
cnm_token('Marker Name', mgim, mnme ).

