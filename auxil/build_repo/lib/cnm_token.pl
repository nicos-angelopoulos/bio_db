/** cnm_token(?Cnm, +Ctx, Tkn ).

Convert between database column names and bio_db tokens.

Ctx is used for disambiguation for identically named Cnm(s).
When there is no need for that, Ctx remains untouched by the call.
Tokens would be unique as they are controlled by bio_db.

==
?- cnm_token(symbol, Ctx, Symb).
Symb = symb.
==

@author nicos angelopoulos
@version  0.1 2023/9/16

*/

cnm_token(name, _, name).
cnm_token(symbol, _, symb).
cnm_token(taxon_id, _, taxo).
cnm_token(vgnc_id, _, vgnc).
