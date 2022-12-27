% a copy from prolog/bio_db.pl as we don't want to load the module file

/** bio_db_organism( -Org ).

Organisms supported by bio_db.

hs will always be returned first, and is considered the default organism
when none is given explicitly (eg via a predicate's Options).

==
?- bio_db_organism( Org ).
==

@author nicos angelopoulos
@version  0:2 2019/4/8

*/

bio_db_organism(hs).      % defaulty
bio_db_organism(gallus).  % 2022/12/21
bio_db_organism(mouse).

/* bio_db_organism( ?Known, ?Canon ).
   bio_db_organism( ?Known, ?Token, ?Canon ).

Canon is the canonical representation of Known which is either 
a known bio_db organism/1 or an alias (bio_db_organism/2) to one.
Token is the token used in predicate names for this organism.

Please note this used to have the semantics of bio_db_organism_alias/2
(until 19.05.02).

==
?- bio_db_organism( Known, Org ).
Known = human,
Org = hs ;
Known = Org, Org = hs ;
Known = Org, Org = mouse.

?- bio_db_organism( human, Org ).
Org = hs.

?- bio_db_organism( KnownAs, hs ).
KnownAs = human ;
KnownAs = hs ;
false.
==

@author nicos angelopoulos
@version  0.2 2019/5/2
@version  0.3 2022/12/25, added /3 version

*/
bio_db_organism( Alias, Org ) :-
     bio_db_organism( Alias, _Token, Org ).

bio_db_organism( Alias, Token, Org ) :-
    ( ground(Alias) -> Backtrack = false; Backtrack = true ),
    bio_db_organism_alias( Alias, Org ),
    ( Backtrack == false -> !; true ),
    bio_db_organism_token( Org, Token ).
bio_db_organism( Org, Token, Canon ) :-
    bio_db_organism( Org ),
    bio_db_organism_token( Org, Token ),
    !,
    Canon = Org.
bio_db_organism( Token, Token, Canon ) :-
    bio_db_organism_token( Canon, Token ).

bio_db_organism_token(gallus, galg).
bio_db_organism_token(hs, homs).
bio_db_organism_token(mouse, musm).

/** bio_db_organism_alias( ?Alias, -Org ).

Alias is a known and supported alternative name for the canonical Org name for an 
organism.

==
?- bio_db_organism_alias( human, hs ).
true.
==

Note this used to be bio_db_organism/2 which has now (19.05.02) changed.

@author nicos angelopoulos
@version  0:1 2019/5/2
@version  0:2 2022/12/20, gallus also known as chicken and gallus_gallus
*/
bio_db_organism_alias( human, hs ).
bio_db_organism_alias( chicken, gallus ).
bio_db_organism_alias( gallus_gallus, gallus ).
bio_db_organism_alias( gg6a, gallus ).

