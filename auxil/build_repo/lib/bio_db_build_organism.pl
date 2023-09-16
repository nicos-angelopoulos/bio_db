% a copy from prolog/bio_db.pl as we don't want to load the module file
% do not edit this file. edit the original and copy-paste here

/** bio_db_organism( ?Org ).

Colloquial name for organisms supported by bio_db.

Human is considered the default organism and returned first.

==
?- bio_db_organism( Org ).
==

@author nicos angelopoulos
@version  0:2 2019/4/8
@version  0:3 2022/12/29,  changed to colloquials and added chicken, were hs and mouse.

*/

bio_db_organism(human).      % defaulty
bio_db_organism(chicken).    % 2022/12/21
bio_db_organism(mouse).
bio_db_organism(multi).      % 2023/09/16
bio_db_organism(pig).        % 2023/05/31

/* bio_db_organism( ?KnownAs, ?Canon ).
   bio_db_organism( ?Known, ?Token, ?Canon ).

Canon is the canonical, colloquial, representation of Known which is either 
a known bio_db organism/1, an alias to one or a organism token.
Token is the token used in bio_db predicate, file and directory names for this organism.

==
?- bio_db_organism(Known,Org),write(Known:Org),nl,fail.
hs:human
gallus:chicken
gallus_gallus:chicken
gg6a:chicken
human:human
chicken:chicken
mouse:mouse
pig:pig
galg:chicken
homs:human
musm:mouse
suss:pig
false.

?- bio_db_organism( human, Org ).
Org = hs.

?- bio_db_organism( KnownAs, hs ).
KnownAs = human ;
KnownAs = hs ;
false.
==

@author nicos angelopoulos
@version  0.2 2019/5/2
@version  0.3 2022/12/25, added /3 version, and added many aliases
@version  0.4 2023/05/31, added organism sus scrofa with alias pig and token suss.

*/
bio_db_organism( Alias, Org ) :-
     bio_db_organism( Alias, _Token, Org ).

bio_db_organism( Alias, Token, Org ) :-
    ( ground(Alias) -> Backtrack = false; Backtrack = true ),
    bio_db_organism_alias( Alias, Org ),
    ( Backtrack == false -> !; true ),
    bio_db_organism_token( Org, Token ).
bio_db_organism( Org, Token, Canon ) :-
    ( ground(Org) -> Backtrack = false; Backtrack = true ),
    bio_db_organism( Org ),
    bio_db_organism_token( Org, Token ),
    ( Backtrack == false -> !; true ),
    Canon = Org.
bio_db_organism( TokenIs, Token, Canon ) :-
    ( ground(TokenIs) -> Backtrack = false; Backtrack = true ),
    bio_db_organism_token( Canon, TokenIs ),
    ( Backtrack == false -> !; true ),
    Token = TokenIs.

bio_db_organism_token(chicken, galg).
bio_db_organism_token(human,   homs).
bio_db_organism_token(mouse,   musm).
bio_db_organism_token(multi,   mult).
bio_db_organism_token(pig,     suss).

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
bio_db_organism_alias( hs, human ).
bio_db_organism_alias( gallus, chicken ).
bio_db_organism_alias( gallus_gallus, chicken ).
bio_db_organism_alias( gg6a, chicken ).
