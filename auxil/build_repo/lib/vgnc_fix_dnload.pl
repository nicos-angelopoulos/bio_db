% I reported that in September, but never heard from them. I think it is fixed now (26.06.16).
vgnc_fix_dnload( Self, TxtF, [Naulty|Rows] ) :-
     mtx( TxtF, Mtx, [match(false),sep(tab)] ),
     Mtx = [Faulty,Row|Rows],
     functor( Faulty, _, Farity ),
     functor( Row, _, Rarity ),
     ( Farity =:= Rarity ->   % this will always be false, as the caller gzip afresh...
          debuc( Self, 'File: ~p had already been corrected.', [TxtF] )
          ;
          debuc( Self, 'Correcting header of file: ~p. Arities were ~d/~d .', [TxtF,Farity,Rarity] )
     ),
     arg_add( -2, Faulty, '', Naulty ),
     mtx( TxtF, [Naulty|Rows], sep(tab) ).
