check_term( Pname, Pname, Arity, Arity ) :- !.
check_term( InPname, Pname, InArity, Arity ) :-
	throw( functor_mismatch(InPname/InArity,Pname/Arity) ).
