/* ui_yes_no( +Mess, +Args, +Def, -Reply ).

Mess and Args are message and args that can used as 2,3 arguments
of debug/3, Def is y or n and Reply is true or false.

==
?- ui_yes_no( true, 'Say what', [], y, Reply ).
% Say what (Y/n) ?       <pressed Enter>
% Continuing with: yes
Reply = true.

[debug]  ?- ui_yes_no( true, 'Say what', [], y, Reply ).
% Say what (Y/n) ?       <pressed n>
% Continuing with: no
Reply = false.

[debug]  ?- ui_yes_no( false, 'Say what', [], y, Reply ).
% Say what (Y/n) ?         <no interaction>
% Continuing with: yes
Reply = true.
==

@author nicos angelopoulos
@version  0.2 2016/9/6, added auto flag
@version  0.1 2015/4/28
@see pack(bio_db)
@tbd don't use internals, use print_message instead
*/

ui_yes_no( Ictive, MessIn, Args, Def, Reply ) :-
	ui_yes_no_parenthesis( Def, YesNo ),
	atomic_list_concat( [MessIn,YesNo,'? '], ' ', Mess ),
	phrase('$messages':translate_message(debug(Mess,Args)), Lines),
	print_message_lines(current_output, kind(informational), Lines),
	ui_yes_no_wait( Ictive, Def, Reply, Continue ),
	phrase('$messages':translate_message(debug('Continuing with: ~w',[Continue])), ContLines),
	print_message_lines(current_output, kind(informational), ContLines).

ui_yes_no_wait( false, Def, Reply, Continue ) :-
	!,
	Reply = true,
	ui_yes_def_continue( Def, Continue ).
ui_yes_no_wait( true, Def, Reply, Continue ) :-
	current_prolog_flag( bio_db_ok, true ),
	!,
	atom_codes( Def, [Code|_] ),
	ui_yes_no_reply( Def, Code, Reply, Continue ).
ui_yes_no_wait( true, Def, Reply, Continue ) :-
	get_single_char( Char ),
	ui_yes_no_reply( Def, Char, Reply, Continue ).
	
ui_yes_no_reply( y, Char, Reply, no ) :-
	ui_yes_no_reply_no( Char ),
	!,
	Reply = false.
ui_yes_no_reply( y, _Char, true, yes ) :- !.
ui_yes_no_reply( n, Char, Reply, yes ) :-
	ui_yes_no_reply_yes( Char ),
	!,
	Reply = true.
ui_yes_no_reply( n, _Char, false, no ).

ui_yes_no_reply_yes( 0'y ).
ui_yes_no_reply_yes( 0'Y ).

ui_yes_no_reply_no( 0'n ).
ui_yes_no_reply_no( 0'N ).

ui_yes_no_parenthesis( y, '(Y/n)' ).
ui_yes_no_parenthesis( n, '(y/N)' ).

ui_yes_def_continue( y, yes ).
ui_yes_def_continue( n, no  ).
