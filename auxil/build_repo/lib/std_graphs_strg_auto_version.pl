
:- use_module(library(lists)).      % member/2.
:- use_module(library(sgml)).       % load_html/3.

:- lib(options).
:- lib(by_unix).
:- lib(stoics_lib:message_report/3).

std_graphs_strg_auto_version_defaults([iactive(true)]).

/**  std_graphs_strg_auto_version(-Vers, +Opts)

Scrobes the current version off the STRING website.

Opts
  * iactive(Iact=true)
    when =false= pass --no-verbose to the wget call

==
?- std_graphs_strg_auto_version( Vers, true ).
 Sending, name: wget, args: [-O,/tmp/index.html,https://string-db.org], opts:[].
--2023-09-23 23:14:10--  https://string-db.org/
Resolving string-db.org (string-db.org)... 172.66.40.141, 172.66.43.115, 2606:4700:3108::ac42:288d, ...
Connecting to string-db.org (string-db.org)|172.66.40.141|:443... connected.
HTTP request sent, awaiting response... 200 OK
Length: unspecified [text/html]
Saving to: ‘/tmp/index.html’

/tmp/index.html                          [ <=>                                                                 ]  94.46K  --.-KB/s    in 0.06s   

2023-09-23 23:14:11 (1.66 MB/s) - ‘/tmp/index.html’ saved [96722]

% Auto detected version: 12.0
Vers = '12.0'.


?- std_graphs_strg_auto_version( Vers, iactive(false) ).
% Sending, name: wget, args: [--no-verbose,-O,/tmp/index.html,https://string-db.org], opts:[].
2023-09-23 23:14:33 URL:https://string-db.org/ [96722] -> "/tmp/index.html" [1]
% Auto detected version: 12.0
Vers = '12.0'.

==

@author nicos angelopoulos
@version  0.1 2018/11/09
@version  0.2 2023/09/23,  added options (iactive(Iact))

*/
std_graphs_strg_auto_version( VerAtm, Args ) :-

    % load_html( 'https://string-db.org/', Idx, [] ),
    Self = std_graphs_strg_auto_version,
    options_append( Self, Args, Opts ),
    ( options(iactive(false),Opts) ->
          @ wget('--no-verbose','-O','/tmp/index.html','https://string-db.org')
          ;
          @ wget('-O','/tmp/index.html','https://string-db.org')
    ),
    % load_html( 'https://string-db.org/', Idx, [] ),
    load_html( '/tmp/index.html', Idx, [] ),
    % findall( Li, xpath(Idx,//(li),Li), Lis ),
    % Lis = [_,element(li,_,[element(a,_,[VerAtm])])|_],
    % % element(li, [class=last], [element(a, _, ['10.5'])])
    % atomic_list_concat( IntAtms, '.', VerAtm ),
    % maplist( atom_number, IntAtms, Ints ),
    % maplist( integer, Ints ),
    % As of version 11.0:
    Idx = [element(_,_,Subs)],
    Subs = [element(head,[],HeadSubs),_Body], 
    member( element(script, [], [ScriptSub] ), HeadSubs ), 
    atom_concat( Pfx, Mdx, ScriptSub ), 
    atom_concat( _, 'string_database_version_dotted: \'', Pfx ), 
    atom_concat(Left,_Right,Mdx), 
    atom_concat( VerAtm, '\'', Left ), 
    atom_number( VerAtm, _Vers ), 
    debuc( std_graphs_strg, 'Auto detected version: ~a', VerAtm ),
    !.
std_graphs_strg_auto_version( unkonwn ) :-
	Mess1 = 'Please check number of latest version at http://string-db.org',
	Mess2 = '... and run, for example, std_graphs_string(\'10.5\').',
	message_report( Mess1, [], information ),
	message_report( Mess2, [], information ),
    fail.
