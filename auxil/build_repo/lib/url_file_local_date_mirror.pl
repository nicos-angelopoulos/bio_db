
:- use_module( library(listing) ). % portray_clause/2.
:- use_module( library(lib) ).

:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).

:- lib(stoics_lib:url_file/3).
:- lib(stoics_lib:get_datetime/1).
:- lib(stoics_lib:date_two_digit_dotted/1).
:- lib(stoics_lib:datime_two_digit_dotted/1).

url_file_local_date_mirror_defaults( Args, Defs ) :-
                                  (memberchk(iactive(false),Args) -> Verb=false; Verb=true),
                                  Defs = [  date(postfix), 
                                            debug(false),
                                            file(_), 
                                            iactive(true),
                                            interface(prolog),
                                            link_stem(true),
                                            make_path(true), 
                                            replace_todays(false), 
                                            stamp(date),ext(_),
                                            record_time(true), 
                                            verb(Verb)
                                         ],
                                  ).

%% url_file_local_date_mirror( +Url, +LocalD ).
%% url_file_local_date_mirror( +Url, +LocalD, +Opts ).
% 
% Keep a local copy of Url to directory LocalD as dated file LocF. 
% The local version is placed in Stem-Date.Ext with a symbolic link (redirected)
% from Stem.Ext to the downloaded version. The idea is that Stem.Ext always points to the latest version.
%
%  Predicate listens to debug( url_file_local_date_mirror ).
% 
%  Opts, a term or list of the following
%  * date(postfix)    
%    or prefix if date stamp is prefered before the stem (then LocF = Date-Basename)
%
%  * dnt_stamp(DntStamp)
%    returns the download stamp recorded in .dnt (the existing if nothing was downloaded)
%
%  * ext(Ext)
%    insist Ext is the extension of the file. that allows you to strip tar.gz as an extension, but make sure
%    your extension is the correct one, else the whole thing will fail
%
%  * file(LocF)    
%    if free variable, returns the local file name (basename only). if ground it is taken as the local output file.
%  
%  * iactive(Iact=true)
%    if =false= is given, default value of Verb changes to =false=
% 
%  * interface(Iface=prolog)  
%    alternatively you can use wget (works for anonymous ftps)
%    
%  * link_stem(Link=true)  
%    or false if LocF is to be linked to Basename.
%
%  * record_time(Rec=true)
%    creates   <Stem>.dnt  with datime(Y,M,D,H,M,S) term
%
%  * make_path(MkPath=true)
%    makes sure LocalD exists (see os_make_path/2)
% 
%  * replace_todays(Replace=false)
%    or false if you want to overide today's earlier download
% 
%  * stamp(Stamp=date)
%      or datetime if you want to include time (up to and including minutes)
%  
%  * verb(Verb=true)
%      only implemented for wget currently. Alt values =false= (alias =no=) and =quiet= (stricter). 
%      When =|Iact=false|= default value of Verb changes to =false=
%
%==
% ?- debug(url_file_local_date_mirror).
% ?- assert( ncbi('$local/../work/db/ncbi') ).
% ?- ncbi(Ncbi), url_file_local_date_mirror( 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', Ncbi ).
% % Using local directory: '/usr/local/users/nicos/local/../work/2014/ncbi'
% % Downloading URL: 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', onto file: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl-14.07.23.gz'
% % Downloaded url: 'ftp://ftp.ncbi.nih.gov/gene/DATA/gene2ensembl.gz', to local: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl-14.07.23.gz'
% % Warning, repointing link does not exist: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl.gz'
% % Linked to: '/usr/local/users/nicos/local/../work/2014/ncbi/gene2ensembl-14.07.23.gz'
% ?- ncbi(Ncbi), ls( Ncbi ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/23
% @tbd  option: delete_older: false,true,affirm
% @tbd  option: report if the same as last
% @tbd  option: update only_if... 
% @tbd  rel(Rel) option to get the relative location of the URL
%
url_file_local_date_mirror( Url, LocalD ) :-
    url_file_local_date_mirror( Url, LocalD, [] ).

url_file_local_date_mirror( Url, LocalD, Args ) :-
    Self = url_file_local_date_mirror,
    options:options_append( Self, Args, Opts ),
    os_make_path( LocalD, Opts ),
    memberchk( file(LocB), Opts ),
    file_base_name( Url, RemB ),
    options( ext(Ext), Opts ),
    url_file_local_date_mirror_local_file_name( LocB, Opts, RemB, Self, Ext ),
    expand_file_name( LocalD, [LocD|_] ),
    debuc( Self, 'Using local directory: ~p', [LocD] ), % pacify debug/3
    directory_file_path( LocD, LocB, LocP ),
    os_ext( dnt, LocP, LocDt ),
    options( replace_todays(Repl), Opts ),
    options( interface(Iface), Opts ),
    options( verb(Verb), Opts ),
    url_file_replace( Repl, Url, LocD, LocP, LocDt, Iface, Verb, RemB, Self, DntStamp ),
    ( memberchk(dnt_stamp(DntStamp),Opts) -> true; true ).

url_file_replace( false, Url, _LocD, LocP, LocDt, _Iface, _Verb, _RemB, _Self, DtStamp ) :-
    exists_file( LocP ),
    !,
    open( LocDt, read, DtStream ),
    read( DtStream, DtStamp ),
    close( DtStream ),
    % fixme: non debug
    debuc( _, 'File with today\'s date exists: ~p, so skipping download of:~p.', [LocP,Url] ).
url_file_replace( _, Url, LocD, LocP, LocDt, Iface, Verb, RemB, Self, BefStamp ) :-
    debug_chain( Self, url_file, UfPrior ),
    get_datetime( BefStamp ),
    url_interface_file( Iface, Url, Verb, LocP ),
    get_datetime( AftStamp ),
    debug_set( UfPrior, url_file ),
    debuc( Self, 'Downloaded url: ~p, to local: ~p', [Url,LocP] ),
    open( LocDt, write, DtOut ),
    portray_clause( DtOut, BefStamp ),
    portray_clause( DtOut, AftStamp ),
    close( DtOut ),
    directory_file_path( LocD, RemB, LnkP ),
    debug_chain( Self, os_repoint, RlPrior ),
    os_ext( dnt, LnkP, LnkPDnt ),
    os_repoint( LnkPDnt, LocDt ),
    os_repoint( LnkP, LocP ),
    % repoint_link( LnkP, LocP ),
    debug_set( RlPrior, os_repoint ).

url_interface_file( prolog, Url, _Verb, LocP ) :-
    url_file( Url, LocP, dnt(true) ).
url_interface_file( wget, Url, Verb, LocP ) :-
    url_interface_file_wget_verbosity( Verb, Vf ),
    @ wget( Url, Vf, '-O', LocP ).

url_interface_file_wget_verbosity( true, '--verbose' ).
url_interface_file_wget_verbosity( false, '--no-verbose' ).
url_interface_file_wget_verbosity( no, '--no-verbose' ).
url_interface_file_wget_verbosity( quiet, '--quiet' ).

url_file_local_date_mirror_local_file_name( LocB, Opts, RemB, Self, Ext ) :-
    var( LocB ),
    !, 
    debuc( Self, 'Creating dated local basename.', [] ),
    url_file_date_stamp( Date, Opts ),
    memberchk( date(DatePos), Opts ),
    url_file_local_date_mirror_local_file_name_date( DatePos, RemB, Date, Ext, LocB ).
url_file_local_date_mirror_local_file_name( LocB, _Opts, _RemB, Self, _Ext ) :-
    atom( LocB ),
    debuc( Self, 'Using given local basename: ~p', [LocB] ).

url_file_local_date_mirror_local_file_name_date( prefix, RemB, Date, _Ext, LocB ) :-
    atomic_list_concat( [Date,RemB], '-', LocB ).
url_file_local_date_mirror_local_file_name_date( postfix, RemB, Date, Ext, LocB ) :-
    % atomic_concat( '-', Date, DDate ),
    os_postfix( Date, RemB, LocB, [separator('-'),ext(Ext)] ).

url_file_date_stamp( Date, Opts ) :-
    options( stamp(Stamp), Opts ),
    url_file_stamp_date( Stamp, Date ).

url_file_stamp_date( date, Date ) :-
    date_two_digit_dotted( Date ).
url_file_stamp_date( datetime, Date ) :-
    datime_two_digit_dotted( Date ).
