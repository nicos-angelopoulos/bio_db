
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(listing)).  % portray_clause/2.
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:map_list_options/3).

% also sets lib alias to that dir
:- ensure_loaded('../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- ensure_loaded( '../../lib/bio_db_repo_info.pl' ).


:- set_prolog_flag(allow_dot_in_atom, false).   % for portaying correctly

std_repo_defaults( [   debug(true),
                       iactive(true)
                       in_subs(true)
                   ] ).

/** std_repo( Opts ).

This is a script that runs data-building scripts for both supported organisms and creates a publishable bio_db_repo.

If you want to only the publish step, then reply `n` to the question. Useful when a small mistake can be 
corrected outside this loop, in one of the species.

It depends on having the upsh executable in your shell path (see pack(upsh)).

Ran with empty list the script works fine and creates everything with today's date. 
Running with options is still experimental and in theory you can force the script to continue working
on a directory with a previous time stamp. Opts are passed through to the children scripts.

This scripts have been build and (currently only) tested on Linux OS.

Opts
  * debug(Dbg=true)
    informational, progress messages
  * iactive(Iact=true)
    whether this is an interactive invocation
  * in_subs(InSubs=true)
    when =|Iact==false|=, this is used as the user input, in order to descent to subs or not

==
% date
Thu 10 Sep 19:34:38 BST 2020
% upsh std_repo pass=assembly=38 pass=release=101
==

@author nicos angelopoulos
@version  0.2 2018/11/12
@version  0.2 2018/11/12, added pass() option
@tbd replace csv_ids_rows/3, with Prolog code, that is currently going through R ?
@tbd on rerunning fails runs, the check for subdirs should be 
     leaning more to re-generating... ?
@see http://stoics.org.uk/~nicos/sware/upsh

*/
std_repo( Args ) :-
    Self = std_repo,
    options_append( std_repo, Args, Opts ),  % deals with debug()
    ( options(debug(false),Opts) -> true ; debuc(by_unix) ),
    bio_db_build_aliases( Opts ),
    absolute_file_name( bio_db_build_downloads('.'), DnDir ),
    debuc( Self, start, true ),
    os_path( BioDb, dnloads, DnDir ),
    os_path( Work, BioDbDir, BioDb ),
    debuc( Self, 'Work dir: ~p', Work ),
    debuc( Self, 'Bio db work dir: ~p', BioDbDir ),
    ( catch(directory_files(DnDir,DnSubs),_,fail) ->
               true
               ;
               DnSubs = []
    ),
    debuc( Self, 'Subs in download dir: ~p', [DnSubs] ),
    options( iactive(Iact), Opts ),
    options( in_subs(InSubs), Opts ),
    atomic_list_concat( [iactive,Icat], '=', Uact ),
    std_repo_subs( DnSubs, Iact, InSubs, Uact, Work+BioDbDir+BioDb, Opts ).

std_repo_subs( [], Iact, InSubs, Uact, Dirs, Opts ) :-
    !,
    debuc( std_repo, 'Downloads dir has no sub dirs.', [] ),
    ensure_loaded( pack('bio_db/src/lib/ui_yes_no') ),
    findall( StdSub, (os_dir(StdSub),StdSub\==lib), StdSubs ),
    Mess = 'Do you want me to run standards in subs: ~w',
    ( Iact == false ->
          ( InSubs == false -> Reply = false; Reply = true )
          ;
          ui_yes_no( true, Mess, [StdSubs], y, Reply )
    ),
    std_repo_subs_reply( Reply, Uact, StdSubs, Dirs, true, Opts ).
std_repo_subs( [H|T], Iact, InSubs, Uact, Dirs, Opts ) :-
    debuc( std_repo, 'Some sub-dirs exist: ~w', [[H|T]] ),
    ensure_loaded( pack('bio_db/src/lib/ui_yes_no') ),
    findall( StdSub, (os_dir(StdSub),StdSub\==lib), StdSubs ),
    Mess = 'Do you want me to run standards in subs: ~w',
    Alt = std_repo_create( Dirs, Opts ),
    ( Iact == false ->
          ( InSubs == false -> Reply = false; Reply = true )
          ;
          ui_yes_no( true, Mess, [StdSubs], y, Reply )
    ),
    std_repo_subs_reply( Reply, Uact, StdSubs, Dirs, Alt, Opts ).

std_repo_subs_reply( true, Uact, Subs, Dirs, _Alt, Opts ) :-
    findall( pass(Pass), member(pass(Pass),Opts), Passes ),
    map_list_options( std_sub(Uact), Subs, [call_options(Passes)|Opts] ),
    std_repo_create( Dirs, Opts ).
std_repo_subs_reply( false, _Uact, _Subs, _Dirs, Alt, _Opts ) :-
    % fixme: debug-write something here ?
    % true.
    call( Alt ).

std_sub( Uact, Sub, Opts ) :-
    working_directory( Old, Sub ),
    atomic_list_concat( [std,Sub], '_', StdSub ),
    maplist( opt_pl_cline, Opts, Crgs ),
    % Upsh =.. [upsh,StdSub,p,' - '|Crgs],
    Upsh =.. [upsh,StdSub,p,Uact|Crgs],
    % @ upsh( StdSub ),
    debuc( std_repo, 'Top level is shelling: ~w', Upsh ),
    @ Upsh,
    working_directory( _, Old ).

/** std_repo_create( Work+BioDbDir+BioDb, _Opts ).

Create the tgz

==
?-      W = '/usr/local/users/nicos/local/git/lib/swipl/pack/Downloads',
	B = 'bio_db_repo-19.04.06',
        directory_file_path( W, B, P ),
	std_repo_create( W+B+P, [] ).
==
*/
std_repo_create( Work+BioDbDir+BioDb, _Opts ) :-
    os_path( Work, 'bio_db_repo-publish', PubDir ),
    os_make_path( PubDir, [debug(true),afresh(true)] ),

    atomic_list_concat( [bio_db_repo,Date], '-', BioDbDir ),
    debuc( std_repo, 'Date: ~w', Date ),
    os_path( PubDir, BioDbDir, Repo ),
    % atomic_list_concat( [bio_db_repo,Date], '-', RepoDir ),
    % os_path( Work, RepoDir, Repo ),
    @ mkdir( Repo ),
    os_path( BioDb, data, BioDbData ),
    ( current_prolog_flag(apple,true) ->
        @ cp( '-R', BioDbData, Repo )
        ;
        @ cp( '-r', '--dereference', BioDbData, Repo )
    ),
    os_path( Repo, data, Rata ),
    % findall( RepoSub, os_dir(RepoSub,dir(Rata)), RepoSubs ),
    os_dir( RepoSubs, [dir(Rata),solutions(findall)] ),
    % os_dir_dirs( Rata, RepoSubs ),
    debuc( std_repo, 'Subs: ~w', [RepoSubs] ),
    % maplist( std_repo_zip_cat(Rata), RepoSubs ),
    zip_pl_files_in( Rata ),
    expand_file_name( '~/pl/packs/src/bio_db_repo', [PackRoot] ),
    os_path( PackRoot, doc, PackDoc ),
    os_path( PackRoot, prolog, PackPl ),
    @ cp( -r, PackDoc, Repo ),
    @ cp( -r, PackPl,  Repo ),

    bio_db_repo_file_version( Repo, Date ),

    findall( Inf, bio_db_repo_info(Inf), [InfNm,InfTi|Infs] ),
    os_path( Repo, 'pack.pl', RepoPackF ),
    portray_clauses( [InfNm,InfTi,version(Date)|Infs], file(RepoPackF) ),
    % os_ext( tar, RepoDir, TarF ),
    os_ext( tar, BioDbDir, TarF ),
    working_directory( Old, PubDir ),
    % @ tar( cf, TarF, RepoDir ),
    @ tar( cf, TarF, BioDbDir ),
    @ gzip( TarF ),
    os_ext( gz, TarF, TarGzF ),
    % os_ext( tgz, RepoDir, TgzF ),
    os_ext( tgz, BioDbDir, TgzF ),
    @ mv( TarGzF, TgzF ),
    std_repo_to_web_page( TgzF, BioDbDir, Date ),
    working_directory( _, Old ),
    debuc( std_repo, end, true ).

bio_db_repo_file_version( Repo, Date ) :-
    os_path( Repo, 'prolog/bio_db_repo_version.pl', VersF ),
    os_postfix( pfx, VersF, PfxF ),
    os_postfix( psfx, VersF, PsfxF ),
    @ cp( PfxF, VersF ),
    atomic_list_concat( [YrA,MnA,DyA], '.', Date ),
    maplist( atom_number, [YrA,MnA,DyA], [Yr,Mn,Dy] ),
    % portray_clauses( [Clause1], [file(VersF),mode(append)] ),
    open( PsfxF, read, PsfxS ),
    open( VersF, append, VersS ),
    LgYr is 2000 + Yr,
    write( VersS, 'V = ' ), write( VersS, Yr:Mn:Dy ), write( VersS, ',' ), nl( VersS ),
    write( VersS, 'D = ' ), portray_clause( VersS, date(LgYr,Mn,Dy) ),
    copy_stream_data( PsfxS, VersS ),
    close( PsfxS ),
    close( VersS ),
    Clauses = [ bio_db_repo_version(Yr:Mn:Dy), bio_db_repo_version(Yr:Mn:Dy,date(LgYr,Mn,Dy)) ],
    portray_clauses( Clauses, [file(VersF),mode(append)] ),
    @ rm( -f, PfxF ),
    @ rm( -f, PsfxF ).

std_repo_to_web_page( TgzF, BioDbDir, Date ) :-
    getenv( 'USER', nicos ),
    getenv( 'HOST', Host ),
    my_hostname( Host, Hname ),
    !,
    expand_file_name( '~/web/sware/packs/bio_db_repo/', [WebD]  ),
    os_path( WebD, TgzF, WebTgzF ),
    ( exists_file(WebTgzF) ->
        % working_directory( _, Old ),
        throw( file_exists(WebTgzF) )
        ;
        true
    ),
    @ mv( TgzF, WebTgzF ),
    @ chmod( 'go+r', WebTgzF ),
    os_path( WebD, data, WebDataD ),
    @ rm( -f, WebDataD ),
    os_path( RepoDir, data, RepoDataDir ),
    os_path( BioDbDir, data, RepoDataDir ),
    atomic_list_concat( [data,Date], '-', DataDatedD ),
    os_path( WebD, DataDatedD, WebRepoDataD ),
    @ cp( -r, RepoDataDir, WebRepoDataD ),
    working_directory( _WorkA, WebD ),
    % @ ln( -s, RepoDir, data ),
    debuc( std_repo, 'When publishing link:~p to data/ in the same directory', [RepoDir] ),
    % @ ln( -s, WebRepoDataD, data ),
    % @ mkvis( WebRepoDataD ),
    @ upsh( os_mk_vis, p, WebRepoDataD ),
    atomic_list_concat( [stoicos,Hname], '.', Unison ),
    @unison( Unison ).
std_repo_to_web_page( Tgz, _BioDbDir, _Date ) :-
    debuc( std_repo, pwd, end_dir ),
    debuc( std_repo, 'Skipping this bit as it is only relevant for Nicos\' own set-up.', true ),
    debuc( std_repo, 'You can publish the pack from tgz file: ~p', [Tgz] ).

/** fixme: delete, no longer used....*/
std_repo_zip_cat( Rata, Sub ) :-
    os_path( Rata, Sub, Path ),
    os_dir( PathSubs, [dir(Path),solutions(findall)] ),
    % os_dir_dirs( Path, PathSubs ),
    maplist( std_repo_zip_dir(Path), PathSubs ).

std_repo_zip_dir( Rata, Sub ) :-
    os_path( Rata, Sub, Path ),
    os_dir( Files, [dir(Path),solutions(findall)] ),
    % os_dir_files( Path, Files ),
    working_directory( Old, Path ),
    debuc( std_repo, pwd, zipping ),
    maplist( bio_db_data_zip_file, Files ),
    working_directory( _, Old ).

bio_db_data_zip_file( File ) :-
    file_name_extension( _Stem, pl, File ),
    !,
    % os_path( Dir, File, Path ),
    debuc( bio_db_data_zip, 'Doing: ~w', [File] ),
    file_name_extension( File, zip, ZipF ),
    @ zip( ZipF, File ),
    @ chmod( 'go+r', ZipF ),
    @ rm( -f, File ).
bio_db_data_zip_file( File ) :-
    debuc( bio_db_data_zip, 'Skipping: ~w', [File] ).

my_hostname( 'nicos-HP-EliteDesk-800-G4-TWR', 'άμπελος' ).
my_hostname( krotos, 'κρότος' ).
my_hostname( lykos, 'λύκος' ).

opt_pl_cline( Opt, Crg ) :-
    Opt =.. [Tn,Ta],
    ( Tn == pass ->
        Ta =.. [Nm,Arg],
        atomic_list_concat( [Tn,Nm,Arg], '=', Crg )
        ;
        atomic_list_concat( [Tn,Ta], '=', Crg )
    ).

zip_pl_files_in( Dir ) :-
    os_sel( os_files, ext(pl), Plies, [dir(Dir),stem(rel)] ),
    % os_files( Files, [dir(Dir),stem(abs)] ),
    % include( 
    length( Plies, NoFiles ),
    % maplist( zip_file_if_pl, Files ),
    working_directory( Old, Dir ),
    debuc( std_repo, pwd, zip_pl_files(changed_dir) ),
    zip_pl_files( Plies, 1, NoFiles ),
    working_directory( _, Old ),
    os_dirs( Dirs, [dir(Dir),stem(abs)] ),
    maplist( zip_pl_files_in, Dirs ).

zip_pl_files( [], _I, _Tot ).
zip_pl_files( [AbsFile|Plies], I, Tot ) :-
    debuc( std_repo, pwd, zip_pl_files(entry_at) ),
    debuc( std_repo, 'Absolute file, in zip_pl_files: ~w', AbsFile ),
    os_ext( zip, AbsFile, ZipF ),
    debuc( std_repo, 'Zipping (~d/~d): ~w into: ~w ', [I,Tot,AbsFile,ZipF] ),
    @ zip( '--quiet', ZipF, AbsFile ),
    @ chmod( 'go+r', ZipF ),
    @ rm( -f, AbsFile ),
    % working_directory( _, Old ),
    J is I + 1,
    zip_pl_files( Plies, J, Tot ).
