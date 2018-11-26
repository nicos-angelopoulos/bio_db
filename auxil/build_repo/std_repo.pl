
% if library(lib) is missing, install via pack_install(lib).
%
:- ensure_loaded( library(lib) ).

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
:- ensure_loaded( '../lib/bio_db_repo_info.pl' ).

:- debug(std_repo).
:- debug(by_unix).

std_repo_defaults( [] ).

/** std_repo( Opts ).

This is a script that runs data-building scripts for both supported organisms and creates a publishable bio_db_repo.

It depends on having the upsh executable in your shell path (see pack(upsh)).

Ran with empty list the script works fine and creates everything with today's date. 
Running with options is still experimental and in theory you can force the script to continue working
on a directory with a previous time stamp. Opts are passed through to the children scripts.

This scripts have been build and (currently only) tested on Linux OS.

@author nicos angelopoulos
@version  0.2 2018/11/12
@tbd on rerunning fails runs, the check for subdirs should be 
     leaning more to re-generating... ?
@see http://stoics.org.uk/~nicos/sware/upsh

*/
std_repo( Args ) :-
    Self = std_repo,
    options_append( std_repo, Args, Opts ),
    bio_db_build_aliases( Opts ),
    absolute_file_name( bio_db_build_downloads('.'), DnDir ),
    debug( Self, start, true ),
    os_path( BioDb, dnloads, DnDir ),
    write( bio_db(BioDb) ), nl,
    os_path( Work, BioDbDir, BioDb ),
    debug( Self, 'Work dir: ~p', Work ),
    debug( Self, 'Bio db work dir: ~p', BioDbDir ),
    os_dirs( DnSubs, dir(DnDir) ),
    debug( Self, 'Subs in download dir: ~p', [DnSubs] ),
    std_repo_subs( DnSubs, Work+BioDbDir+BioDb, Opts ).

% The logic for half-builds is no so good here,
% but fine for new builds (1st clause).
std_repo_subs( [], Dirs, Opts ) :-
    !,
    debug( std_repo, 'Downloads dir has no sub dirs.', [] ),
    ensure_loaded( pack('bio_db/src/lib/ui_yes_no') ),
    findall( StdSub, (os_dir(StdSub),StdSub\==lib), StdSubs ),
    Mess = 'Do you want me to run standards in subs: ',
    ui_yes_no( true, Mess, [StdSubs], y, Reply ),
    std_repo_subs_reply( Reply, StdSubs, Dirs, Opts ).
std_repo_subs( [_|_], Dirs, Opts ) :-
    std_repo_create( Dirs, Opts ).

std_repo_subs_reply( true, Subs, Dirs, Opts ) :-
    map_list_options( std_sub, Subs, Opts ),
    std_repo_create( Dirs, Opts ).
std_repo_subs_reply( false, _Subs, _Dirs, _Opts ) :-
    % ??? debug-write something here ?
    true.

std_sub( Sub, Opts ) :-
    working_directory( Old, Sub ),
    atomic_list_concat( [std,Sub], '_', StdSub ),
    maplist( opt_pl_cline, Opts, Crgs ),
    Upsh =.. [upsh,StdSub,Crgs],
    % @ upsh( StdSub ),
    @ Upsh,
    working_directory( _, Old ).

std_repo_create( Work+BioDbDir+BioDb, _Opts ) :-
    os_path( Work, 'bio_db_repo-publish', PubDir ),
    os_make_path( PubDir, [debug(true),afresh(true)] ),

    atomic_list_concat( [bio_db_repo,Date], '-', BioDbDir ),
    debug( std_repo, 'Date: ~w', Date ),
    os_path( PubDir, BioDbDir, Repo ),
    % atomic_list_concat( [bio_db_repo,Date], '-', RepoDir ),
    % os_path( Work, RepoDir, Repo ),
    @ mkdir( Repo ),
    os_path( BioDb, data, BioDbData ),
    @ cp( -r, '--dereference', BioDbData, Repo ),
    os_path( Repo, data, Rata ),
    % findall( RepoSub, os_dir(RepoSub,dir(Rata)), RepoSubs ),
    os_dir( RepoSubs, [dir(Rata),solutions(findall)] ),
    % os_dir_dirs( Rata, RepoSubs ),
    debug( std_repo, 'Subs: ~w', [RepoSubs] ),
    % maplist( std_repo_zip_cat(Rata), RepoSubs ),
    zip_pl_files_in( Rata ),
    expand_file_name( '~/pl/packs/src/bio_db_repo', [PackRoot] ),
    os_path( PackRoot, doc, PackDoc ),
    os_path( PackRoot, prolog, PackPl ),
    @ cp( -r, PackDoc, Repo ),
    @ cp( -r, PackPl,  Repo ),
    os_path( Repo, 'prolog/bio_db_repo_version.pl', VersF ),
    atomic_list_concat( [YrA,MnA,DyA], '.', Date ),
    maplist( atom_number, [YrA,MnA,DyA], [Yr,Mn,Dy] ),
    portray_clauses( [bio_db_repo_version(Yr:Mn:Dy)], file(VersF) ),
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
    debug_call( std_repo, end, true ).

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
    % os_path( RepoDir, data, RepoDataDir ),
    os_path( BioDbDir, data, RepoDataDir ),
    atomic_list_concat( [data,Date], '-', DataDatedD ),
    os_path( WebD, DataDatedD, WebRepoDataD ),
    @ cp( -r, RepoDataDir, WebRepoDataD ),
    working_directory( _WorkA, WebD ),
    % @ ln( -s, RepoDir, data ),
    @ ln( -s, WebRepoDataD, data ),
    @ mkvis( WebRepoDataD ),
    atomic_list_concat( [stoicos,Hname], '.', Unison ),
    @unison( Unison ).
std_repo_to_web_page( Tgz, _BioDbDir, _Date ) :-
    debug( std_repo, 'Skipping this bit as it is only relevent for Nicos\' own set-up.', true ),
    debug( std_repo, 'You can publish the pack from tgz file: ~p', [Tgz] ).

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
    % debug( std_repo, 'Dir: ~p Files: ~w', [Path,Files] ),
    working_directory( Old, Path ),
    maplist( bio_db_data_zip_file, Files ),
    working_directory( _, Old ).

bio_db_data_zip_file( File ) :-
    file_name_extension( _Stem, pl, File ),
    !,
    % os_path( Dir, File, Path ),
    debug( bio_db_data_zip, 'Doing: ~w', [File] ),
    file_name_extension( File, zip, ZipF ),
    @ zip( ZipF, File ),
    @ chmod( 'go+r', ZipF ),
    @ rm( -f, File ).
bio_db_data_zip_file( File ) :-
    debug( bio_db_data_zip, 'Skipping: ~w', [File] ).

my_hostname( krotos, 'κρότος' ).
my_hostname( lykos, 'λύκος' ).

opt_pl_cline( Opt, Crg ) :-
    Opt =.. [Tn|Ta],
    atomic_list_concat( [Tn,Ta], '=', Crg ).

zip_pl_files_in( Dir ) :-
    os_files( Files, [dir(Dir),stem(abs)] ),
    maplist( zip_file_if_pl, Files ),
    os_dirs( Dirs, [dir(Dir),stem(abs)] ),
    maplist( zip_pl_files_in, Dirs ).

zip_file_if_pl( AbsFile ) :-
    % fixme:  just sticking tape for now
    ( os_ext(pl,_Stem,AbsFile) -> 
            os_path( Path, File, AbsFile ),
            working_directory( Old, Path ),
            os_ext( zip, File, ZipF ),
            @ zip( ZipF, File ),
            working_directory( _, Old )
            ;
            true 
    ).
