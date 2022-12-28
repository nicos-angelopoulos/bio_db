
:- use_module(library(csv)).        % csv_read_file/3.
:- use_module(library(apply)).      % maplist/2.
:- use_module(library(listing)).    % portray_clause/2.

:- lib(stoics_lib:get_date_time/1).

%% csv_to_pl( +Self, +Stem ).
%  csv_to_pl( +Stem ).
% 
%  Convert Stem.csv or Stem (with .csv extension) to a terms file RealStem.pl.
% 
%  Self, guides printing of debug message, regarding the output file.
%
%  csv_to_pl/1 is only provided for backward compatibility, entry point should
%  only be via /2 version. Self is set to csv_to_pl, which means message is not
%  printed.
%
% @author nicos angelopoulos
% @version  0.2 2016/20
% @version  0.3 2020/9/11, added Self
%
csv_to_pl( InStem ) :-
    csv_to_pl( csv_to_pl, InStem ).

csv_to_pl( Self, InStem ) :-
    file_name_extension( InStem, csv, CsvF ),
    file_name_extension( Stem, csv, CsvF ),
    file_name_extension( Stem, pl, PlF ),
    debuc( Self, 'Output Prolog file: ~p', [PlF] ),
    % 16.06.20: changed default functor from row() to basename of stem
    file_base_name( Stem, Base ),
    csv_to_pl( CsvF, PlF, Base, [functor(Base)] ).

csv_to_pl( CsvF, PlF, _Base, Opts ) :-
    csv_read_file( CsvF, Csv, Opts ),
    open( PlF, write, Out ),
    write( Out, '% ' ), 
    get_date_time( Date ),
    Date = date(Y,M,D,Hr,Mn,_Sc,_Off,Tzone,_DST),
    write( Out, Y/M/D ), write( Out, ' @ ' ), write( Out, Hr:Mn ), 
    write( Out, ' (' ), write( Out, Tzone ), write( Out, ')' ), nl( Out ),
    % atomic_list_concat( [Base,date], '_', Bate ),
    % Bterm =.. [Bate,Date],
    % maplist( portray_clause(Out), [Bterm|Csv] ),
    maplist( portray_clause(Out), Csv ),
    close( Out ).
