
:- ensure_loaded( library(options) ).
:- ensure_loaded( library(real) ).

:- r_library(ggplot2).

gg_lines_defaults( Defs ) :-
	Defs = [df(df),x(x),y(y),group(group),line_size(1),gg_term('')].


/** gg_lines( +DfVals, +Opts ).

Plot data framed values to a ggplot2 plot.
DfVals can be atomic, taken as an R data frame variable or a 
matrix representation that is converted of DF via mtx_df/2.

Opts
 * rvar(Df=df)  default data frame value to use. 
 * x(X=x)   variable for x-axis
 * y(Y=y)   variable for y-axis
 * line_size size for drawn lines

  @author nicos angelopoulos
  @version  0.1 2015/9/22
%
*/

gg_lines( DfVals, Args ) :-
	options_append( gg_lines, Args, Opts ),
	gg_lines_df( DfVals, Df, Opts ),
	options( [x(X),y(Y),group(Group)], Opts ),
	options( line_size(Size), Opts ),
	G = ggplot(Df,aes(X, Y, group=Group, colour=Group)) + geom_line(size=Size),
	options( gg_term(Gterm), Opts ),
	gg_optional_term( Gterm, G, GG ),
	<- print( GG ).

gg_optional_term( '', G, G ) :- !.
gg_optional_term( Additive, G, (G + Additive) ).

gg_lines_df( Df, Df, _Opts ) :-
	atom( Df ),
	!.
gg_lines_df( DfValues, Df, Opts ) :-
	options( df(Df), Opts ),
	mtx_df( DfValues, Df ).
