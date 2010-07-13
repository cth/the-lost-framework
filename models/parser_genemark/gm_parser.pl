%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NAME :
%      gm_parser.pl -- LOST tool v0.0
%
% FUNCTION :
%        Parser of the report of Genemark.Hmm 
%
% HISTORIQUE :
%      O.T.L 23/10/2009: creation
%      M.P   14/01/2010: Integration in the LOST toolbox and few modifications
%
%  
% DESCRIPTION : Translation of ouptut file from Genemark (*. into Prolog formatted fact. 
%               Header of the file are commented. Comments are ended when the character
%               # (35) appears in a line. Parser of the data lines starts after this line.
%               Data lines are translated into prolog facts:
%               gm(From, To, Direction,Extra_Info).
%          
% REMARK :
% genemark grammar:
%		Comments -> Entries
%		Entries -> Entry,Entries | <eof>
%		Entry -> number,<tab>,direction,<tab>,start,<tab>,stop,<tab>,length,<tab>,class,nl 
%       
% NOTE : n/a
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%
% gm_parser(++EG_Input_File,--EG_Output_File)
% Description: see description of the file header
%%%%%%%%
gm_parser(GM_Input_File,GM_Output_File):-
	open(GM_Input_File, read, Input_Stream,[alias(gm_in)]),
	open(GM_Output_File, write, Output_Stream,[alias(gm_out)]),
        set_output(Output_Stream),
        readline(Input_Stream,New_Line),
        var(Comment),
        parser_gm(Input_Stream,Comment,New_Line),
	set_input(user_input),
	set_output(user_output),
	close(Input_Stream),
	close(Output_Stream).

% End of File = [-1]
parser_gm(_Input_Stream,_Comment,[-1]):-
        !.

% Empty Line = readline =[]	
parser_gm(Input_Stream,Comment,[]):-
        !,
        readline(Input_Stream,New_Line),
        parser_gm(Input_Stream,Comment,New_Line).
	


% Parse of Genemark Header
parser_gm(Input_Stream,Comment,List_Codes):-
        var(Comment),
        !,
        (last_comment(List_Codes) ->
            Comment = 'end'
        ;
            true
        ),
        atom_codes(Comment_Line,[37|List_Codes]), 
	write(Comment_Line),nl,
        readline(Input_Stream,New_Line),
        parser_gm(Input_Stream,Comment,New_Line).


parser_gm(Input_Stream,'end',List_Codes):-
        parser_line(List_Codes,Entry_Tokens),
        fact_building_gm(Entry_Tokens),
        readline(Input_Stream,Next_Line),
        parser_gm(Input_Stream,'end',Next_Line).



% Tokens to Prolog fact

fact_building_gm([_Gene_Number,Dir,Start,End,_Length,_Class]):-
        Temp is Start mod 3,
        (Temp = 0 ->
            Frm = 3
        ;
            Frm = Temp
        ),
        Cds =.. [gm,Start,End,Dir,Frm,_Extra_Info], % more arguments can be be added as neccassary
	write(Cds),
        write('.'),
        nl.


% Utils

last_comment(List_Codes) :-
        member(35,List_Codes),
        !.

last_comment(_List_Codes) :-
         fail.



%%%%%%%%%
%%%% end_of_token
%%%%%%%%%

%%%end_of_token(32) :-
%%%        !.

%%%end_of_token(9) :-
%%%        !.

