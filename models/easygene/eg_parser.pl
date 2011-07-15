%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NAME :
%      eg_parser.pl -- LOST tool v0.0
%
% FUNCTION :
%        Extraction of information from Easygene report
%
% HISTORIQUE :
%      O.T.L 15/10/2009: creation
%      M.P   14/01/2010: Integration in the LOST toolbox and few modifications
%
%  
% DESCRIPTION : Translation of ouptut file from Easygene (*.GFF)into Prolog formatted fact. 
%               Lines that begin by # are commented. Data lines are translated into prolog facts:
%               eg(From, To, Direction,Extra_Info).
%          
% REMARK :
%      easygene grammar:
%		eg_file -> line eg_file | <eof>
%		line -> comment | entry
%		comment -> # non#* <eol>
%		entry -> seqname <tab> model <tab> feature <tab> start <tab> end	<tab> score <tab> dir <tab> somecode <tab> startc <tab> odds <eol>

%		
%          
% NOTE : n/a
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-use_module(library(lists)).

%%%%%%%%
% eg_parser(++EG_Input_File,--EG_Output_File)
% Description: see description of the file header
%%%%%%%%
eg_parser(EG_Input_File,EG_Output_File):-
	open(EG_Input_File, read, Input_Stream),
	open(EG_Output_File, write, Output_Stream),
        set_output(Output_Stream),
        readline(Input_Stream,First_Line),
	parser_eg(Input_Stream,First_Line),
	set_input(user_input),
	set_output(user_output),
	close(Input_Stream),
	close(Output_Stream).

% End of File = readline = [-1]
parser_eg(_,[-1]):-
        !.

% Empty Line = readline = []
parser_eg(Input_Stream,[]):-
        !,
        readline(Input_Stream,New_Line),
        parser_eg(Input_Stream,New_Line).									

									

% Line that starts with #
parser_eg(Input_Stream,[35|RestComment]):-			
	!,
	atom_codes(Comment_Atom,[37,35|RestComment]),
	write(Comment_Atom),nl,
	readline(Input_Stream,Next_Line),
	parser_eg(Input_Stream,Next_Line).

% Parse of a data line
parser_eg(Input_Stream,Entry_Codes):-					
	%NT_list = [seqname,model,feature,start,end,score,dir,somecode,startc,odds],
	parser_line(Entry_Codes,Entry_Tokens),
	fact_building_eg(Entry_Tokens),
	readline(Input_Stream,Next_Line),
	parser_eg(Input_Stream,Next_Line).





% List of Tokens to prolog fact
fact_building_eg(List):-
        nth1(4,List,Start),
        nth1(5,List,End),
        nth1(7,List,Dir),
        Temp is Start mod 3,
        (Temp = 0 ->
            Frm = 3
        ;
            Frm = Temp
        ),
        Cds =.. [eg,na,Start,End,Dir,Frm,[]], % more arguments can be be added as neccassary
	write(Cds),
        write('.'),
        nl.







	
