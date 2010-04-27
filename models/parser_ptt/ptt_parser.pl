%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NAME :
%      ptt_parser.pl -- LOST tool v0.0
%
% FUNCTION :
%        Extraction of information from Genebank
%
% HISTORIQUE :
%      O.T.L 23/10/2009: creation
%      M.P   18/01/2010: Integration in the LOST toolbox and few modifications
%
%
% DESCRIPTION : - Translation of ouptut file from Genebank (*.ptt)into Prolog formatted fact.
%               Lines before the data are commented. Data lines are translated into prolog facts:
%               gb(From, To, Direction,Extra_Info). (Extra_Info = [Gene_Name] if available)
%               - Nucleotids manipulation given the *.fna file of Genebank
%
%
% REMARK :
%      genebank grammar:
%		eg_file -> line eg_file | <eof>
%		line -> comment | entry
%		comment -> # non#* <eol>
%		entry -> Start .. End <tab> Direction <tab> Lenght <tab> PID <space> Gene <tab> Synonym <tab> Code <tab> COG <tab> Product
%
% NOTE : Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%-----------
% Ptt parser
%-----------
% gb_parser(++GB_Input_File,--GB_Output_File)
% Description: see description of the file header
%-----------

gb_parser(GB_Input_File,GB_Output_File):-
	open(GB_Input_File, read, Input_Stream,[alias(gb_in)]),
	open(GB_Output_File, write, Output_Stream,[alias(gb_out)]),
        set_output(Output_Stream),
        readline(Input_Stream,First_Line),
        var(Comment),
	parser_gb(Input_Stream,Comment,First_Line),
	set_input(user_input),
	set_output(user_output),
	close(Input_Stream),
	close(Output_Stream).

% End of File = readline = [-1]
parser_gb(_,_Comment,[-1]):-
        !.

% Empty Line = readline = []
parser_gb(Input_Stream,Comment,[]):-
        !,
        readline(Input_Stream,New_Line),
        parser_gb(Input_Stream,Comment,New_Line).


% Line that starts with 'Location' (76,111,99,97,116,105,111,110) end of the comments
parser_gb(Input_Stream,Comment,[76,111,99,97,116,105,111,110|RestComment]):-
        var(Comment),
	!,
	atom_codes(Comment_Atom,[37,76,111,99,97,116,105,111,110|RestComment]),
	write(Comment_Atom),nl,
	readline(Input_Stream,Next_Line),
        Comment = 'end',
	parser_gb(Input_Stream,Comment,Next_Line).

% Line to comment
parser_gb(Input_Stream,Comment,Line_Codes):-
        var(Comment),
	!,
	atom_codes(Comment_Atom,[37|Line_Codes]),
	write(Comment_Atom),nl,
	readline(Input_Stream,Next_Line),
	parser_gb(Input_Stream,Comment,Next_Line).


% Parse of a data line (Ignored characters = [space,tab,'.'])
parser_gb(Input_Stream,'end',Entry_Codes):-
	parser_line(Entry_Codes,[9,32,46],Entry_Tokens),
	fact_building_gb(Entry_Tokens),
	readline(Input_Stream,Next_Line),
	parser_gb(Input_Stream,'end',Next_Line).



% Tokens to Prolog fact

fact_building_gb(List):-
	List = [ Start, Stop, Dir, Length, PID, Gene, Synonym, Code, COG | ProductList],
	/*
        nth1(1,List,Start),
        nth1(2,List,Stop),
        nth1(3,List,Dir),
        nth1(6,List,Gene),
	nth1(7,List,SynCode),
	*/
        Temp is Start mod 3,
	(Temp = 0 ->
            Frm = 3
	;
            Frm = Temp
	),
	add_spaces(ProductList,ProductListSpaced),
	atomize_list(ProductListSpaced,AtomProductListSpaced),
	atom_concat_list(AtomProductListSpaced,Product),
	Cds =.. [gb,Start,Stop,Dir,Frm,[gene_name(Gene),length(Length),pid(PID),synonym(Synonym),code(Code),cog(COG),product(Product)]],
	writeq(Cds),write('.'),nl.

atomize_list([],[]).
atomize_list([Atom|R1],[Atom|R2]) :-
	atom(Atom),
	!,
	atomize_list(R1,R2).
atomize_list([Token|R1],[Atom|R2]) :-
	term2atom(Token,Atom),
	!,
	atomize_list(R1,R2).

add_spaces([],[]).
add_spaces([Token], [Token]).
add_spaces([Token,NextToken|RestTokens],[Token,' '|Rest2]) :-
	add_spaces([NextToken|RestTokens],Rest2).

