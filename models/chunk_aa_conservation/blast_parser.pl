%==========================================================================
% Blast_parser.pl
% parses 1 block of alignment as output from tblastn and returns it as a list 
% of prolog lists where each correspond to a hit among a number of database-sequences.
% If a query is matched several times by the same db-sequence, all are reported.
% each list will have the format (ID, Start, End, Alignment)
%==========================================================================
% version 0.01
% 29.10.2009
% Ole Torp Lassen
%==========================================================================
:-[read_line].
%==========================================================================
% Grammar for tblastn output, one block at a time
%==========================================================================
prod(init,									[intro,qinfo,dbinfo,db_ids]).
/*
prod(main,									[aln]).
prod(finish,								[rest]).
*/

prod(intro,							[comment(13)]).
prod(qinfo,							[empty_token, query_id, comment(1), query_length, comment(2)]).
prod(dbinfo,						[comment(10),db_ids]).
prod(comment(N),				[{N>1,M is N -1},read_line,comment(M)]).
prod(comment(1),				[read_line]).

/*
prod(aln(Accs),						[query_line, db_lines(Accs)]). 			% or empty
prod(query_line,					[qPosBefore, query, qPosAfter]).
prod(db_lines([A|Ccs]),		[db_line(A), more_hits(Ccs)]).
prod(db_lines([]),				[empty_line]).
prod(more_hits([A|Ccs]),	[B,{B\=A}, comment(1), more_hits(Ccs)]).	
prod(more_hits(Accs),			[db_lines(Accs)]).	
prod(db_line,							[db_acc, db_pos_before, db_seq, db_pos_after]).
prod(finish,							['  Database:']).
*/

%=============================================================================
% parse_blast_init/2
% parses first lines of tblastn output file and returns IDs [Query_id | Db_ids]
%
parse_blast_init(Infile,QLength,IDs):-					% Accs is [QId | DBIDS]
	parse_bi(init,Infile,QLength,IDs-[]).

parse_bi(LHS,Infile,QLength,L1-L2):-
	prod(LHS,RHS),
	proces_bi(RHS,Infile,QLength,L1-L2).

%============================================================================
% parse_blast_main/4
%	parses one block of alignments according to a list of DB_IDs, 
% such that only the best match for each will be parsed.
% returns the new query nucleotide position and n+1 aligned amino sequences,
% namely the query and n best database matches as presented in Infile.
% each is presented as (ID, Startpos, Endpos, AA_list).
% Everything is in character-codes, except positions. 
%
parse_blast_main(Infile,[QId|DB_Ids],QPos,QLastPos,Alns):-
	/*
	append(_,[47|Tx_start],QId),
	append(Num,[45|_],Tx_start),
	number_codes(Qpos,Num),
	*/
	(
	parse_bm(query_line,Infile,QId,QPos,QPosAfter,Aln_tab,Qa) ->	
	parse_bm(db_lines_1,Infile,Aln_tab,DB_Ids,DBas),
	Alns = [Qa|DBas],
	QLastPos is QPosAfter -1
	;
	%QLastPos = 1, QPos = 1, % Something here is not working!!!!
	%writeln((QPos,QPosAfter)),
	%QPos = TempQPos,
	%QLastPos = TempQLastPos,
	QPos = 0,
	%LastPos = 0,
	Alns = []
	).

%============================================================================
% parse_blast_main_new/4
%	parses all blocks of alignments according to a list of DB_IDs, 
% such that onlyy the best match for each is parsed.
% returns the last query nucleotide position and n+1 aligned amino sequences,
% namely the query and n best database matches as presented in Infile.
% each is presented as (ID, Startpos, Endpos, AA_list).
% Everything is in character-codes, except positions. 
%
parse_blast_main_new(Infile,[QId|DB_Ids],QPos,QLastPos,Alignment):-
	(
	parse_bm(query_line,Infile,QId,QPos,QPosInBetween,Aln_tab,Q) ->	
	parse_bm(db_lines_1,Infile,Aln_tab,DB_Ids,DB),
		
	% adjust(DB,DB_Ids,Adjusted_DB),
		(
		parse_blast_main_new(Infile,[QId|DB_Ids],QPosInBetween,QPosAfter,[Q_rest|DB_rest])->		
	%	 writeln([Q_rest|DB_rest]),
		combine_tuples(Q,Q_rest,Query),																						% (Qid,P1,_,A1)+(Qid,_,P2,A2) = (Qid,P1,P2,append(A1,A2))		
		
		combine_DB(DB,DB_rest,DataBase)																					%  similar for each DB_Id, DB changed from adjusted_DB
		
		;
		
		Query = Q,
		DataBase = DB		% changed from adjusted_DB
		),
	Alignment = [Query|DataBase],		
	QLastPos is QPosAfter
	
	;
	Alignment = ['n/a','n/a'],
		(
		nonvar(QPos)->
		QLastPos is QPos
		;
		QPos is 1,
		QLastPos is 1	
		)
	)
	.

%=====================================
% Adjusted_DB = mathces in DB + and dummy seqeunce for each ID in DB_IDs that do not occur in this block
%
%
adjust(Actual_matches,Supposed_match_Ids,Adjusted_matches):-
	(
	select(ID,Supposed_match_Ids,Rest_Ids),	not(member((ID,_,_,_),Actual_matches))->
	makelist(60,32,Dummy),
	adjust(Actual_matches,Rest_Ids,Adjusted_Rest),
	Adjusted_matches = [(ID,'n/a','n/a',Dummy)|Adjusted_Rest]
	;
	Adjusted_matches = Actual_matches
	).

combine_tuples(Tuple,'n/a',Tuple).
combine_tuples((Id,P1,_,A1),(Id,_,P2,A2),(Id,P1,P2,A3)):-												% (Id,P1,_,A1)+(Id,_,P2,A2) = (Id,P1,P2,append(A1,A2))		
	append(A1,A2,A3).

% Combining to blocks A and B of DB alignments that may not contain same number of members.
% in case |A|<|B| each a will be combined with one unique b, surplus b's will be padded before
% in case |A|>|B| each a will be combined with one unique b, surplus a's will be padded after
combine_DB(DB,['n/a'],DB).																											% similar for each DB_Id	

combine_DB([],[(Id,P3,P4,A2)|DB2_Rest],[(Id,P3,P4,A3)|DB_Rest]):-								% If 2nd database has more sequences than the 1st, add padding before each
	makelist(60,32,Padding),
	append(Padding,A2,A3),
	combine_DB([],DB2_Rest,DB_Rest).

combine_DB([],[],[]).

combine_DB([(DBid,P1,P2,A1)|DB1_Rest],DB2,[(DBid,P1,P4,A3)|DataBaseRest]):-			% 			
	
	DB2 \= ['n/a'],	
	(
	% Before is P2-1, After is P2+1,
	
	select((DBid,P3,P4,A2),DB2,DB2_Rest), (abs(P3-P2) =:= 1)->							% find a continuation in 2nd for every member in 1st
	/*																																				% in either direction
	atom_codes(Atom, DBid),
	atom_codes(L,A2),																																				% old check : (P3 = After; P3 = Before)
	write(Atom),write(' '),write(P1), write(' '), write(P2),writeln(' '),
	write(Atom),write(' '),write(P3), write(' '), write(P4),writeln(' '),
	writeln(L),nl,
	*/
	append(A1,A2,A3)
	;			 																																				% if for some there is no match in 2nd, add padding after 
	DB2_Rest = DB2,
	P4 is P2,
	
	makelist(60,32,Padding),
	append(A1,Padding,A3)
	),
	combine_DB(DB1_Rest,DB2_Rest,DataBaseRest).

	

/*
adjust_lengths([],[]).
adjust_lengths([(Q,P1,P2,Qa)|DB],[(Q,P1,P2,Qa)|DB2]):-
	adjust(P1,P2,DB,DB2).

adjust(_,_,[],[]).
adjust(P1,P4,[(DBid,P2,P3,A)|DBRest],[(DBid,P1,P2,B)|DBRest2]):-
	Pre is P2-P1, makelist(Pre,32,Prefix),
	Post is P4-P3, makelist(Post,32,Postfix),
	append(A,Postfix,C),
	append(Prefix,C,B),
	adjust(DBRest,DBRest2).
*/



%============================================================================
% parse_blast_bm/7
% parses the first line of an alignment block in the blast output.
%----------------------------------------------------------------------------
% arguments in, Infile, Qid, Qpos
% arguments out, Aln_tab (for calculationg start of alighment), (Qid,Qpos,QposNext, QAlign)
% 
parse_bm(query_line,Infile,Qid,Qpos,NextQpos,Aln_tab,Q ):-
	read_token(Infile,_,[49,95,48]),
	read_token(Infile,_,Pos),
	read_token(Infile,Spaces,QAlign),
	number_codes(Qpos,Pos),						% added
	length(Pos,P),
	Aln_tab is P + Spaces,
	read_token(Infile,_,Qpa),
	read_line(Infile,_),
	number_codes(QLastpos,Qpa),
	NextQpos is QLastpos+1,
	% NextQpos is ((Qpanum +1) * 3) + Qpos -1,
	Q = (Qid,Qpos,QLastpos,QAlign).

%============================================================================
% parse_blast_bm/5
% parse_blast_bm/6
% parses the databse match lines in a block of Blast output.
% For each DB_Id in the list (4th argument) the corresponding first/best match is parsed
% to (ID,Pos,PosAfter,Align) and collected in the resultlist
%----------------------------------------------------------------------------
% arguments in /5: 	db_lines_1, Infile, Aln_tab, [ID|DB_IDs]
% arguments in /6:	db_lines, Infile, Aln_tab, ID, DB_IDs
%	arguments out: 	[(ID,Pos,PosAfter,Align)]
%
parse_bm(db_lines_1,Infile,Aln_tab,DB_IDs,[(ID,Pos,PosAfter,Align)|RestAs]):-					% first line after query line must be match an untreated DB_IDS
	read_token(Infile,_,ID),																														% remove in from untreated list and treat it		
	% member(ID,DB_IDs),
	% select(ID,DB_IDs,DB_IDs_Rest), 
	parse_bm(db_lines,Infile,Aln_tab,ID,DB_IDs,[(ID,Pos,PosAfter,Align)|RestAs]).


parse_bm(db_lines,Infile,Aln_tab,ID,DB_IDs,[(ID,Pos,PosAfter,Align)|RestAs]):-		% Treating ID
	read_token(Infile,_,PosCodes),
	number_codes(Pos,PosCodes),
	length(PosCodes,M),
	N is Aln_tab - M,
	read_n_spaces(Infile,N),
	read_line(Infile,Rest_of_Line),
	
	append(Align,[32,Num|RestposAfter],Rest_of_Line),num(Num),
	append(PosACodes,[13],[Num|RestposAfter]),
	number_codes(PosAfter,PosACodes),
	
	get_next_ID(Infile,DB_IDs,Next),																									% scan for next untreated or empty line
	(
		Next \= [] ->																																		% detected untreated db_line in alignment
	% select(Next,DB_IDs,DB_IDs_Rest),																							% remove it from untreated
		parse_bm(db_lines,Infile,Aln_tab,Next,DB_IDs,RestAs)												% and treat it
	;
		RestAs = []																																			% no more db_lines in alignment
	).	 
	

get_next_ID(Infile,DB_IDs,Next):-											% given a file and a set of untreated DB-IDs
	read_token(Infile,_,Token),													% scan each line until either an untreated ID or an empty line is met,
	(																										
		Token == [] -> Next = [],	read_line(Infile,_)
	;
		(
		% atom_codes(Atom,Token),
		% member(Atom,DB_IDs) -> Next = Token, read_line(Infile,_)
		% writeln(DB_IDs),
		member(Token,DB_IDs) -> Next = Token
		;
		read_line(Infile,_),
		get_next_ID(Infile,DB_IDs,Next)
		)
	).


num(N):- member(N,[49,50,51,52,53,54,55,56,57]).

%============================================================================
% RHS processes for parse_blast_init
%
proces_bi([],_File,_,L-L).
proces_bi([H|Rest],Infile,Q,L1-L2):-			% 
	nonterminal(H),
	parse_bi(H,Infile,Q,L1-L3),
	proces_bi(Rest,Infile,Q,L3-L2).
	
proces_bi([M|Rest],Infile,Q,L1-L2):-
	metaterminal(M, Goal),
	call(Goal),
	proces_bi(Rest,Infile,Q,L1-L2).

proces_bi([query_id|Rest],Infile,Q,[ID|L2]-L3):-
	read_token(Infile,_,ID),
	proces_bi(Rest,Infile,Q,L2-L3).
	
proces_bi([query_length|Rest],Infile,QLength,L1-L2):-
	read_token(Infile,_,[40|QLengthCodes]),
	number_codes(QLength,QLengthCodes),
	proces_bi(Rest,Infile,QLength,L1-L2).

proces_bi([db_ids|Rest],Infile,Q,L1-L3):-	
	get_DBids(Infile,DBIds),
	append(DBIds,L2,L1),
	proces_bi(Rest,Infile,Q,L2-L3).
	
proces_bi([read_line|Rest],Infile,Q,L1-L2):-
	read_line(Infile,_),
	proces_bi(Rest,Infile,Q,L1-L2).
	
proces_bi([empty_token|Rest],Infile,Q,L1-L2):-
	read_token(Infile,_,_),
	proces_bi(Rest,Infile,Q,L1-L2).


get_DBids(Infile,IDS):- 
	read_token(Infile,_,ID),
	(
	ID = [78,67|_],
	IDS = [ID|Rest_IDS],
	read_line(Infile,_),
	get_DBids(Infile,Rest_IDS)
	;
	IDS = []
	).

metaterminal({G},G).
nonterminal(init).
nonterminal(intro).						
nonterminal(qinfo).					
nonterminal(dbinfo).	  
nonterminal(comment(_)).

write_list([]).
write_list([A|Rest]):-
	writeln(user_output,A),
	write_list(Rest).

makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
	N > 0,
	M is N-1,
	makelist(M,X,Rest).



%==========================================================================
% test goals

testlist([32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,80,70,82,65,86,83,83,65,76,83,65,68,83,73,65,86,76,83,83,65,71,83,76,83,80,32,32,32,83,67,76,82,86,112,42,105,82,87,83,97,87,76,82,86,42,84,83,83,83,78,76,82,83,65,83,65,76,65,83,65,83,82,84,73,70,87,73,83,83,83,70,75,80,69,68,65,76,77,86,73,70,67,83,114,80,86,70,76,83,76,65,68,84,67,75,73,80,83,65,83,73,83,75,86,84,83,73,67,65,77,80,82,71,65,71,119,73,80,83,82,76,78]).

testgoal1 :- open(tblastn_test,read,_,[alias(input)]),
						parse_blast_init(input,_Length,IDs),
						parse_blast_main_new(input,IDs,_TempQPos2,_TempLastPos2,Alns1),
						write_list(Alns1),
						close(input).

testgoal2 :- open('ec100k_67735-68846_(AA)-2.out',read,_,[alias(input)]),
						parse_blast_init(input,IDs),
						parse_blast_main(input,IDs,_,Alns1),
						write_list(Alns1),
						parse_blast_main(input,IDs,_,Alns2),
						write_list(Alns2),
						close(input).
						


