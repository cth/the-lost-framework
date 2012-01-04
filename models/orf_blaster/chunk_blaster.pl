:- [read_line].
:- lost_include_api(misc_util).

%%%%%%
% Blast Command Building
%%%%%%

blast_database('/opt/db/blast/EnteroBacterialesGB/EnteroBacterialesGB.nt').


blast_command(Cmd) :-
        blast_database(Database),
        atom_concat_list(['tblastn -db ', Database, ' -outfmt 5 -db_gencode 11 -use_sw_tback'],Cmd).

%------
% conservation(++Chunk_Stream,++Counter,++Dir,++Frame,++Aln_Stream,++Cons_Stream)
% Main predicate, computation
%------

conservation(Chunk_Stream,Counter,Dir,Frame,Aln_Stream,Cons_Stream):-
	(at_end_of_stream(Chunk_Stream) ->
            true
	;
            cons_init(Chunk_Stream,QId,Left,Right,Start_ORF,Query,DBIDs,All_alignments,ChunkTerminated,Status),
            (Status == 0 ->
                cons_main(Query,DBIDs,All_alignments,Aln_Stream,Cons,Avg_Cons)
                ;
                Cons = [],
                Avg_Cons is 0
            ),
            report_cons(Cons_Stream,QId,Left,Right,Dir,Frame,Start_ORF,ChunkTerminated,Cons,Avg_Cons),
            Counter2 is Counter+1,
            !,
            conservation(Chunk_Stream,Counter2,Dir,Frame,Aln_Stream,Cons_Stream)
	).

% cons_init(++Chunk_Stream,--QId,--Left,--Right,--Start_ORF,--Query,--All_Alignment,--ChunkTerminated,--Status)
% From the stream of the FASTA file, this predicate picks one query sequence, blast this sequence with several genomes and compute the different hits
% from the result of the blast report.

cons_init(Chunk_Stream,QId,Left,Right,Start_ORF,Query,IDs,All_alignments,ChunkTerminated,Status):-
	blast_next_chunk(Chunk_Stream,Headline_Chunk,Query,XML_Blast_File,ChunkTerminated,Status),
	nl,write('Blast Status --------> '), writeln(Status),
	(Status == 0 ->
		parser_headline_fasta(Headline_Chunk,QId,Left,Right,Start_ORF),
		nl,writeln('Headline Parsed ------------- 1 '),
		write('XML_Blast_File :'), writeln(XML_Blast_File),nl,
		parser_blast(XML_Blast_File, IDs, All_alignments),
		nl,writeln('Rest Parsed ----------------- 2 ')
	;
		% TODO something but I don't know what /TODO
		IDs = [],
		All_alignments = [],
		Left=0,
		Right=0
	).


% Here the calculated alignments may be written to Aln_Stream if desired.
% ---> in that case toggle "output_alignmnets(yes)" 
% must return also start and end of alignment, both in [1..QueryLength].
%
%cons_main(++Query,++DBIDs,++All_alignments,++Aln_Stream,--Cons,--Avg_Cons),
cons_main(Query,_DBID,All_alignments,_Aln_Stream,Cons,Avg_Cons) :-
	All_alignments \= [],
	All_alignments \= ['n/a','n/a'],
	!,
	determine_best_alns(All_alignments,Best_alignments),
	% TODO Do something to manage the option output_alignments /TODO
	% (output_alignments(yes)->
	%     report_alns(Aln_Stream,FirstPos,LastPos,All_alignments,Best_alignments)
       % ;
       %     true
       % ),
        compute_conservation(Query,Best_alignments,Cons,Avg_Cons).

cons_main(_Query,_DBID,_All_alignments,_Aln_Stream,Cons,Avg_Cons) :-
        Cons = [],
        Avg_Cons is 0.



% report_cons(++Cons_Stream,++QId,++Left,++Right,++Dir,++Frame,++Start_ORf,++ChunkTerminated,++Cons,++Avg_Cons)

report_cons(Cons_Stream,QId,Left,Right,Dir,Frame,Start_ORF,ChunkTerminated,Cons,Avg_Cons) :-
        Term =.. [conservation,QId,Left,Right,Dir,Frame,Rest_Infos],
        ( Dir = '+' ->
            Cons1 = Cons
        ;
            reverse(Cons,Cons1)
        ),
        times_3(Cons1,Cons_Final),
        Rest_Infos = [cons_annotation(Cons_Final),cons_score(Avg_Cons),stop(ChunkTerminated)|Rest_Infos2],
        (var(Start_ORF) ->
            Rest_Infos2 = []
        ;
            Rest_Infos2 = [start_ORF(Start_ORF)]
        ),
        writeq(Cons_Stream,Term),
        write(Cons_Stream,'.'),
        nl(Cons_Stream).


%---------
% Utils cons_init
%---------


% blast_next_chunk(++Chunk_Stream,--Headline_Chunk,--XML_Blast_File,--ChunkTerminated,--Status),
% Get information on one chunk (Headline and Query) and generate the blast XML report

blast_next_chunk(Chunk_Stream,Headline_Chunk,Query,XML_Blast_File,ChunkTerminated,Status):-
	blast_command(Blast_Command),
	blast_input_file(Blast_Input),
	blast_output_file(XML_Blast_File),
	copy_fasta(Chunk_Stream,Blast_Input,Headline_Chunk,Query,ChunkTerminated,CopyFasta_OK),
	(CopyFasta_OK = 'yes' ->
            atom_concat(' -query ',Blast_Input, Arg1),
            atom_concat(' -out ',XML_Blast_File, Arg2),
            atom_concat(Arg1,Arg2,Args),
            atom_concat(Blast_Command,Args,Command),
            writeq(Command),
            system(Command,Status)
	;
            Status = 2
	).

% copy_fasta(++Infile,--OutFile,--Headline_Chunk,--ChunkTerminated,CopyFasta_OK)
copy_fasta(Infile,OutFile,Headline_Chunk,Seq_codes,ChunkTerminated,OK):-
	read_line(Infile,[62|Headline_Chunk]),
	read_line(Infile,Seq_codes),
	read_line(Infile,_),
	atom_codes(Title_Line,[62|Headline_Chunk]),
	atom_codes(Seq_Line,Seq_codes),
	(append(_,[42],Seq_codes) ->
            ChunkTerminated = 'yes'
        ;
            ChunkTerminated = 'no'), % check for ChunkTerminated to adjust trailing annotaions
			(Seq_Line \= 'n/a' ->
            open(OutFile,write,Output),
            writeln(Output,Title_Line),
            writeln(Output,Seq_Line),
            close(Output),
            OK = 'yes'
	;
            OK = 'no'
	).

% parser_headline_fasta(++Headline_Chunk,--QId,--Left,--Right,--Start_ORF)
parser_headline_fasta(Headline_Chunk,QId,Left,Right,Start_ORF) :-
        parser_line(Headline_Chunk,Entry_Token), % Token = [Qid,Left,Right,DirFrame,(ORF_Start)]
        (length(Entry_Token,5) ->
            Entry_Token = [QId,Left,Right,_,Start_ORF]
        ;
            Entry_Token = [QId,Left,Right,_]
        ).
        


%----------
% Utils cons_main
%----------

% determine_best_alns(++All_alignments,--Best_alignments)
% Computes the best hit for each database genome among all allignments 
% No more used
%determine_best_alns([Q|All_alignments],[Q|Best_alignments]):-
%	score_alns(All_alignments,Best_alignments).

% determine_best_alns(++All_alignments,--Best_alignments).
%-----------------------------------------------------------------------------------
% scores a list of alignments (from tBlastn)and keeps the highest scoring for each unique DB-ID
% args: All_alignments: list of unscored allignments each of which have the form (ID,P1,P2,M),where:
%			ID is a database ID
%			P1 and P2 are start- and end position of the alignment in the resepective genome
%			 M is the alignment sequence (of amino acids)
%	Best_alignments: list of highest scoring members of All_alignments for each unique ID.
%===================================================================================
determine_best_alns([],[]).
determine_best_alns([(ID,P11,P12,M)|Rest1],[(ID,P31,P32,M3)|Rest3]):-
        score(M,M1,_,Score1),
        best_of_rest(Rest1,ID,P21,P22,M2,Rest2,Score2),
      	(Score1 > Score2 ->
            P31 = P11,
            P32 = P12,
            M3  = M1
	;
            P31 = P21,
            P32 = P22,
            M3	= M2
	),
	determine_best_alns(Rest2,Rest3).

% best_of_rest(++All_Allignments,--Id_Best,--Pos_Left_Best,--Pos_Right_Best,--Rest_All_Allignments,--Score_Best)
best_of_rest(Rest1,ID,P31,P32,M3,Rest3,Score3):-
	(select((ID,P11,P12,M),Rest1,Rest2)->
            score(M,M1,_,Score1),
            best_of_rest(Rest2,ID,P21,P22,M2,Rest3,Score2),
            (Score1 > Score2 ->
                P31 = P11,
                P32 = P12,
                M3 	= M1,
                Score3 is Score1
            ;
                P31 = P21,
                P32 = P22,
                M3 	= M2,
                Score3 is Score2
            )
	;
            P31 = 'n/a',
            P32 = 'n/a',
            M3 = 'n/a',
            Score3 = 0,
            Rest3 = Rest1
	).

% score(++Match,--Match_Modified,--Saw_a_Stop,--Score)
% replaces terminated subseqs in L1 with a seq of blanks in L2, and copies the rest of L1 to L2,
% computes a score for the adjusted matches and keeps the best for each unique DBID
%=============================================================================================
score([],[],0,0) :-
        !.

score([42],[42],0,Score) :- % Stop as final char is LEGAL, i.e., report "no illegal stops".
        sc(42,Score),
        !.

score([X|L1],[Y|L2],Saw_a_stop,Score):-
	score(L1,L2,Saw_a_stop_in_rest,ScoreRest),
	( X \= 42, Saw_a_stop_in_rest = 0 ->
            Y = X,
            Saw_a_stop = 0 % copies X to Y and reports "no illegal stops" if X is not a '*' AND "no illegal stops" in rest
	;
            Y = 32,
            Saw_a_stop = 1 % replaces X by ' ' and reports "illegal stop(s)" oherwisel.
	),
	sc(Y,ScoreY),   % predicate defined in blosum62scores
	Score is ScoreRest + ScoreY.


% compute_conservation(++Query,++Best_Alignments,--Cons_Annot,--Cons_Score)
% Arguments:
%   Arg1, Query format list of Ascii code
%   Arg2, Alignments
%   Arg3, Conservation_List
%   Arg4, normalised average conservation pr position
%=========================
% Alignments, is a list DB_Alignments, where
% 		DB_Alignments is a list of DB_Alignments each of which has the form (DBId, Start, Stop, DB_Sequence)
%
% Conservation_List is a list of length equal to Query of integres 0-n, reflecting for each position ion the query the number of DB-sequences having a match at this position
% Conservation/position : (divide by # DBseqs (8) to get avg-conservation as a percentage).
%=========================
compute_conservation(Query,DB_Aligns, Cons, AvgCons):-
        length(Query,AlignmentLength),
	query_vs_dbs(Query,DB_Aligns,1,Cons,SummedCons),
	AvgCons is SummedCons / AlignmentLength.

 


% query_vs_dbs(++QuerySeq,++DB_Best_Hits,++Position,--Cons_Annot,--Acumulated_Score)
query_vs_dbs([],_DB_Best_Hits,_Position,[],0) :-
        !.

query_vs_dbs([Qhead|Qtail],DB,Position,[Chead|Ctail],SummedCons):-
	qhead_vs_dbheads(Qhead,DB,Position,DBtails,Chead),
        Position1 is Position+1,
	query_vs_dbs(Qtail,DBtails,Position1,Ctail,CtailSum),
	SummedCons is Chead + CtailSum. % Before adding: divide Chead by # dbseqs to get percentage-meassure


% query_vs_dbheads(++Query_Symbol,++DB_List,--DB_List_Tails,--Symbol_Matchs)

% end of DBs
qhead_vs_dbheads(_Qhead,[],_Position,[],0) :-
        !.                    

% Case: Qhead has not been aligned with DB
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[_DBhead|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQ):-
        Position < Start,
        !,                  
	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQ).


% Case: Qhead has not been aligned with DB
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[_DBhead|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQ):-
        Stop < Position,
        !,                  
	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQ).


% Case Perfect Match with the first DB
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[Qhead|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQ):-
        !,                  
	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQRest), 
	CountQ is 1 + CountQRest. % increment match count

% Case Perfect Match: Qhead is 'X' and DBhead is legal stop '*'
qhead_vs_dbheads(88,[(DBID,Start,Stop,[42|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQ):-
        !,                  
	qhead_vs_dbheads(88,RestDBs,Position,RestTails,CountQRest), 
	CountQ is 1 + CountQRest. % increment match count

% Case: Mismatch a space in DB
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[32|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQRest):-
       Qhead \= 88,
	!,
	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQRest).

% Case: Mismatch a - in DB
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[45|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQRest):-
	!,
	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQRest).

% Case: Mismatch in DB, but 1 either 0 is added given the option mismatch_score
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[_DBhead|TailDB1])|RestDBs],Position,[(DBID,Start,Stop,TailDB1)|RestTails],CountQ):-
        !,
       	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQRest),
	nongap_mismatch_score(MMS), % Option mismatch_score of the model
        CountQ is MMS + CountQRest.

% Case: no DB Left
qhead_vs_dbheads(Qhead,[(DBID,Start,Stop,[])|RestDBs],Position,[(DBID,Start,Stop,[])|RestTails],CountQRest):-  
	qhead_vs_dbheads(Qhead,RestDBs,Position,RestTails,CountQRest).





% Options Report alignment: Report into a file the best alignments find into cons_main
% : report_alns(Aln_Stream,FirstPos,LastPos,Alns,Alns2)
report_alns(Aln_Stream,AFirst,ALast,Alns,Alns2):-
	% write(user_output,'before '),writeln(user_output,Alns),
	% writeln(user_output,'----------------------------------------------'),
	% write(user_output,'after '),writeln(user_output,Alns2),
	% writeln(user_output,'=============================================='),
	set_output(Aln_Stream),
	write(AFirst),write(','),
	write(ALast),write(','),
	nl,
	write(' Full Alignments:'), nl,
	write(' ----------------'), nl,
	% writeln(user_output,'report alns1'),
	( Alns \= 'n/a' ->
            report_aln(Alns)
        ;
            writeln('n/a')
	),
        % writeln(user_output,'report alns2 ok'),
	write(' Alignments with removed terminated prefixes:'), nl,
	write(' --------------------------------------------------------------------'), nl,
	( Alns2 \= 'n/a' ->
            report_aln(Alns2)
        ;
            writeln('n/a')
	),
	% writeln(user_output,'report alns3 ok'),
	nl,
	set_output(user_output)
	.

report_aln([(_,_,_,QSeq)|DBs]):- % Aln = [Query|DBs]
        (QSeq \= 'n/a' ->
            atom_codes(QuerySequence,QSeq),
            write('Query    ,'),write(QuerySequence),nl, % Query = (QueryID,QuerySequence)
            report_DBs(DBs)
        ;
            write('Query    ,'),write('n/a'),nl
        ), % DBs = [DB|DBsRest]
        nl. % DB = (DBID,DBSequence)

report_DBs([]).
report_DBs([(DBId,_,_,DBSeq)|DBsRest]):-
	atom_codes(DBIdAtom,DBId),
	(DBSeq \= 'n/a' ->
            atom_codes(DBAtom,DBSeq),
            write(DBIdAtom),write(','),write(DBAtom),nl
	;
            write(DBIdAtom),write(','),write('n/a'),nl
	),
	report_DBs(DBsRest).


%---
% Diverse utils (no more used ??)
%---

makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
	N > 0,
	M is N-1,
	makelist(M,X,Rest).

extract(QId,Dir,QName,P1,P3,Pfx):-  % QId pattern : ec100k_115/142-255_AA
        atom_codes(QId,List_QId),
	append(QId_codes,[95|Body1],List_QId),
        append(P1_codes,[47|Body2],Body1),
	append(P2_codes,[45|Body3],Body2),
	append(P3_codes,[95|_],Body3),
	atom_codes(QName,QId_codes),
	number_codes(P1,P1_codes),
	number_codes(P2,P2_codes),
	number_codes(P3,P3_codes),
	( Dir = '+' ->
            (P2 = P1 -> Pfx is 0
            ;
		Pfx is P2 - P1 -1 % -1 added
            )
	;
            (
              P1 = P2 -> Pfx is 0
            ;
              Pfx is P3 - P2 + 1 % orignal +1 added

            )
	).

times_3([],[]).
times_3([H|Rest],[H,H,H|Rest_times_3]):-
	times_3(Rest,Rest_times_3).

toggle(alnout):-
	(
	alnout(true)-> retract_all(alnout(_)),assert(alnout(false))
	;
	retract_all(alnout(_)),assert(alnout(true))
	).


%==========================================================================
% Testgoals

close_files:-
	(close(blast_in);true),
	(close(blast_out);true),
	(close(chunk_in);true),
	(close(cons_out);true).


test:- conservation('test_multihits_tx1.fst',+,1,'testalns','testcons').
testna:- conservation('testna.fst',+,1,'testNAalns','testNAcons').
testYidD:-	conservation('ec3880k_cnk_+2_tx1.fst',+,2,'testYidDalns','testYidDcons').
testYidD2:-	conservation('testYidD2.fst',+,2,'testYidDalns2','testYidDcons2').
testYidD3:-	conservation('YidD_chunk_+2_test_tx1.fst',+,2,'testYidDalns3','testYidDcons3').

test2:- conservation('test2-AA','test2-cons').


%cons_main_new(Input_Stream,IDs,FirstPos,LastPos,Aln_Stream,Cons)
test3:- open(tblastn_test,read,Blast_Stream,[alias(blast_in)]),
        open(alntest_out,write,AlnOut_Stream,[alias(aln_out)]) ,
        parse_blast_init(Blast_Stream,_,IDs),
        cons_main_new(Blast_Stream,IDs,FirstPos,LastPos,AlnOut_Stream,Cons),
        write('FirstPos :'), writeln(FirstPos),
        write('LastPos'),writeln(LastPos),
        close(blast_in),
        close(aln_out),
        writeln(user_output,Cons).


testgoal:-run_chunk_conservation('u00096-500',[direction(+),frame(1),mode(0),nmScore(1),genecodefile('genecode11.pl')],Output), write('Output :'),writeln(Output).

testlist1([73,83,75,86,84,83,73,67,65,77,80,82,71,65,71,119,73,80,83,82,76,78]).
testlist2([32,32,32,86,84,83,73,67,65,77,45,82,71,65,71,119,73,80,83,82,76,78]).
testlist3([73,83,75,86,84,83,73,67,65,77,42,82,71,65,71,119,73,80,83,82,76,78]).

testlist4([77,71,75,73,73,71,73,68,76,71,84,84,78,83,67,86,65,73,77,68,71,84,84,80,82,86,76,69,78,65,69,71,68,82,84,84,80,83,73,73,65,89,84,81,68,71,69,84,76,86,71,81,80,65,75,82,81,65,86,84,78,80,81,78,84,76,70,65,73,75,82,76,73,71,82,82,70,81,68,69,69,86,81,82,68,86,83,73,77,80,70,75,73,73,65,65,68,78,71,68,65,87,86,69,86,75,71,81,75,77,65,80,80,81,73,83,65,69,86,76,75,75,77,75,75,84,65,69,68,89,76,71,69,80,86,84,69,65,86,73,84,86,80,65,89,70,78,68,65,81,82,81,65,84,75,68,65,71,82,73,65,71,76,69,86,75,82,73,73,78,69,80,84,65,65,65,76,65,89,71,76,68,75,71,84,71,78,82,84,73,65,86,89,68,76,71,71,71,84,70,68,73,83,73,73,69,73,68,69,86,68,71,69,75,84,70,69,86,76,65,84,78,71,68,84,72,76,71,71,69,68,70,68,83,82,76,73,78,89,76,86,69,69,70,75,75,68,81,71,73,68,76,82,78,68,80,76,65,77,81,82,76,75,69,65,65,69,75,65,75,73,69,76,83,83,65,81,81,84,68,86,78,76,80,89,73,84,65,68,65,84,71,80,75,72,77,78,73,75,86,84,82,65,75,76,69,83,76,86,69,68,76,86,78,82,83,73,69,80,76,75,86,65,76,81,68,65,71,76,83,86,83,68,73,68,68,86,73,76,86,71,71,81,84,82,77,80,77,86,81,75,75,86,65,69,70,70,71,75,69,80,82,75,68,86,78,80,68,69,65,86,65,73,71,65,65,86,81,71,71,86,76,84,71,68,86,75,68,86,76,76,76,68,86,84,80,76,83,76,71,73,69,84,77,71,71,86,77,84,84,76,73,65,75,78,84,84,73,80,84,75,72,83,81,86,70,83,84,65,69,68,78,81,83,65,86,84,73,72,86,76,81,71,69,82,75,82,65,65,68,78,75,83,76,71,81,70,78,76,68,71,73,78,80,65,80,82,71,77,80,81,73,69,86,84,70,68,73,68,65,68,71,73,76,72,86,83,65,75,68,75,78,83,71,75,69,81,75,73,84,73,75,65,83,83,71,76,78,69,68,69,73,81,75,77,86,82,68,65,69,65,78,65,69,65,68,82,75,70,69,69,76,86,81,84,82,78,81,71,68,72,76,76,72,83,84,82,75,81,86,69,69,65,71,68,75,76,80,65,68,68,75,84,65,73,69,83,65,76,84,65,76,69,84,65,76,75,71,69,68,75,65,65,73,69,65,75,77,81,69,76,65,81,86,83,81,75,76,77,69,73]).
testlist5([32,32,32,32,32,71,73,68,76,71,84,84,78,83,108,86,65,116,86,114,83,103,113,97,69,84,76,97,68,104,69,71,114,72,108,108,80,83,86,86,104,89,113,81,81,71,72,83,45,86,71,121,100,65,82,116,78,65,65,108,68,116,97,78,84,73,115,83,86,75,82,76,77,71,82,115,76,97,68,45,45,73,81,81,114,121,112,104,76,80,89,81,70,113,65,83,69,78,71,108,112,109,73,69,84,97,65,103,108,76,110,80,118,82,86,83,65,68,73,76,75,97,76,97,97,114,65,116,69,97,76,65,103,101,76,100,103,86,86,73,84,86,80,65,89,70,68,68,65,81,82,81,71,84,75,68,65,65,82,76,65,71,76,72,86,108,82,76,76,78,69,80,84,65,65,65,73,65,89,71,76,68,83,71,113,45,69,103,86,73,65,86,89,68,76,71,71,71,84,70,68,73,83,73,76,82,76,83,82,45,45,45,45,103,86,70,69,86,76,65,84,71,71,68,83,97,76,71,71,68,68,70,68,104,108,76,97,68,89,73,114,69,81,45,45,45,45,97,71,73,112,100,82,83,68,110,114,86,81,82,69,76,108,68,65,65,105,97,65,75,73,97,76,83,68,65,68,83,86,116,86,78,86,45,45,45,45,65,103,119,113,71,45,45,45,45,45,45,69,73,83,82,101,81,70,78,69,76,73,97,112,76,86,75,82,84,76,108,97,99,82,114,65,76,75,68,65,71,86,69,65,68,69,86,108,69,86,86,77,86,71,71,83,84,82,86,80,76,86,82,69,82,86,71,69,70,70,71,82,112,80,108,116,83,73,68,80,68,75,86,86,65,73,71,65,65,73,81,65,100,73,76,86,71,78,107,112,68,100,86,73,112,108,115,108,103,76,69,116,109,103,103,108,86,101,107,86,73,112,114,110,116,84,105,112,86,65,114,97,113,100,102,84,116,102,107,68,103,113,116,97,109,83,105,104,86,109,113,103,101,114,69,108,118,81,100,99,114,83,108,97,82,102,97,108,82,103,105,112,97,108,112,97,103,71,97,72,105,114,118,116,70,81,86,100,97,68,103,108,108,83,118,116,97,109,101,75,83,116,103,118,69,65,83,105,81,118,75,112,83,121,103,108,84,68,83,101,105,97,83,109,73,75,100,115,77,115,121,65,69,113,68,86,75,65,114,109,108,97,69,81,107,86,69,65,97,82,118,108,69,115,76,104,103,65,108,97,97,68,65,97,108,108,83,97,97,101,114,113,118,105,100,100,65,97,97,104,108,115,101,86,97,81,103,100,100,118,100,97,105,69,113,65,73,75,110,118,100,75,81,116,113,68,102,65,97,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]).
testlist511([32,32,32,32,32,71,73,68,76,71,84,84,78,83,108,86,65,116,86,114,83,103,113,97,69,84,76,97,68,104,69,71,114,72,108,108,80,83,86,86,104,89,113,81,81,71,72,83,45,86,71,121,100,65,82,116,78,65,65,108,68,116,97,78,84,73,115,83,86,75,82,76,77,71,82,115,76,97,68,45,45,73,81,81,114,121,112,104,76,80,89,81,70,113,65,83,69,78,71,108,112,109,73,69,84,97,65,103,108,76,110,80,118,82,86,83,65,68,73,76,75,97,76,97,97,114,65,116,69,97,76,65,103,101,76,100,103,86,86,73,84,86,80,65,89,70,68,68,65,81]).
testlist5121([82,81,71,84,75,68,65,65,82,76,65,71,76,72,86,108,82,76,76,78,69,80,84,65,65,65,73,65,89,71,76,68,83,71,113,45,69,103,86,73,65,86,89,68,76,71,71,71,84,70,68,73,83,73,76,82,76,83,82,45,45,45,45,103,86,70,69,86,76,65,84,71,71,68,83,97,76,71]).
testlist512211([68,89,73,114,69,81,45,45,45,45,71,68,68,70,68,104,108,76,97]).
testlist512212([97,71,73,112,100,82,83,68,110,114,86,81,82,69,76,108,68,65,65,105]).
testlist51222([97,65,75,73,97,76,83,68,65,68,83,86,116,86,78,86,45,45,45,45,65,103,119,113,71,45,45,45,45,45,45,69,73,83,82,101,81,70,78,69,76,73,97,112,76,86,75,82,84,76,108,97,99,82,114,65]).
testlist52([76,75,68,65,71,86,69,65,68,69,86,108,69,86,86,77,86,71,71,83,84,82,86,80,76,86,82,69,82,86,71,69,70,70,71,82,112,80,108,116,83,73,68,80,68,75,86,86,65,73,71,65,65,73,81,65,100,73,76,86,71,78,107,112,68,100,86,73,112,108,115,108,103,76,69,116,109,103,103,108,86,101,107,86,73,112,114,110,116,84,105,112,86,65,114,97,113,100,102,84,116,102,107,68,103,113,116,97,109,83,105,104,86,109,113,103,101,114,69,108,118,81,100,99,114,83,108,97,82,102,97,108,82,103,105,112,97,108,112,97,103,71,97,72,105,114,118,116,70,81,86,100,97,68,103,108,108,83,118,116,97,109,101,75,83,116,103,118,69,65,83,105,81,118,75,112,83,121,103,108,84,68,83,101,105,97,83,109,73,75,100,115,77,115,121,65,69,113,68,86,75,65,114,109,108,97,69,81,107,86,69,65,97,82,118,108,69,115,76,104,103,65,108,97,97,68,65,97,108,108,83,97,97,101,114,113,118,105,100,100,65,97,97,104,108,115,101,86,97,81,103,100,100,118,100,97,105,69,113,65,73,75,110,118,100,75,81,116,113,68,102,65,97,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]).

testlist6([32,32,32,32,73,71,73,68,76,71,84,84,78,83,108,73,65,86,119,107,100,103,65,97,81,76,73,112,78,107,102,71,69,121,108,84,80,83,73,73,83,109,100,69,78,78,72,105,76,86,71,75,80,65,118,115,82,114,84,83,72,80,68,75,84,97,97,108,70,75,82,97,77,71,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,83,78,116,110,87,45,82,76,103,83,68,116,70,110,97,80,69,76,83,83,108,86,76,82,83,76,75,69,100,65,69,69,70,76,113,82,80,73,107,68,86,86,73,83,86,80,65,89,70,83,68,101,81,82,75,104,84,82,108,65,65,69,76,65,71,76,78,65,118,82,76,73,78,69,80,84,65,65,65,77,65,89,71,76,104,116,113,113,78,84,82,83,76,45,86,70,68,76,71,71,71,84,70,68,86,84,86,76,69,121,45,45,45,45,65,116,112,86,73,69,86,104,65,83,97,71,68,78,102,76,71,71,69,68,70,116,104,109,76,86,68,101,86,76,75,45,45,82,97,68,118,65,114,116,116,108,78,69,115,45,101,76,97,97,76,121,97,67,86,69,97,65,75,45,45,45,45,99,83,78,81,83,112,76,72,73,114,87,113,121,113,69,101,84,45,45,45,45,114,69,99,69,102,121,69,110,69,76,69,68,76,119,108,112,76,76,78,82,108,114,118,80,73,69,113,65,76,82,68,65,114,76,75,112,83,81,73,68,83,76,86,76,86,71,71,97,83,81,77,80,76,86,81,82,105,65,86,82,76,70,71,75,108,80,121,81,83,121,68,80,83,116,105,86,65,97,65,67,114,76,82,83,101,100,73,101,101,118,73,108,116,100,73,99,112,121,115,108,103,86,69,86,78,114,81,103,116,116,118,112,86,83,114,86,101,84,121,83,84,109,104,80,101,81,100,83,105,84,118,78,86,121,81,103,69,78,104,107,86,107,110,78,73,76,118,101,83,102,100,118,112,108,107,75,116,71,97,121,81,115,105,100,45,45,45,45,105,114,102,115,108,101,118,68,86,108,76,101,100,103,115,118,107,83,114,86,73,78,104,83,112,118,116,108,83,65,113,81,105,69,69,115,114,116,82,108,83,65,108,107,105,121,112,82,100,77,45,45,45,45,45,45,45,45,45,45,108,105,78,82,116,70,75,97,107,76,119,65,82,45,45,45,97,108,103,100,69,114,101,69,105,105,84,68,102,100,65,97,108,113,115,110,68,109,97,114,86,100,69,118,114,114,114,65,115,68,121,108,97,105,101,105,112,42,84,114,76,112,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]).
testlist7([32,32,32,70,73,71,70,68,121,71,84,65,78,99,115,86,65,86,77,114,100,103,107,80,72,76,76,75,109,101,78,100,83,116,108,108,80,83,77,76,67,45,65,112,116,114,69,65,86,115,101,119,108,121,82,72,72,100,86,112,97,100,68,68,101,116,113,65,76,108,82,114,97,105,82,121,110,82,69,69,68,73,68,118,116,65,75,115,86,113,70,103,76,115,83,108,112,75,83,102,108,103,65,45,115,103,108,75,112,81,65,113,108,112,101,65,101,97,110,116,81,97,81,103,105,108,101,82,97,97,107,114,97,103,102,114,68,86,86,70,113,121,101,112,118,97,108,118,86,68,105,103,71,109,103,112,113,119,82,115,114,108,100,82,101,97,115,76,76,71,72,115,103,67,114,105,103,71,110,100,76,68,105,65,108,65,102,75,78,76,109,112,45,108,76,71,109,71,103,101,116,101,75,103,73,97,76,69,116,114,97,83,108,112,102,73,83,101,115,65,108,83,113,112,108,116,114,105,114,119,97,101,118,118,105,99,76,82,108,115,113,102,102,99,99,108,116,42,114,70,104,113,82,72,103,80,99,119,115,77,97,100,99,81,114,114,102,81,112,109,115,102,103,71,104,102,82,72,121,114,76,83,86,99,87,114,121,45,45,45,45,45,45,82,102,73,82,112,69,104,107,116,65,102,116,97,114,70,112,118,68,110,114,108,103,81,77,118,97,107,112,102,104,76,108,78,65,116,45,45,45,84,103,110,105,99,103,99,70,110,116,107,82,112,42,114,114,70,84,82,82,97,110,65,103,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]).


testalns([(db1,1,20,[73,83,75,86,84,83,73,67,65,77,80,82,71,65,71,119,73,80,83,82,76,78]),
					(db1,40,60,[32,32,32,86,84,83,73,67,65,77,45,82,71,65,71,119,73,80,83,82,76,78]),
					(db1,80,100,[73,83,75,86,84,83,73,67,65,77,42,82,71,65,71,119,73,80,83,82,76,78]),
					(db2,1,20,[73,83,75,86,84,83,73,67,65,77,80,82,71,65,71,119,73,80,83,82,76,78]),
					(db3,40,60,[32,32,32,86,84,83,73,67,65,77,45,82,71,65,71,119,73,80,83,82,76,78]),
					(db3,80,100,[73,83,75,86,84,83,73,67,65,77,42,82,71,65,71,119,73,80,83,82,76,78])
					]).
testalns2([
					([78,67,95,48,48,48,57,49,51],12163,13965,[77,71,75,73,73,71,73,68,76,71,84,84,78,83,67,86,65,73,77,68,71,84,84,80,82,86,76,69,78,65,69,71,68,82,84,84,80,83,73,73,65,89,84,81,68,71,69,84,76,86,71,81,80,65,75,82,81,65,86,84,78,80,81,78,84,76,70,65,73,75,82,76,73,71,82,82,70,81,68,69,69,86,81,82,68,86,83,73,77,80,70,75,73,73,65,65,68,78,71,68,65,87,86,69,86,75,71,81,75,77,65,80,80,81,73,83,65,69,86,76,75,75,77,75,75,84,65,69,68,89,76,71,69,80,86,84,69,65,86,73,84,86,80,65,89,70,78,68,65,81,82,81,65,84,75,68,65,71,82,73,65,71,76,69,86,75,82,73,73,78,69,80,84,65,65,65,76,65,89,71,76,68,75,71,84,71,78,82,84,73,65,86,89,68,76,71,71,71,84,70,68,73,83,73,73,69,73,68,69,86,68,71,69,75,84,70,69,86,76,65,84,78,71,68,84,72,76,71,71,69,68,70,68,83,82,76,73,78,89,76,86,69,69,70,75,75,68,81,71,73,68,76,82,78,68,80,76,65,77,81,82,76,75,69,65,65,69,75,65,75,73,69,76,83,83,65,81,81,84,68,86,78,76,80,89,73,84,65,68,65,84,71,80,75,72,77,78,73,75,86,84,82,65,75,76,69,83,76,86,69,68,76,86,78,82,83,73,69,80,76,75,86,65,76,81,68,65,71,76,83,86,83,68,73,68,68,86,73,76,86,71,71,81,84,82,77,80,77,86,81,75,75,86,65,69,70,70,71,75,69,80,82,75,68,86,78,80,68,69,65,86,65,73,71,65,65,86,81,71,71,86,76,84,71,68,86,75,68,86,76,76,76,68,86,84,80,76,83,76,71,73,69,84,77,71,71,86,77,84,84,76,73,65,75,78,84,84,73,80,84,75,72,83,81,86,70,83,84,65,69,68,78,81,83,65,86,84,73,72,86,76,81,71,69,82,75,82,65,65,68,78,75,83,76,71,81,70,78,76,68,71,73,78,80,65,80,82,71,77,80,81,73,69,86,84,70,68,73,68,65,68,71,73,76,72,86,83,65,75,68,75,78,83,71,75,69,81,75,73,84,73,75,65,83,83,71,76,78,69,68,69,73,81,75,77,86,82,68,65,69,65,78,65,69,65,68,82,75,70,69,69,76,86,81,84,82,78,81,71,68,72,76,76,72,83,84,82,75,81,86,69,69,65,71,68,75,76,80,65,68,68,75,84,65,73,69,83,65,76,84,65,76,69,84,65,76,75,71,69,68,75,65,65,73,69,65,75,77,81,69,76,65,81,86,83,81,75,76,77,69,73]),
					([78,67,95,48,48,48,57,49,51],2656891,2655167,[32,32,32,32,32,71,73,68,76,71,84,84,78,83,108,86,65,116,86,114,83,103,113,97,69,84,76,97,68,104,69,71,114,72,108,108,80,83,86,86,104,89,113,81,81,71,72,83,45,86,71,121,100,65,82,116,78,65,65,108,68,116,97,78,84,73,115,83,86,75,82,76,77,71,82,115,76,97,68,45,45,73,81,81,114,121,112,104,76,80,89,81,70,113,65,83,69,78,71,108,112,109,73,69,84,97,65,103,108,76,110,80,118,82,86,83,65,68,73,76,75,97,76,97,97,114,65,116,69,97,76,65,103,101,76,100,103,86,86,73,84,86,80,65,89,70,68,68,65,81,82,81,71,84,75,68,65,65,82,76,65,71,76,72,86,108,82,76,76,78,69,80,84,65,65,65,73,65,89,71,76,68,83,71,113,45,69,103,86,73,65,86,89,68,76,71,71,71,84,70,68,73,83,73,76,82,76,83,82,45,45,45,45,103,86,70,69,86,76,65,84,71,71,68,83,97,76,71,71,68,68,70,68,104,108,76,97,68,89,73,114,69,81,45,45,45,45,97,71,73,112,100,82,83,68,110,114,86,81,82,69,76,108,68,65,65,105,97,65,75,73,97,76,83,68,65,68,83,86,116,86,78,86,45,45,45,45,65,103,119,113,71,45,45,45,45,45,45,69,73,83,82,101,81,70,78,69,76,73,97,112,76,86,75,82,84,76,108,97,99,82,114,65,76,75,68,65,71,86,69,65,68,69,86,108,69,86,86,77,86,71,71,83,84,82,86,80,76,86,82,69,82,86,71,69,70,70,71,82,112,80,108,116,83,73,68,80,68,75,86,86,65,73,71,65,65,73,81,65,100,73,76,86,71,78,107,112,68,100,86,73,112,108,115,108,103,76,69,116,109,103,103,108,86,101,107,86,73,112,114,110,116,84,105,112,86,65,114,97,113,100,102,84,116,102,107,68,103,113,116,97,109,83,105,104,86,109,113,103,101,114,69,108,118,81,100,99,114,83,108,97,82,102,97,108,82,103,105,112,97,108,112,97,103,71,97,72,105,114,118,116,70,81,86,100,97,68,103,108,108,83,118,116,97,109,101,75,83,116,103,118,69,65,83,105,81,118,75,112,83,121,103,108,84,68,83,101,105,97,83,109,73,75,100,115,77,115,121,65,69,113,68,86,75,65,114,109,108,97,69,81,107,86,69,65,97,82,118,108,69,115,76,104,103,65,108,97,97,68,65,97,108,108,83,97,97,101,114,113,118,105,100,100,65,97,97,104,108,115,101,86,97,81,103,100,100,118,100,97,105,69,113,65,73,75,110,118,100,75,81,116,113,68,102,65,97,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]),
					([78,67,95,48,48,48,57,49,51],682595,680934,[32,32,32,32,73,71,73,68,76,71,84,84,78,83,108,73,65,86,119,107,100,103,65,97,81,76,73,112,78,107,102,71,69,121,108,84,80,83,73,73,83,109,100,69,78,78,72,105,76,86,71,75,80,65,118,115,82,114,84,83,72,80,68,75,84,97,97,108,70,75,82,97,77,71,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,83,78,116,110,87,45,82,76,103,83,68,116,70,110,97,80,69,76,83,83,108,86,76,82,83,76,75,69,100,65,69,69,70,76,113,82,80,73,107,68,86,86,73,83,86,80,65,89,70,83,68,101,81,82,75,104,84,82,108,65,65,69,76,65,71,76,78,65,118,82,76,73,78,69,80,84,65,65,65,77,65,89,71,76,104,116,113,113,78,84,82,83,76,45,86,70,68,76,71,71,71,84,70,68,86,84,86,76,69,121,45,45,45,45,65,116,112,86,73,69,86,104,65,83,97,71,68,78,102,76,71,71,69,68,70,116,104,109,76,86,68,101,86,76,75,45,45,82,97,68,118,65,114,116,116,108,78,69,115,45,101,76,97,97,76,121,97,67,86,69,97,65,75,45,45,45,45,99,83,78,81,83,112,76,72,73,114,87,113,121,113,69,101,84,45,45,45,45,114,69,99,69,102,121,69,110,69,76,69,68,76,119,108,112,76,76,78,82,108,114,118,80,73,69,113,65,76,82,68,65,114,76,75,112,83,81,73,68,83,76,86,76,86,71,71,97,83,81,77,80,76,86,81,82,105,65,86,82,76,70,71,75,108,80,121,81,83,121,68,80,83,116,105,86,65,97,65,67,114,76,82,83,101,100,73,101,101,118,73,108,116,100,73,99,112,121,115,108,103,86,69,86,78,114,81,103,116,116,118,112,86,83,114,86,101,84,121,83,84,109,104,80,101,81,100,83,105,84,118,78,86,121,81,103,69,78,104,107,86,107,110,78,73,76,118,101,83,102,100,118,112,108,107,75,116,71,97,121,81,115,105,100,45,45,45,45,105,114,102,115,108,101,118,68,86,108,76,101,100,103,115,118,107,83,114,86,73,78,104,83,112,118,116,108,83,65,113,81,105,69,69,115,114,116,82,108,83,65,108,107,105,121,112,82,100,77,45,45,45,45,45,45,45,45,45,45,108,105,78,82,116,70,75,97,107,76,119,65,82,45,45,45,97,108,103,100,69,114,101,69,105,105,84,68,102,100,65,97,108,113,115,110,68,109,97,114,86,100,69,118,114,114,114,65,115,68,121,108,97,105,101,105,112,42,84,114,76,112,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]),
					([78,67,95,48,48,48,57,49,51],2145701,2147440,[32,32,32,70,73,71,70,68,121,71,84,65,78,99,115,86,65,86,77,114,100,103,107,80,72,76,76,75,109,101,78,100,83,116,108,108,80,83,77,76,67,45,65,112,116,114,69,65,86,115,101,119,108,121,82,72,72,100,86,112,97,100,68,68,101,116,113,65,76,108,82,114,97,105,82,121,110,82,69,69,68,73,68,118,116,65,75,115,86,113,70,103,76,115,83,108,112,75,83,102,108,103,65,45,115,103,108,75,112,81,65,113,108,112,101,65,101,97,110,116,81,97,81,103,105,108,101,82,97,97,107,114,97,103,102,114,68,86,86,70,113,121,101,112,118,97,108,118,86,68,105,103,71,109,103,112,113,119,82,115,114,108,100,82,101,97,115,76,76,71,72,115,103,67,114,105,103,71,110,100,76,68,105,65,108,65,102,75,78,76,109,112,45,108,76,71,109,71,103,101,116,101,75,103,73,97,76,69,116,114,97,83,108,112,102,73,83,101,115,65,108,83,113,112,108,116,114,105,114,119,97,101,118,118,105,99,76,82,108,115,113,102,102,99,99,108,116,42,114,70,104,113,82,72,103,80,99,119,115,77,97,100,99,81,114,114,102,81,112,109,115,102,103,71,104,102,82,72,121,114,76,83,86,99,87,114,121,45,45,45,45,45,45,82,102,73,82,112,69,104,107,116,65,102,116,97,114,70,112,118,68,110,114,108,103,81,77,118,97,107,112,102,104,76,108,78,65,116,45,45,45,84,103,110,105,99,103,99,70,110,116,107,82,112,42,114,114,70,84,82,82,97,110,65,103,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32])
					]).

testalns3([
					([78,67,95,48,48,48,57,49,51],12163,13965,		[77,71,75]),
					([78,67,95,48,48,48,57,49,51],2656891,2655167,[32,32,75]),
					([78,67,95,48,48,48,57,49,51],682595,680934,	[32,32,32]),
					([78,67,95,48,48,48,57,49,51],2145701,2147440,[32,70,73])
					]).


clean:-	close(aln_out),close(cons_out).
