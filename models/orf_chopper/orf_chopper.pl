/*******************************************************************************
* orf_chopper.pl
* version 003
* Ole Torp Lassen
* 18.03.10 : tested successfully on ecoli 100k and verfied with genebank
*
* 1 orf/chunk Chunking strategy for all frames in both directions
*
* Nonprobabilistic Main predicate : orfchop(Infile,Frame,Dir,ChunkOut,AnnOut,FastaOut).
* - Infile expected to contain a file of one or more data instances:
*		data(Id,StartPos,List). 
*		 where List is a list of nucleotides.
* 
* - (NEW) This version is sensitive to frames and attempts to apply the 
*   chopping scheme to any of the 3 forward and 3 reverse reading frames.
*		It has been tested sucessfully on the first 100000 bases of Ecoli, acc.nr: nc000913
*
* - direction of frame must be as + (direct strand) or - reverse strand
*		
* - ChunkOut will contain a prolog fact for each subsequences in the data:
*		chunk(Id, Start, End, Dir, Frame, SubSequence, Type, StartCodons).
*
* - AnnOut will contain a prolog fact for each subsequences in the data:
*		potorfs(Id, Start, End, Dir, Frame, Annotation).
*
*		 where Type is orf / rst, Annotation is a linear representation of the parse, StartCodons is a list of startcodons in the sub-sequence.
*
*	- FastaOut is a file contaning the same information as above in FASTA format.	
*
*===============================================================================
* The top-layer of a multilayered model for analysis of DNA-sequences. 
* This dominationg layer divides a single strand of DNA in Gene preserving
* subsequences. 
* - This version divides immediately after the stop codon in any complete ORF,
* 	When no complete ORF can be found in a subseqeunce it is returned unchanged.
* - This version include simple annotations of start and stop codons as well as 
* 	distinction between in and out of ORF's
* - also positions for alternative start codons are collected
********************************************************************************/

/*******************************************************************************
* transitions for main model: dna-model
* states: dna(0),orf, rest
*===============================================================================
* FSA for distinction of subsequences in a single reading-frame of DNA
* In this model there are two kinds of subseqeunces:
* -	orf-sequences: those that includes one complete ORF, ending by the stop codon of that ORF. These will make up the bulk of any DNA sequence analysis.
* -	rest-seqeucnes: those that does not include any ORF. In praxis this reduces to the ONE sequence following the last ORF-sequence in a given reading-frame
* ORF-sequecnes and REST-sequences are defined in separate models below.
********************************************************************************/

values(dna(start),	[orf, rst]).
values(trans(orf),	[orf, rst, end]).
values(trans(rst),	[end]).


/*******************************************************************************
* transitions for sub-model 1 : rest-sequences
* states: orf, preStart(orf), start(orf), preStop(orf), stop(orf), end
* These seqeunces all includes one complete longest possible ORF
*===============================================================================
* 
********************************************************************************/

values(orf,										[preStart(orf),		start(orf)															 ]).
values(trans(preStart(orf)),	[preStart(orf),		start(orf)]).
values(trans(start(orf)),			[																preStop(orf), 		stop(orf)]).
values(trans(preStop(orf)),		[																preStop(orf),			stop(orf)]).
values(trans(stop(orf)),			[																																end]).

/*******************************************************************************
* transitions for sub-model 2 : rest-sequences
* states: rest, preStart(rest), start(rest), preStop(rest),n2(rest), n1(rest), end
* These seqeunces are all lacking one or more ORF constituents
*===============================================================================
*
********************************************************************************/

values(rst,										[preStart(rst),	start(rst),	 										  n(2)						 ]).
values(trans(preStart(rst)),	[preStart(rst),	start(rst), 											n(2), 				end]).
values(trans(start(rst)),		[																preStop(rst),				n(2), 				end]).
values(trans(preStop(rst)),	[																preStop(rst),				n(2),					end]).
values(trans(n(2)),						[																													n(1),	end]).
values(trans(n(1)),						[																																end]).

/*******************************************************************************
* emissions for both sub-models : subsequnces-sequences
*===============================================================================
* - preStart(_): 	emits any one triplet but a start triplet
* - start(_):			emits any one start triplet
* - preStop(_):		emits any one triplet but a stop triplet
* - stop(_):			emits any one stop triplet
* - n(_):					emits one nucleotide
********************************************************************************/
values(emit(preStart(_)),[	[t,t,t],[t,c,t],[t,a,t],[t,g,t],
														[t,t,c],[t,c,c],[t,a,c],[t,g,c],
														[t,t,a],[t,c,a],[t,a,a],[t,g,a],
																		[t,c,g],[t,a,g],[t,g,g],
												
														[c,t,t],[c,c,t],[c,a,t],[c,g,t],
														[c,t,c],[c,c,c],[c,a,c],[c,g,c],
														[c,t,a],[c,c,a],[c,a,a],[c,g,a],
																		[c,c,g],[c,a,g],[c,g,g],
												
																		[a,c,t],[a,a,t],[a,g,t],
																		[a,c,c],[a,a,c],[a,g,c],
																		[a,c,a],[a,a,a],[a,g,a],
																		[a,c,g],[a,a,g],[a,g,g],
												
														[g,t,t],[g,c,t],[g,a,t],[g,g,t],
														[g,t,c],[g,c,c],[g,a,c],[g,g,c],
														[g,t,a],[g,c,a],[g,a,a],[g,g,a],
																		[g,c,g],[g,a,g],[g,g,g]			]).
												
values(emit(start(_)),[			[t,t,g],[c,t,g],[a,t,t],[a,t,c],[a,t,a],[a,t,g],[g,t,g]]).

values(emit(preStop(_)),[		[t,t,t],[t,c,t],[t,a,t],[t,g,t],       
														[t,t,c],[t,c,c],[t,a,c],[t,g,c],       
														[t,t,a],[t,c,a],       
														[t,t,g],[t,c,g],				[t,g,g],       
												                                       
														[c,t,t],[c,c,t],[c,a,t],[c,g,t],       
														[c,t,c],[c,c,c],[c,a,c],[c,g,c],       
														[c,t,a],[c,c,a],[c,a,a],[c,g,a],       
														[c,t,g],[c,c,g],[c,a,g],[c,g,g],       
												                                       
														[a,t,t],[a,c,t],[a,a,t],[a,g,t],       
														[a,t,c],[a,c,c],[a,a,c],[a,g,c],       
														[a,t,a],[a,c,a],[a,a,a],[a,g,a],       
														[a,t,g],[a,c,g],[a,a,g],[a,g,g],       
												                                       
														[g,t,t],[g,c,t],[g,a,t],[g,g,t],       
														[g,t,c],[g,c,c],[g,a,c],[g,g,c],       
														[g,t,a],[g,c,a],[g,a,a],[g,g,a],       
														[g,t,g],[g,c,g],[g,a,g],[g,g,g]			]).
												
values(emit(stop(_)),[			[t,a,a],[t,g,a],[t,a,g]]).

values(emit(n(_)),					[a,c,t,g]).


/*******************************************************************************
* non-probabilistic parser for both sub-models model: subsequences
*===============================================================================
*/


sub_parse(end,_Dir,S-S, 												P,	P,  B-B,   E-E).
%------------------------------------------------------------------------------
sub_parse(preStart(T),Dir,[N1,N2,N3|S2]-S3,			P1,	P3, B2-B3, E2-E3):-
	nonprob_msw(emit(preStart(T)),[N1,N2,N3]),
																								P2 is P1+(3*Dir),
	nonprob_msw(trans(preStart(T)),Next),
	sub_parse(Next,Dir,S2-S3,											P2, P3, B2-B3, E2-E3).		
%------------------------------------------------------------------------------
sub_parse(preStop(T),Dir,[N1,N2,N3|S2]-S3, 			P1, P3, B1-B3, E2-E3):-
	nonprob_msw(emit(preStop(T)),[N1,N2,N3]),
																						(
																						startcodon([N1,N2,N3])->
																								(Dir = 1 -> P is P1+1	;	P is P1	),
																								B1 = [P|B2] 
																						; 
																						  	B1 = B2
																						),																						
																						P2 is P1+(3*Dir),
	nonprob_msw(trans(preStop(T)),Next),
	sub_parse(Next,Dir,S2-S3,											P2, P3, B2-B3, E2-E3).		
%------------------------------------------------------------------------------
sub_parse(start(T),Dir,[N1,N2,N3|S2]-S3,				P1,	P3, [P|B2]-B3, E2-E3):-
	nonprob_msw(emit(start(T)),[N1,N2,N3]),
																						 		P2 is P1+(3*Dir),
																								(Dir = 1 -> P is P1+1	;	P is P1	),
	nonprob_msw(trans(start(T)),Next),
	sub_parse(Next,Dir,S2-S3,											P2, P3, B2-B3, E2-E3).		
%------------------------------------------------------------------------------
sub_parse(stop(T),Dir,[N1,N2,N3|S2]-S3,					P1,	P3, B2-B3, [P|E2]-E3):-
	nonprob_msw(emit(stop(T)),		[N1,N2,N3]),
																								P2 is P1+(3*Dir),
																								(Dir = 1 -> P is P1+1	;	P is P1	),
	nonprob_msw(trans(stop(T)),Next),
	sub_parse(Next,Dir,S2-S3,											P2, P3, B2-B3, E2-E3).
%------------------------------------------------------------------------------	
sub_parse(n(Num),Dir,[N|S2]-S3,									P1,	P3, B2-B3, E2-E3):-
	nonprob_msw(emit(n(Num)),	N),																						
																								P2 is P1+(1*Dir),	
	nonprob_msw(trans(n(Num)),Next),
	sub_parse(Next,Dir,S2-S3,											P2, P3, B2-B3, E2-E3).			
%-----------------------------------------------------------------------------
sub_parse(This, Dir,S1-S2, 											P1, P2, B1-B2, E2-E3):-
	subtype(This),
	nonprob_msw(This,FirstState),
	sub_parse(FirstState,Dir, S1-S2,							P1, P2, B1-B2, E2-E3).
%-----------------------------------------------------------------------------
startcodon(Cod):-
				values(emit(start(_)),StartCodons), 
				member(Cod,StartCodons),!.

subtype(orf).
subtype(rst).

/****************************************************************************
* Run Predicates TOOLS
****************************************************************************/
/**************************************************************************************/
% Recusive call of the prediction routine
%
/*******************************************************************************
* non-probabilistic parser for main model: dna model
*
* Direction must be listed as 1 for direct and -1 for reverse
*
* add direction to  all predicates
*
*===============================================================================
*/

dna_chop_init(S,SeqStartPos,Dir,Frame, L,Id,ChunkFileOutStream):-	
	Pm3 is SeqStartPos mod 3,
	init_frame(Pm3, Frame,Offset,S,S2), 
	ChunkStartPos = SeqStartPos + Offset,
	( Dir = -1 ->
		reverse_complement(S2,[],S3,ChunkStartPos,SeqEndPos),																								
		Trailing is (SeqEndPos - ChunkStartPos)mod 3,																								
		nFirst(Trailing,S3,_Firstfew,S4),								%cutoff(Trailing,_,S3,S4),		
		LeftPos is SeqEndPos - Trailing,																								
		RightPos is ChunkStartPos
	;
		S4 = S2,
		LeftPos is ChunkStartPos,
		L = RightPos
	),	
	nonprob_msw(dna(start),FirstState),	
	dna_chop(FirstState,Dir,Frame,S4-[],				LeftPos, RightPos, Id,ChunkFileOutStream).
	

dna_chop(end,_Dir,_Frm,S-S, 									P,	P, _Id,_Chunk).
dna_chop(This,Dir,Frame,S1-S2,								P1, P2, Id,ChunkFileOutStream):-	
	This \= end,
	sub_parse(This,Dir,S1-S3,										P1, P3, Begins-[],Ends-[]),		% emit a sequence of type This
	(Dir = 1 -> Dir_symbol = '+'; Dir_symbol = '-'), 
	report(Id,P1,P3,S1,Dir_symbol,Frame,Begins,Ends,ChunkFileOutStream),
	nonprob_msw(trans(This),Next),
	dna_chop(Next,Dir,Frame,S3-S2,							P3, P2,Id,ChunkFileOutStream).

%======================================================================================
% Tools
%======================================================================================
% init_frame/5
%--------------------------------------------------------------------------------------
% new version, that takes into regard the modulo 3 of the sequence start position Pm3
% computes chunk-position offset and adjusts the seqeunce according to Frame
%
%          Pm3, F, 	Offset, List1, 	List2
init_frame(1,		1,	0,			S,			S).
init_frame(1,		2,	1,			[_|S],	S).
init_frame(1,		3,	2,			[_,_|S],S).

init_frame(2,		1,	2,			[_,_|S],S).
init_frame(2,		2,	0,			S,			S).
init_frame(2,		3,	1,			[_|S],	S).

init_frame(0,		1,	1,			[_|S],	S).
init_frame(0,		2,	2,			[_,_|S],S).
init_frame(0,		3,	0,			S,			S).

%======================================================================================
% reverse_complement/5
% complement/2
%--------------------------------------------------------------------------------------
% reverses and complements the sequence
% 
reverse_complement([X|Y],Z,W,P1,L) :- 					
	P2 is P1+1,
	complement(X,Xc),
	reverse_complement(Y,[Xc|Z],W,P2,L).

reverse_complement([],X,X,L,L).
	
complement(a,t).
complement(t,a).
complement(c,g).
complement(g,c).			

%--------------------------------------------------------------------------------------
% nfirst/4
%--------------------------------------------------------------------------------------
% Given an integer n and a list L returns two lists, L2 and L3 where L2 is the n first elements of L and L3 is the rest of L
%
nFirst(0,L1,[],L1).
nFirst(N,[X|T1],[X|T2],Remaining):-
	N2 is N-1,
	nFirst(N2,T1,T2,Remaining).

%--------------------------------------------------------------------------------------
% nonprob_msw/2
%--------------------------------------------------------------------------------------
% non-probabilistic version of PRISM-msw using membership relation
%
nonprob_msw(S,V):-
	values(S,Range),
	member(V,Range).	

%--------------------------------------------------------------------------------------
% report(Dir_symbol,Frame,S1,P1,P3,Begins,Stops,Id,ChunkFileOutStream),
%--------------------------------------------------------------------------------------
% Outputs current chunk to the output file
%
report(Id,P1,P2,S1,Dir,Frame,Begins,Ends,ChunkFileOutStream):-
	(Dir = '+' -> 
		N is P2-P1,	Left is P1 ,	Right is P2-1  % this was added /*+1*/
		;
		N is P1-P2,	Left is P2 , Right is P1-1 % this was added /*+1*/
	), 
	nFirst(N,S1,S,_),
	(Dir = '+' ->
		S2 = S
		;
		reverse_complement(S,[],S2,0,_)
	),
	Entry =.. [chunk,Id,Left,Right,S,Dir,Frame,Begins,Ends],
	write(ChunkFileOutStream,Entry), writeln(ChunkFileOutStream,'.').	


	
%======================================================
% TESTGOALS
%======================================================
test(Dir,Frame):-
	open('U00096-500.pl',read,InputStream,[alias(seqin)]),
	open('testchunkfile',write,OutputStream,[alias(chunkout)]),
	read(InputStream,data(Id,StartPos,_,S)),
	% Term =.. [data(ID,StartPos,_,S),
	(Dir = + -> DirFactor = 1 ; DirFactor = -1),
	% orf_chop_main(Term,DirFactor,Frame,InputStream,OutputStream),
	dna_chop(S,StartPos,DirFactor,Frame,_,Id,OutputStream),
	read(InputStream,_), % to close the file properly
	close(seqin),
	close(chunkout),
  write('LoSt orf-chopper completed succesfully'),nl,!.
  
  
/*
orf_chop_main(end_of_file,_Dir,_Frm,_Fin,_Chunk).
orf_chop_main(data(Id,StartPos,_,S),Dir,Frame,FileInStream,ChunkFileOutStream):-
	dna_chop(S,StartPos,Dir,Frame,_,_, Id, ChunkFileOutStream),
	read(FileInStream,Term),
	chop_main(Term,Dir,Frame,FileInStream,ChunkFileOutStream).
*/
