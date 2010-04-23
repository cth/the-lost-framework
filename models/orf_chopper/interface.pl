:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(io).

lost_input_formats(lost_best_annotation, [text(prolog(sequence(_)))]).
lost_output_format(lost_best_annotation, _Options, text(prolog(ranges(gene)))).

% This is what is used to get the best annotation
% requires direction (+/-) and frame (1,2,3)
lost_best_annotation([Sequence_File],Options,Orf_Chunk_File) :-
	write('LoSt orf chopper: '),nl,
				lost_required_option(Options,direction,Dir),
				lost_required_option(Options,frame,Frame),
				% write(lost_best_annotation(ParamFile,[OrfFile,ConsFile],Options,OutputFile)),nl,
				% lost_required_option(Options,parameter_file,ParamFile),
				cl('orf_chopper.pl'), % Load the actual PRISM model
				% write('parameters loaded'),nl,
			  open(Sequence_File,read,InputStream,[alias(seqin)]),
				open(Orf_Chunk_File,write,OutputStream,[alias(chunkout)]),
				read(seqin,data(Id,StartPos,_,S)),
				(Dir = + -> DirFactor = 1 ; DirFactor = -1),
				dna_chop_init(S,StartPos,DirFactor,Frame,_,Id,OutputStream),
				read(InputStream,_), % to close the file properly
				close(InputStream),
				close(OutputStream),
  			write('LoSt orf-chopper completed succesfully'),nl.




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
