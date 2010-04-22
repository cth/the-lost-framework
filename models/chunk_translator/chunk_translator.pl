%--------------------------------------------------------------------------------------------------
% chunk_translator.pl
%--------------------------------------------------------------------------------------------------
% translates a file of frame specific nukleotide chunks into a file of equivalent fastaformatted amino-acid sequences
%
%
%--------------------------------------------------------------------------------------------------
% Translation Modes:
% 	mode =1 : only orfs are translated
% 	mode =0 : entire chunk is translated
%--------------------------------------------------------------------------------------------------

chunk_translator(FileIn,Mode,AFastaOut):-
	chunk_translator_init(FileIn,FileInStream,AFastaOut,AFastaStream),
	read(FileInStream,Term),
	chunk_translator_main(Term,Mode,FileInStream, AFastaStream),
	chunk_translator_cleanup(FileInStream, AFastaStream).

chunk_translator_main(end_of_file,_Mode,_FileIn,_FastaOut).

% mode = 0 : entire chunk is translated
chunk_translator_main(chunk(Id,Start,End,Seq,Dir,Frm,_Starts,_Stops),0,	FileInStream,	AFastaStream):-
%write('0-0 raw              '),writeln(Seq),
	(
		Dir = '-' -> reverse_complement(Seq,[],SeqR,0,_Length)
	;
		SeqR = Seq
	),
%write('0-1 b4 translation '),writeln(SeqR),
	translate(SeqR,Product),
%write('0-2 af translation '),writeln(Product),
	report_prod(AFastaStream,Id,Start,Start,End,Dir,Frm,_Length,Product),
	read(FileInStream,Term),
	chunk_translator_main(Term,0,FileInStream,AFastaStream)
	.


% mode =1 : only orfs are translated
chunk_translator_main(chunk(Id,Start,End,Seq,Dir,Frm,Starts,Stops),1,	FileInStream,	AFastaStream):-
%write('0-0 raw              '),writeln(Seq),
	Stops \= [],
	Starts = [OrfStart|_Starts],
	(
		Dir = '-' -> reverse_complement(Seq,[],SeqR,0,_Length)
	;
	 	SeqR = Seq
	),
%write('0-1 b4 segment      '),writeln(SeqR),
	(
		Dir = '+' ->
			PreStartLength is OrfStart - Start -1 ,
			Length is End - OrfStart -1
	;
			PreStartLength is End - OrfStart  + 1,
			Length is End - Start +1
	),
	segment(PreStartLength,_PreStart,Orf,SeqR),
%write('0-2 b4 translation  '),writeln(Orf),
	translate(Orf,Product),
%write('1-3 af translation  '),writeln(Product),
	report_prod(AFastaStream,Id,Start,OrfStart,End,Dir,Frm, Length,Product),
	read(FileInStream,Term),
	chunk_translator_main(Term,1,FileInStream,AFastaStream).

chunk_translator_main(chunk(Id,Start,End,_Seq,Dir,Frm,_Starts,[]),1,	FileInStream,	AFastaStream):-
	Product = [],
	Length is 0,
	report_prod(AFastaStream,Id,Start,End,End,Dir,Frm,Length,Product),
	read(FileInStream,Term),
	chunk_translator_main(Term,1,FileInStream,AFastaStream).

chunk_translator_init(FileIn,FileInStream,AFastaOut,AFastaStream):-
	open(FileIn,read,FileInStream),
	open(AFastaOut,write,AFastaStream),
	set(report,on).

chunk_translator_cleanup(FileInStream,AFastaStream):-
	close(FileInStream),
	close(AFastaStream),
	set(report,off).

translate([],[]).
translate([_],[]).
translate([_,_],[]).
translate([N1,N2,N3|RestOrf],[A|RestAs]):-
	genecode([N1,N2,N3],A),
	translate(RestOrf,RestAs).

report_prod(AFastaStream,Id,Start,TranslationStart,End,Dir,Frm,_Length,Product):-
	set_output(AFastaStream),
	write('>'),
		write(Id),		write('_'),
		write(Start),write('/'),write(TranslationStart),	write('-'), 	write(End),		write('_'),
		write(Dir),write(Frm), write('_'),write('AA'),
		nl,
	(
	Product \= [] -> write_list(Product)
	;
	write('n/a')
	),
	nl,nl,
	set_output(user_output).

segment(PreStartLength,PreStart,Orf,Seq):-
	append(PreStart,Orf,Seq), length(PreStart,PreStartLength).


/**************************************************************************
*	flag for turning output on/off
*/

set(report,on):-
	retractall(report(_)),
	assert(report(on)).

set(report,off):-
	retractall(report(_)).

write_list([]).
write_list(['*'|Rest]):-
	write('*'),
	write_list(Rest).

write_list([X|Rest]):-
	X \='*',
	char_code(X,C),
	C2 is C -32,
	char_code(X2,C2),
	write(X2),
	write_list(Rest).
%========================================================
reverse_complement([X|Y],Z,W,P1,L) :- 					% reverses and complements the sequence
	P2 is P1+1,
	complement(X,Xc),
	reverse_complement(Y,[Xc|Z],W,P2,L).

reverse_complement([],X,X,L,L).

complement(a,t).
complement(t,a).
complement(c,g).
complement(g,c).

%=======================================================
% testgoals
%=======================================================
testgoal1(Mode):-
	chunk_translator('u00096-500_+1.gen',Mode,'u00096-500_+1-tx').

testgoal2(Mode):-
	chunk_translator('u00096-500_-1.gen',Mode,'u00096-500_-1-tx').
