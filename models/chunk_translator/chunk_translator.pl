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

chunk_translator(FileIn,Mode,GeneCode,SequenceFunctor,AFastaOut):-
	chunk_translator_init(FileIn,FileInStream,AFastaOut,AFastaStream),
	read(FileInStream,Term),
	chunk_translator_main(Term,Mode,GeneCode,SequenceFunctor,FileInStream, AFastaStream),
        writeln('**************'),
	chunk_translator_cleanup(FileInStream, AFastaStream).


% End of the file
chunk_translator_main(end_of_file,_Mode,_GeneCode,_FileIn,_FastaOut) :-
        !.

% mode = 0 : entire chunk is translated
%chunk_translator_main(chunk(Id,Left,Right,Dir,Frm,Extra_Infos),0,GeneCode,FileInStream,AFastaStream):-
chunk_translator_main(Term,0,GeneCode,SequenceFunctor,FileInStream,AFastaStream):-
        gene_extra_field(Term,SeqFun,Seq),
        !,
	translate(GeneCode,Seq,AA_Sequence),
        (AA_Sequence = [] ->
            true  % no report
        ;
            report_prod(AFastaStream,Id,Left,Right,Dir,Frm,AA_Sequence)
        ),
	read(FileInStream,Term),
	chunk_translator_main(Term,0,GeneCode,FileInStream,AFastaStream).


% mode = 1 : only orfs are translated
chunk_translator_main(chunk(Id,Left,Right,Dir,Frm,Extra_Infos),1,GeneCode,FileInStream,AFastaStream):-
        member(sequence(Seq),Extra_Infos),
        member(stop(Stops),Extra_Infos),
        member(starts([OrfStart|_]),Extra_Infos),
        Stops \= [],
        !,
        (Dir = '+' ->
            Prestart_Length is OrfStart-Left
        ;
            Prestart_Length is Right-OrfStart
        ),
        orf_computation(Prestart_Length,Seq,Orf),
        translate(GeneCode,Orf,AA_Sequence),
        (AA_Sequence = [] ->
            true % no report
        ;
            report_prod(AFastaStream,Id,Left,Right,Dir,Frm,AA_Sequence,OrfStart)
        ),
	read(FileInStream,Term),
	chunk_translator_main(Term,1,GeneCode,FileInStream,AFastaStream).

% Last chunk that is not a right ORF
chunk_translator_main(chunk(_Id,_Left,_Right,_Dir,_Frm,Extra_Infos),1,GeneCode,FileInStream,AFastaStream):-
        member(stop([]),Extra_Infos),
        !,  % No Report produced
        read(FileInStream,Term),
	chunk_translator_main(Term,1,GeneCode,FileInStream,AFastaStream).

chunk_translator_init(FileIn,FileInStream,AFastaOut,AFastaStream):-
	open(FileIn,read,FileInStream),
	open(AFastaOut,write,AFastaStream).


chunk_translator_cleanup(FileInStream,AFastaStream):-
	close(FileInStream),
	close(AFastaStream).



%%%%%%
% Utils chunk translator
%%%%%%%
% orf_computation(++Prestart_Length,++List,--Sublist)

orf_computation(0,L,L) :-
        !.

orf_computation(Length,[_Elt|Rest],Result) :-
        !,
        Length1 is Length-1,
        orf_computation(Length1,Rest,Result).

%segment(PreStartLength,PreStart,Orf,Seq):-
%	append(PreStart,Orf,Seq), length(PreStart,PreStartLength). Fun ;)


% translate(++GeneCode,++List_Nucleotides,--List_AA)
translate(_GeneCode,[],[]) :-
        !.

translate(_GeneCode,[_],[]) :-
        !.

translate(_GeneCode,[_,_],[]) :-
        !.

translate(GeneCode,[N1,N2,N3|RestOrf],[A|RestAs]):-
	genecode(GeneCode,[N1,N2,N3],A),
	translate(GeneCode,RestOrf,RestAs).

% Write in the Fasta Format the translated sequence
% In mode 0,
report_prod(AFastaStream,Id,Left,Right,Dir,Frm,AA_Sequence):-
	set_output(AFastaStream),
        % Headline
        write('>'),write(Id),write(' '),write(Left),write(' '),write(Right),write(' '),write(Dir),write(Frm),
        nl,
        % Data 
        write_list(AA_Sequence),
	nl,nl,
	set_output(user_output).

% Example of Headline in mode 0
% >u00096 1 48 +1
% SFSF*LQRAICLCVD*


% Write in the Fasta Format the translated sequence
% In mode 1,
report_prod(AFastaStream,Id,Left,Right,Dir,Frm,AA_Sequence,OrfStart):-
	set_output(AFastaStream),
        % Headline
	write('>'),write(Id),write(' '),write(Left),write(' '),write(Right),write(' '),write(Dir),write(Frm),write(' '),write(OrfStart), 
        nl,
        % Data
        write_list(AA_Sequence),
	nl,nl,
	set_output(user_output).



% Example of Headline in mode 1
% >u00096 1 48 +1 16
% LQRAICLCVD*


%%%/**************************************************************************
%%%*	flag for turning output on/off
%%%*/

%%%set(report,on):-
%%%	retractall(report(_)),
%%%	assert(report(on)).

%%%set(report,off):-
%%%	retractall(report(_)).


% write_list(++List_AA)
write_list([]) :-
        !.

write_list(['*'|Rest]):-
        !,
	write('*'),
	write_list(Rest).

write_list([X|Rest]):-
	X \='*',
        !,
	char_code(X,C),
	C2 is C -32,
	char_code(X2,C2),
	write(X2),
	write_list(Rest).

%%%%========================================================
%%%reverse_complement([X|Y],Z,W,P1,L) :- 					% reverses and complements the sequence
%%%	P2 is P1+1,
%%%	complement(X,Xc),
%%%	reverse_complement(Y,[Xc|Z],W,P2,L).

%%%reverse_complement([],X,X,L,L).

%%%complement(a,t).
%%%complement(t,a).
%%%complement(c,g).
%%%complement(g,c).

%%%%=======================================================
%%%% testgoals
%%%%=======================================================
%%%testgoal1(Mode):-
%%%	chunk_translator('u00096-500_+1.gen',Mode,'u00096-500_+1-tx').

%%%testgoal2(Mode):-
%%%	chunk_translator('u00096-500_-1.gen',Mode,'u00096-500_-1-tx').
