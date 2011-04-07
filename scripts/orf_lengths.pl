%%%%%%%
% Convert facts from the orf_chopper format (which contains multiple starts per fact)
% to the range facts used everywhere else

:- ['../lost.pl'].

orfs_to_range_facts(OrfFile,RangeFactFile) :-
	open(OrfFile,read,InStream),
	open(RangeFactFile,write,OutStream),
	read_and_convert_orf(0,InStream,OutStream),
	close(InStream),
	close(OutStream).

read_and_convert_orf(Counter,InStream,OutStream) :-
	Counter1 is Counter + 1,
	((0 is Counter1 mod 1000) ->
		write('procesed '), write(Counter1), write(' orfs.\n')
		;
		true
	),
	read(InStream,ORF),
	((ORF == end_of_file) ->
		true
	;
		(	(orf_to_range_facts(ORF,RangeFacts),
			extract_gene_lengths(RangeFacts,Lengths),
			forall(member(L,Lengths),(write(OutStream,L),write(OutStream,'\n'))))
			;
			(write('error processing ORF: '),write(ORF),nl)
		),
		!,
		read_and_convert_orf(Counter1,InStream,OutStream)).

extract_gene_lengths([],[]).
extract_gene_lengths([G|Gs],[L|Ls]) :-
	G =.. [_Func,_Id,Start,End,_Strand,_Frame,_extra],
	NucleotideLength is abs(Start - End),
	L is NucleotideLength // 3,
	extract_gene_lengths(Gs,Ls).

orf_to_range_facts(ORF,RangeFacts) :-
	ORF =.. [_Functor,SeqId,_Left,_Right,Dir,Frame,Extra],
	member(starts(Starts), Extra),
	member(stop([Stop]), Extra),
	findall(RangeFact,(	member(Start,Starts),
						RangeFact=range_fact(SeqId,Start,Stop,Dir,Frame,[])),
			RangeFacts).
			
test :-
	lost_sequence_file('ecoli_all_orfs',OrfFile),
	lost_sequence_file('ecoli_orf_lengths',RangeFactsFile),
	orfs_to_range_facts(OrfFile,RangeFactsFile).
