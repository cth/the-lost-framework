:- use(fasta).
:- use(lists).

extract_from_sequence(FastaFile,Left,Right,Extracted) :-
	fasta_load_sequence(Fasta,_,_,Sequence),
	region_from_circular_list(Left,Right,Sequence,Extracted).

region_from_circular_list(Left,Right,Sequence,Extracted) :-
		Left =< Right,
		split_list(Left,_LeftOfExtract,LeftToEnd),
		ToTake is 1 + Right - Left,
		take(ToTake,LeftToEnd,Extracted).
		
region_from_circular_list(Left,Right,Sequence,Extracted) :-
		Left > Right,
		length(Sequence,SequenceLength),
		BeforeRight is SequenceLength - Left,
		split_list(BeforeLeft,_,LeftToTerminus),
		take(Right,Sequence,TerminusToRight),
		append(LeftToTerminus,TerminuesToRight,Extracted).
