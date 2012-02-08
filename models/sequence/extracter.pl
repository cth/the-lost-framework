:- use(fasta).
:- use(lists).

extract_from_sequence(FastaFile,Left,Right,Extracted) :-
	fasta_load_sequence(FastaFile,_,_,Sequence),
	region_from_circular_list(Left,Right,Sequence,Extracted).

region_from_circular_list(Left,Right,Sequence,Extracted) :-
		Left =< Right,
		Pivot is Left - 1,
		split_list(Pivot,Sequence,_LeftOfExtract,LeftToEnd),
		ToTake is 1 + Right - Left,
		take(ToTake,LeftToEnd,Extracted).
		
region_from_circular_list(Left,Right,Sequence,Extracted) :-
		Left > Right,
		length(Sequence,SequenceLength),
		split_list(Left,Sequence,_,LeftToTerminus),
		take(Right,Sequence,TerminusToRight),
		append(LeftToTerminus,TerminusToRight,Extracted).
