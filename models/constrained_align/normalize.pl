
normalize_alignments(Inputs,Outputs,[RangeMin,RangeMax],Inverted) :-
	alignment_statistics(Inputs,Small,Large,Total),
	Range is RangeMax - RangeMin,
	open(InputFile,read,InStream),
	open(OutputFile,write,OutStream),
	renorm(Range,Inverted,InStream,OutStream),
	close(InStream),
	close(OutStream).

invert()	
	
normalize_streams(Range,Invert,InStream,OutStream) :-
	read(Stream,Term),
	((Term == end_of_file) ->
		true
		;
		Term = [ Cost, A, B ],
		((Cost==inf) ->
			UpdCost = 1
			;
			UpdCost1 is Cost / Range,
			(Invert -> UpdCost is 1 - UpdCost1 ; UpdCost = UpdCost1),
			write(OutStream,[UpdCost,A,B]),
			write('.\n'))).

alignment_statistics(Stream,Smallest,Largest,Total) :-
	read(Stream,Term),
	((Term == end_of_file) ->
		true
		;
		alignment_statistics(Stream,SmallestRest,LargestRest,TotalRest),
		Term = [ Cost, _A, _B ],
		((Cost==na) ->
			[Smallest,Largest,Total] = [SmallestRest,LargestRest,TotalRest]
			;
			Total is Total + TotalRest,
			((Cost>LargestRest) ->
				Largest = Cost
				;
				Largest = LargestRest),
			((Cost<SmallestRest) ->
				Smallest = Cost
				;
				Smallest = SmallestRest))).
	