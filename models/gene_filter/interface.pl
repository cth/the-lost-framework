% Gene filter model
% This model filters away some genes given a set of genes
:- ['../../lost.pl'].

:- lost_include_api(interface).
:- lost_include_api(regex).
:- lost_include_api(io).
:- lost_include_api(genecode).
:- lost_include_api(sequence).

lost_option(lost_best_annotation,
	    match_extra_fields,
	    [],
	    'A list of regular field names in the extra and regular expressions to match those fields. If a gene is matched by one of these regular expressions, then it is a candidate for the output.').

lost_option(lost_best_annotation,
	    no_match_extra_fields,
	    [],
	    'A list of field names in the extra and regular expressions to match those fields. If a gene is matched by any of these regular expression, it will be filtered and will not occur in the output.').

lost_option(lost_best_annotation,
	    match_protein_coding,
	    no,
	    'Specifies that the gene should start with a valid start codon and end with a valid stop codon').

%lost_option(lost_best_annotation,


lost_option(lost_best_annotation,
	    genecode,
	    11,
	    'The gene code to used. This is used to determine which genes are protein coding').

	    

lost_option(lost_best_annotation,
	    range_min,
	    min,
	    'The minimum of the range of ganes to be included.').

/*
lost_option(lost_best_annotation,
	    invert_results,
	    no,
	    'When this option is set to \'yes\', then the filtered genes will appear in the output instead of non-filtered genes').
*/

lost_option_values(lost_best_annotation, match_protein_coding, [yes,no]).
lost_option_values(lost_best_annotation,gene_code,[11]). % FIXME: add more..

lost_best_annotation([GeneListFile,GeneDataFile], Options, OutFile) :-
	write('lost_best_annotation called.'),nl,
	
	terms_from_file(GeneListFile,GeneTerms),
	
	% Filter out any genes that do not match all of the specified
	% regular expressions in match_extra_fields
	write('Filtering genes not matching match_extra_fields'),nl,
	get_option(Options,match_extra_fields,NoMatchRegexExtraFields),
	regex_filter_non_matching(NoMatchRegexExtraFields,GeneTerms,Filtered1),

	% Filter out genes matching any of the regular expressions
	% specified by no_match_extra_fields
	write('Filtering genes matching no_match_extra_fields'),nl,
	get_option(Options,no_match_extra_fields,MatchRegexExtraFields),
	regex_filter_matching(MatchRegexExtraFields,Filtered1,Filtered2),

	write('loading genome sequence..'),nl,
	load_sequence(data_sequence,GeneDataFile),
	write('done'),nl,

	gene_ranges(GeneTerms,GeneRanges),
	
	get_option(Options,genecode,GeneCode),

	write('gene code:'),
	write(GeneCode),
	nl,

	write('Created protein regex matcher'),nl,
	
	filter_non_protein_coding(GeneCode,data_sequence,Filtered2,Filtered3),
	
	% Write genes that where not filtered out to the output file:
	terms_to_file(OutFile,Filtered2).

/*
range_first_and_last_triplet([],[]).

range_first_and_last_triplet(Min,Max,RangeFirst,RangeLast) :-
	Min3 is Min + 2,
	Max3 is Max - 2,
	RangeFirst = [ Min, Min3 ],
	RangeLast = [ Max3, Max ].
	range_first_and_last_triplet(RangesRest,TripletRangeRest).
*/

gene_ranges([],[]).
gene_ranges([GeneTerm|GenesRest], [Range|RangesRest]) :-
	GeneTerm =.. [ _functor, Start, End, _strand, _frame, _extraList ],
	Range = [ Start, End ],
	gene_ranges(GenesRest,RangesRest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% regex_filter_non_matching(+RegexList, +GeneTermList, -MatchedGeneTermList).
% Only terms from GeneTermList matching all the regular expressions in RegexList
% are kept in MatchedGeneTermList
regex_filter_non_matching(_,[],[]).
regex_filter_non_matching(RegexList,[GeneTerm|GeneTermsRest],[GeneTerm|MatchedGeneRest]) :-
	forall(member(Regex,RegexList), regex_match_geneterm(Regex,GeneTerm)),
	write('match: '),
	write(GeneTerm),
	nl,
	!,
	regex_filter_non_matching(RegexList,GeneTermsRest,MatchedGeneRest).

regex_filter_non_matching(RegexList,[_|GeneTermsRest],MatchedGeneRest) :-
	regex_filter_non_matching(RegexList,GeneTermsRest,MatchedGeneRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% regex_filter_matching(+RegexList, +GeneTermList, -UnmatchedGeneTermList).
% Only terms that do not match any of the regular expression s in RegexList
% are kepts in UnmatchedGeneTerms
regex_filter_matching(_,[],[]).

regex_filter_matching(RegexList,[GeneTerm|GeneTermsRest],UnmatchedGeneRest) :-
	member(Regex,RegexList),
	regex_match_geneterm(Regex,GeneTerm),
	!,
	regex_filter_matching(RegexList,GeneTermsRest,UnmatchedGeneRest).

regex_filter_matching(RegexList,[GeneTerm|GeneTermsRest],[GeneTerm|UnmatchedGeneRest]) :-
	regex_filter_matching(RegexList,GeneTermsRest,UnmatchedGeneRest).

% regex_match_geneterm(+Regex, +GeneTerm)
% True if the regular expression Regex matches GeneTerm
regex_match_geneterm(RegexCondition,GeneTerm) :-
	RegexCondition =.. [ Functor, RegexAtom ],
	GeneTerm =.. [ _functor, _start, _end, _strand, _frame, ExtraList ],
	FunctorMatcher =.. [ Functor, Data ],
	member(FunctorMatcher,ExtraList),
	re_compile(RegexAtom,Regex),
	re_match(Regex,Data,_matches).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_non_protein_coding(+ProteinRegexMatch,+GenomeDataTerms,+GeneTerms,-MatchedGeneTerms)
filter_non_protein_coding(_,_,[],[]).

filter_non_protein_coding(GeneCode,GenomeSeqId,[GeneTerm|GenesRest],[GeneTerm|MatchedRest]) :-
	%get_gene_nucleotides(GeneTerm,GenomeSeqId,Sequence),
	get_gene_start_and_stop_sequence(GeneTerm,GenomeSeqId,Sequence),
	GeneTerm =.. [ _functor, _start, _end, Strand, _frame, _extraList ],
	% Get the gene data starting
	((Strand = '+') ->
	 SequenceForward = Sequence,
	 reverse(Sequence,SequenceBackward)
	;
	 dna_seq_complement(Sequence,SequenceComplemented),
	 SequenceBackward = SequenceComplemented,
	 reverse(SequenceComplemented,SequenceForward)
	),

	% Write as atom
	write(SequenceForward),nl,

	% Extract start codon
	SequenceForward = [ Start1,Start2,Start3 | _ ],
	SequenceBackward = [ Stop3, Stop2, Stop1 | _ ],
	StartCodon = [Start1,Start2,Start3],
	StopCodon = [Stop1,Stop2,Stop3],

	write('start: '), write(StartCodon),nl,
	write('stop: '), write(StopCodon),nl,

	genecode_start_codon(GeneCode,StartCodon),
	genecode_stop_codon(GeneCode,StopCodon),
		
	% Try to match gene with regex
	%protein_gene_regex_matcher(GeneCode, RegexMatcher),
	%re_match(RegexMatcher,DataCodesForward,_),
	!,
	filter_non_protein_coding(GeneCode,GenomeSeqId,GenesRest,MatchedRest).

filter_non_protein_coding(GeneCode,GenomeSeqId,[GeneTerm|GenesRest], [MatchedRest]) :-
	write('This gene does not look like a real protein:'),
	writeq(GeneTerm),nl,
	!,
	filter_non_protein_coding(GeneCode,GenomeSeqId,GenesRest,MatchedRest).

get_gene_nucleotides(GeneTerm,GenomeSequenceId,NucleotideData) :-
	GeneTerm =.. [ _functor, Min, Max, Strand, _frame, _extra ],
	write(get_sequence_range(GenomeSequenceId,Min,Max,NucleotideData)),nl,
	get_sequence_range(GenomeSequenceId,Min,Max,NucleotideData).


get_gene_start_and_stop_sequence(GeneTerm,GenomeSequenceId,NucleotideData) :-
	GeneTerm =.. [ _functor, Min, Max, Strand, _frame, _extra ],
	write(get_sequence_range(GenomeSequenceId,Min,Max,NucleotideData)),nl,
	FirstMax is Min + 2,
	LastMin is Max - 2,
	get_sequence_range(GenomeSequenceId,Min,FirstMax,StartCodon),
	get_sequence_range(GenomeSequenceId,LastMin,Max,EndCodon),
	append(StartCodon,EndCodon,NucleotideData).
	
% Assumes each atom to be exactly one character
atom_list_code_list([],[]).
atom_list_code_list([Atom|AtomsRest],[Code|CodesRest]) :-
	atom_codes(Atom, [Code]),
	atom_list_code_list(AtomsRest,CodesRest).