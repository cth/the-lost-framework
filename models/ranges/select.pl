% Gene filter model
% This model filters away some genes given a set of genes
:- ['../../lost.pl'].

:- use(regex).
:- use(genecode).
:- use(dnaseq).
:- use(lists).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Declaration of input and output formats:

lost_input_formats(annotate, [text(prolog(ranges(gene))),text(prolog(sequence))]).
lost_output_format(annotate, _, [text(prolog(ranges(gene)))]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Declaration of input and output formats:

lost_option(annotate,
	    match_frames,
	    [1,2,3],
	    'A list of valid frames. Genes in other frames will be filtered.').

lost_option(annotate,
	    match_strands,
	    ['-','+'],
	    'A list of valid strands. Genes occuring an other strand will be filtered.').

lost_option(annotate,
	    exact_match_extra_fields,
	    [],
	    'A list of Prolog terms terms each of which must unify with an element of the Extralist.').

lost_option(annotate,
	    exact_no_match_extra_fields,
	    [],
	    'A list of Prolog terms terms any of which must NOT unify with any element of the Extralist.').

lost_option(annotate,
	    regex_match_extra_fields,
	    [],
	    'A list of regular field names in the extra and regular expressions to match those fields. If a gene is matched by one of these regular expressions, then it is a candidate for the output.').

lost_option(annotate,
	    regex_no_match_extra_fields,
	    [],
	    'A list of field names in the extra and regular expressions to match those fields. If a gene is matched by any of these regular expression, it will be filtered and will not occur in the output.').

lost_option(annotate,
	    match_protein_coding,
	    no,
	    'Specifies that the gene should start with a valid start codon and end with a valid stop codon').

lost_option(annotate,
	    genecode,
	    11,
	    'The gene code to used. This is used to determine which genes are protein coding').

lost_option(annotate,
	    invert_results,
	    no,
	    'When this option is set to \'yes\', then the filtered genes will appear in the output instead of non-filtered genes').

lost_option(annotate,
            left,
            min,
            'An integer value denoting the left-most nucleotide to include in results. The default value (min) refers to the left-most nucleotide in the input data').

lost_option(annotate,
            right,
            max,
            'An integer value denoting the right-most nucleotide to include in results. The default value (min) refers to the right-most nucleotide in the input data').

lost_option(annotate,
            range,
            [default,default],
            'A range [RangeMin,RangeMax] used to filter genes with a length in this range').
            
lost_option_values(annotate,match_protein_coding, [yes,no]).
lost_option_values(annotate,gene_code,[11]). % FIXME: add more..
lost_option_values(annotate,insert_results,[yes,no]).

annotate([GeneListFile], Options, OutFile) :-
	annotate([GeneListFile,_], Options, OutFile).

annotate([GeneListFile,GeneDataFile], Options, OutFile) :-
	write('gene_filter: annotate called.'),nl,
	terms_from_file(GeneListFile,GeneTerms),

        length(GeneTerms,NumTerms),nl,
        write('before filtering: '), write(NumTerms), nl,
        report_number_filtered(GeneTerms),nl, 
        
        % Filter by Left position 
        get_option(Options,left,Left),
	write('filtering genes with left end < '), write(Left),nl,
        left_range_filter(Left,GeneTerms,RangeFilteredLeft),
        !,
	report_number_filtered(RangeFilteredLeft),

        % Filter by right position
        get_option(Options,right,Right),
	write('filtering genes with right end > '), write(Right),nl,
        right_range_filter(Right,RangeFilteredLeft,RangeFilteredLeftRight),
        !,
	report_number_filtered(RangeFilteredLeftRight),

	% Filter genes not matching specified frames
	get_option(Options,match_frames,MatchFrames),
	write('filtering genes which not in frame(s) '), write(MatchFrames),nl,
	!,
	filter_by_frames(MatchFrames,RangeFilteredLeftRight,GeneTermsMatchFrame),
	report_number_filtered(GeneTermsMatchFrame),

	% Filter genes no matching specified strands
	get_option(Options,match_strands,MatchStrands),
	write('filtering genes which not on strand(s) '), write(MatchStrands),nl,
	!,
	filter_by_strands(MatchStrands,GeneTermsMatchFrame,GeneTermsMatchStrand),
	report_number_filtered(GeneTermsMatchStrand),

        % Filter genes with a length in a specified Range
        get_option(Options,range,Range),
	write('filtering genes which a length in '), write(Range),nl,
	!,
	filter_by_length(Range,GeneTermsMatchStrand,GeneTermsFilterRange),
	report_number_filtered(GeneTermsFilterRange),
        
	% Filter out any genes that do not match all of the specified
	% regular expressions in match_extra_fields
	write('Filtering genes not matching regex_match_extra_fields'),nl,
	get_option(Options,regex_match_extra_fields,NoMatchRegexExtraFields),
	regex_filter_non_matching(NoMatchRegexExtraFields,GeneTermsFilterRange,MatchRegexNo),
	report_number_filtered(MatchRegexNo),	

	% Filter out genes matching any of the regular expressions
	% specified by no_match_extra_fields
	write('Filtering genes matching regex_no_match_extra_fields'),nl,
	get_option(Options,regex_no_match_extra_fields,MatchRegexExtraFields),
	regex_filter_matching(MatchRegexExtraFields,MatchRegexNo,MatchRegexYes),
	report_number_filtered(MatchRegexYes),
	
	% Exact match extra fields
	write('Filtering genes matching exact_match_extra_fields'),nl,	
	get_option(Options,exact_match_extra_fields,ExactMatchers),
	exact_filter_matching(ExactMatchers,MatchRegexYes,MatchExact1),
	report_number_filtered(MatchExact1),
	

	% Exact non_match extra fields
	write('Filtering genes not matching exact_no_match_extra_fields'),nl,
	get_option(Options,exact_no_match_extra_fields,ExactNoMatchers),
	exact_filter_non_matching(ExactNoMatchers,MatchExact1,MatchExact2),
	report_number_filtered(MatchExact2),
	
	MatchFixpoint1 = MatchExact2,
	
	% Filter out genes which are not protein coding if requested
	(get_option(Options,match_protein_coding,yes) ->
	 write('filtering non-protein coding genes'),nl,
	 write('loading genome sequence..'),nl,
	 load_sequence(data_sequence,GeneDataFile),
	 write('done'),nl,
	 get_option(Options,genecode,GeneCode),
	 write('gene code:'),
	 write(GeneCode),
	 nl,
	 filter_non_protein_coding(GeneCode,data_sequence,MatchFixpoint1,MatchProtein)
	;
	 write('NOT filtering non-protein coding genes'),nl,
	 MatchProtein=MatchFixpoint1),
	report_number_filtered(MatchProtein),
	MatchAll = MatchProtein, 

        % Invert results if requested.
        get_option(Options,invert_results,InvertResults),
        ((InvertResults == yes) ->
               subtract(GeneTerms,MatchAll,FinalResults)
               ;
               FinalResults=MatchAll
        ),
	% Write genes that where not filtered out to the output file:
	terms_to_file(OutFile,FinalResults).

report_number_filtered(GeneTerms) :-
	length(GeneTerms,L),
	write(L), write(' genes left unfiltered'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% left_range_filter(+Left,+Unfiltered,-Filtered)

left_range_filter(min,Terms,Terms).

left_range_filter(_,[],[]).

left_range_filter(Left,[Gene|Us],Fs) :-
        integer(Left),
		Gene =.. [ _functor, _Genome_Key, GeneLeft, _end, _strand, _frame, _extra ],
        Left > GeneLeft, 
        !,
        left_range_filter(Left,Us,Fs).
        
left_range_filter(Left,[Gene|Us],[Gene|Fs]) :-
        !,
        left_range_filter(Left,Us,Fs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% right_range_filter(+Right,+Unfiltered,-Filtered)

right_range_filter(max,Terms,Terms).

right_range_filter(_,[],[]).

right_range_filter(Right,[Gene|Us],Fs) :-
        integer(Right),
	Gene =.. [ _functor, _Genome_Key, _Left, GeneRight, _strand, _frame, _extra ],
        Right < GeneRight, 
        !,
        right_range_filter(Right,Us,Fs).
        
right_range_filter(Right,[Gene|Us],[Gene|Fs]) :-
        !,
        right_range_filter(Right,Us,Fs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_by_strands(+GoodStrands,+GeneTermsIn,-GeneTermsOut)

filter_by_strands(GoodStrands,GeneTerms,GeneTerms) :-
	member('+',GoodStrands),
	member('-',GoodStrands),
	write('No filtering performed'), nl, !.

filter_by_strands(_,[],[]).

filter_by_strands(GoodStrands,[GeneTerm|TermsRest],[GeneTerm|MatchedRest]) :-
	GeneTerm =.. [ _functor,_Genome_Key ,_start, _end, Strand, _frame, _extra ],
	member(Strand,GoodStrands),
	!,
	filter_by_strands(GoodStrands,TermsRest,MatchedRest).

filter_by_strands(GoodStrands,[_|TermsRest],MatchedRest) :-
	filter_by_strands(GoodStrands,TermsRest,MatchedRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_by_frames(+GoodFrames,+GeneTermsIn,-GeneTermsOut)

% Skip filtering if we allow all frames
filter_by_frames(GoodFrames,GeneTerms,GeneTerms) :-
	member(1,GoodFrames),
	member(2,GoodFrames),
	member(3,GoodFrames),
	write('No filtering performed'), nl, !.

filter_by_frames(_,[],[]).

filter_by_frames(GoodFrames,[GeneTerm|TermsRest],[GeneTerm|MatchedRest]) :-
	GeneTerm =.. [ _functor, _Genome_Key, _start, _end, _strand, Frame, _extra ],
	member(Frame,GoodFrames),
	!,
	filter_by_frames(GoodFrames,TermsRest,MatchedRest).

filter_by_frames(GoodFrames,[_|TermsRest],MatchedRest) :-
	filter_by_frames(GoodFrames,TermsRest,MatchedRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_by_length(+MinLength,+MaxLength,+GeneTermsIn,-GeneTermsOut)

filter_by_length([default,default],GeneTerms,GeneTerms) :- 
        write('filter_by_length: No filtering performed'),nl,!.

filter_by_length(_Range,[],[]).

filter_by_length([MinLength,MaxLength],[G|GRest],[G|MRest]) :-
	G =.. [ _functor,_Genome_Key, Start, End, _strand, _frame, _extra ],
        ((End > Start) ->
                Length is (End - Start) + 1 
                ;
                Length is (Start - End) + 1
        ),
        Length >= MinLength,
        Length =< MaxLength,
        !,
        filter_by_length([MinLength,MaxLength],GRest,MRest).

filter_by_length([MinLength,MaxLength],[_|GRest],MRest) :-
        filter_by_length([MinLength,MaxLength],GRest,MRest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% regex_filter_non_matching(+RegexList, +GeneTermList, -MatchedGeneTermList).
% Only terms from GeneTermList matching all the regular expressions in RegexList
% are kept in MatchedGeneTermList
regex_filter_non_matching(_,[],[]).
regex_filter_non_matching(RegexList,[GeneTerm|GeneTermsRest],[GeneTerm|MatchedGeneRest]) :-
	forall(member(Regex,RegexList), regex_match_geneterm(Regex,GeneTerm)),
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
	write('removing match:'), write(GeneTerm),nl,
	!,
	regex_filter_matching(RegexList,GeneTermsRest,UnmatchedGeneRest).

regex_filter_matching(RegexList,[GeneTerm|GeneTermsRest],[GeneTerm|UnmatchedGeneRest]) :-
	regex_filter_matching(RegexList,GeneTermsRest,UnmatchedGeneRest).

% regex_match_geneterm(+Regex, +GeneTerm)
% True if the regular expression Regex matches GeneTerm
regex_match_geneterm(RegexCondition,GeneTerm) :-
	RegexCondition =.. [ Functor, RegexAtom ],
	GeneTerm =.. [ _functor, _Genome_Key, _start, _end, _strand, _frame, ExtraList ],
	FunctorMatcher =.. [ Functor, Data ],
	member(FunctorMatcher,ExtraList),
	re_compile(RegexAtom,Regex),!,
	re_match(Regex,Data,_matches).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

exact_filter_matching(_,[],[]).

exact_filter_matching(MatcherList,[GeneTerm|GeneTermsRest],UnmatchedGeneRest) :-
	%write(MatcherList),nl,
	%write(GeneTerm),nl,
	GeneTerm =.. [ _func, _id, _left, _right, _strand, _frame, Extra ],
	%write(Extra),nl,	
	member(Matcher,MatcherList),
	member(Matcher,Extra),
	write('removing match:'), write(GeneTerm),nl,
	!,
	exact_filter_matching(MatcherList,GeneTermsRest,UnmatchedGeneRest).
	
exact_filter_matching(MatcherList,[GeneTerm|GeneTermsRest],[GeneTerm|UnmatchedGeneRest]) :-
	exact_filter_matching(MatcherList,GeneTermsRest,UnmatchedGeneRest).

exact_filter_non_matching(_,[],[]).

exact_filter_non_matching(MatcherList,[GeneTerm|GeneTermsRest],UnmatchedGeneRest) :-
	%write(MatcherList),nl,
	%write(GeneTerm),nl,	
	GeneTerm =.. [ _func, _id, _left, _right, _strand, _frame, Extra ],
	%write(Extra),nl,
	write(MatcherList),nl,
	write(Extra),nl,
	member(Matcher,MatcherList),
	not(member(Matcher,Extra)),
	write('removing match:'), write(GeneTerm),nl,
	!,
	exact_filter_non_matching(MatcherList,GeneTermsRest,UnmatchedGeneRest).

exact_filter_non_matching(MatcherList,[GeneTerm|GeneTermsRest],[GeneTerm|UnmatchedGeneRest]) :-
	exact_filter_non_matching(MatcherList,GeneTermsRest,UnmatchedGeneRest).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_non_protein_coding(+ProteinRegexMatch,+GenomeDataTerms,+GeneTerms,-MatchedGeneTerms)
filter_non_protein_coding(_,_,[],[]).

filter_non_protein_coding(GeneCode,GenomeSeqId,[GeneTerm|GenesRest],[GeneTerm|MatchedRest]) :-
	%get_gene_nucleotides(GeneTerm,GenomeSeqId,Sequence),
	get_gene_start_and_stop_sequence(GeneTerm,GenomeSeqId,Sequence),
	GeneTerm =.. [ _functor, _Genome_Key, _start, _end, Strand, _frame, _extraList ],
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
	%write(SequenceForward),nl,

	% Extract start codon
	SequenceForward = [ Start1,Start2,Start3 | _ ],
	SequenceBackward = [ Stop3, Stop2, Stop1 | _ ],
	StartCodon = [Start1,Start2,Start3],
	StopCodon = [Stop1,Stop2,Stop3],

	write('Not filtering; '), write(GeneTerm), nl,
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
	GeneTerm =.. [ _functor, _Genome_Key, Min, Max, _Strand, _frame, _extra ],
	get_sequence_range(GenomeSequenceId,Min,Max,NucleotideData).


get_gene_start_and_stop_sequence(GeneTerm,GenomeSequenceId,NucleotideData) :-
	GeneTerm =.. [ _functor, _Genome_Key, Min, Max, _strand, _frame, _extra ],
	FirstMax is Min + 2,
	LastMin is Max - 2,
	get_sequence_range(GenomeSequenceId,Min,FirstMax,StartCodon),
	get_sequence_range(GenomeSequenceId,LastMin,Max,EndCodon),
	append(StartCodon,EndCodon,NucleotideData).


% Extract the ranges for each gene in list
gene_ranges([],[]).
gene_ranges([GeneTerm|GenesRest], [Range|RangesRest]) :-
	GeneTerm =.. [ _functor, _Genome_Key, Start, End, _strand, _frame, _extraList ],
	Range = [ Start, End ],
	gene_ranges(GenesRest,RangesRest).
