:- task(as_fasta([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(fasta))).
:- task(range_take([text(prolog(ranges(gene)))], [count(10)], text(prolog(ranges(gene))))).
:- task(translate([text(prolog(ranges(gene)))], [sequence_functor(sequence),genecode(11)], text(prolog(ranges(gene))))).
:- task(add_extra_field([text(prolog(ranges(gene)))],[extra_field(extra(na))],text(prolog(ranges(gene))))).
:- task(sort_by_field([text(prolog(ranges(gene)))],[sort_field(na)],text(prolog(ranges(gene))))).
:- task(add_sequence_field([text(prolog(ranges(gene))),text(fasta)],text(prolog(ranges(gene))))).

:- task(filter(
		[text(prolog(ranges(gene))),text(prolog(sequence))],
				[
					match_frames([1,2,3]), % A list of valid frames. Genes in other frames will be filtered
					match_strands(['-','+']), % A list of valid strands. Genes occuring an other strand will be filtered
					exact_match_extra_fields([]), % A list of Prolog terms terms each of which must unify with an element of the Extralist
					exact_no_match_extra_fields([]), % A list of Prolog terms terms any of which must NOT unify with any element of the Extralist
					regex_match_extra_fields([]), % A list of regular field names in the extra and regular expressions to match those fields. If a gene is matched by one of these regular expressions, then it is a candidate for the output
					regex_no_match_extra_fields([]), % A list of field names in the extra and regular expressions to match those fields. If a gene is matched by any of these regular expression, it will be filtered and will not occur in the output.
					match_protein_coding(no), % Specifies that the gene should start with a valid start codon and end with a valid stop codon
					genecode(11), % The gene code to used. This is used to determine which genes are protein coding
					invert_results(no), % When this option is set to \'yes\', then the filtered genes will appear in the output instead of non-filtered genes
					left(min), % An integer value denoting the left-most nucleotide to include in results. The default value (min) refers to the left-most nucleotide in the input data
					right(max), % An integer value denoting the right-most nucleotide to include in results. The default value (min) refers to the right-most nucleotide in the input data
					range([default,default]) % A range [RangeMin,RangeMax] used to filter genes with a length in this range
				],
				text(prolog(ranges(gene))))).
				
:- task(filter(
		[text(prolog(ranges(gene)))],
			[
				match_frames([1,2,3]), % A list of valid frames. Genes in other frames will be filtered
				match_strands(['-','+']), % A list of valid strands. Genes occuring an other strand will be filtered
				exact_match_extra_fields([]), % A list of Prolog terms terms each of which must unify with an element of the Extralist
				exact_no_match_extra_fields([]), % A list of Prolog terms terms any of which must NOT unify with any element of the Extralist
				regex_match_extra_fields([]), % A list of regular field names in the extra and regular expressions to match those fields. If a gene is matched by one of these regular expressions, then it is a candidate for the output
				regex_no_match_extra_fields([]), % A list of field names in the extra and regular expressions to match those fields. If a gene is matched by any of these regular expression, it will be filtered and will not occur in the output.
				match_protein_coding(no), % Specifies that the gene should start with a valid start codon and end with a valid stop codon
				genecode(11), % The gene code to used. This is used to determine which genes are protein coding
				invert_results(no), % When this option is set to \'yes\', then the filtered genes will appear in the output instead of non-filtered genes
				left(min), % An integer value denoting the left-most nucleotide to include in results. The default value (min) refers to the left-most nucleotide in the input data
				right(max), % An integer value denoting the right-most nucleotide to include in results. The default value (min) refers to the right-most nucleotide in the input data
				range([default,default]) % A range [RangeMin,RangeMax] used to filter genes with a length in this range
			],
			text(prolog(ranges(gene))))).
			
:- task(add_sequence_field([text(prolog(ranges(gene))),text(fasta)],[],text(prolog(ranges(gene))))).

:- use(fasta).
:- use(genedb).
:- use(lists).

% FIXME: Makes this read and write terms one at a time...
as_fasta([InputFile],Options,OutputFile) :-
	get_option(Options,sequence_functor,SeqFunc),
	open(InputFile,read,InStream),
	open(OutputFile,write,OutStream),
	read_term_and_write_fasta_entry(SeqFunc,InStream,OutStream),!,
	close(InStream),
	close(OutStream).
	
read_term_and_write_fasta_entry(SeqFunc,InStream,OutStream) :-
	read(InStream,Term),
	((Term == end_of_file) ->
		true
		;
		write('.'),
		create_fasta_entry(Term,SeqFunc,FastaEntry),
		forall(member(Code,FastaEntry),put_code(OutStream,Code)),
		read_term_and_write_fasta_entry(SeqFunc,InStream,OutStream)
	).

create_fasta_entry(GeneTerm,SeqFunc,FastaEntry) :-
	gene_sequence_id(GeneTerm,SeqId),
	gene_left(GeneTerm,Left),
	gene_right(GeneTerm,Right),
	gene_strand(GeneTerm,Strand),
	gene_frame(GeneTerm,Frame),
	gene_extra_field(GeneTerm,SeqFunc,Sequence),
	intersperse('_',[SeqId,Left,Right,Strand,Frame],HeaderElemList),
	atom_concat_list(HeaderElemList,HeaderAtom),
	to_fasta(HeaderAtom,Sequence,FastaEntry).

%% range_take(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% OutputFile contains the the N first elements of InputFile, where N is determined by option count
range_take([InputFile],Options,OutputFile) :-
	get_option(Options,count,Count),
	terms_from_file(InputFile,Terms),
        writeln('loaded terms'),
	take(Count,Terms,Taken),
        writeln('writing temrs to file'),
	terms_to_file(OutputFile,Taken),
        writeln(done).



%% translate(+InputFile,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% Translates nucleotide sequence indicated by =|sequence_functor|= to a protein.
% The protein sequence is added as an extra field with the functor =|protein_sequence|=.
translate([InputFile],Options,OutputFile) :-
	write('!!!'),
	get_option(Options,genecode,GeneCode),
	get_option(Options,sequence_functor,SequenceFunctor),
	open(InputFile,read,InStream),
	open(OutputFile,write,OutStream),
	cl(translate),
	translate_terms(InStream,OutStream,SequenceFunctor,GeneCode),
	close(InStream),
	close(OutStream).

%% add_extra_field(+InputFile,+Option,+OutputFile)
add_extra_field([InputFile],Options,OutputFile) :-
	get_option(Options,extra_field,ExtraField),
	open(InputFile,read,InStream),
	open(OutputFile,write,OutStream),
	add_extra_field_rec(InStream,OutStream,ExtraField),
	close(InStream),
	close(OutStream).

%% filter(+InputFiles,+Optiosn,+OutputFiles)
% ==
% InputFiles = [ GenesFile ]
% ==
% Filters away some genes given a set of genes.
filter([GeneListFile], Options, OutFile) :-
	filter([GeneListFile,_], Options, OutFile).

%% filter(+InputFiles,+Optiosn,+OutputFiles)
% ==
% InputFiles = [ GenesFile, SequenceFile ]
% ==
% Filters away some genes given a set of genes.
filter([GeneListFile,GeneDataFile], Options, OutFile) :-
	cl(filter),
	filter_impl([GeneListFile,GeneDataFile], Options, OutFile).

add_extra_field_rec(InStream,OutStream,ExtraField) :-
	read(InStream,Term),
	write('.'),
	((Term == end_of_file) ->
		true
		;
		ExtraField =.. [ Key, Value ],
		gene_add_extra_field(Term,Key,Value,NewTerm),
		writeq(OutStream,NewTerm),
		write(OutStream,'.\n'),
		!,
		add_extra_field_rec(InStream,OutStream,ExtraField)).
		
%% sort_by_field(+InputFile,+Option,+OutputFile)
sort_by_field([InputFile],Options,OutputFile) :-
	get_option(Options,sort_field,SortFieldFunctor),
	terms_from_file(InputFile,Terms),
	writeln('wrapping..'),
	wrap_by_field(SortFieldFunctor,Terms,Wrapped),
	writeln('sorting...'),
	sort(Wrapped,WrappedSorted),
	writeln('unwrapping..'),
	unwrap(WrappedSorted,SortedTerms),
	writeln('writing terms..'),
	terms_to_file(OutputFile,SortedTerms).
	
wrap_by_field(_Field,[],[]).

wrap_by_field(Field,[T|Ts],[[Value,T]|Ws]) :-
	gene_extra_field(T,Field,Value),
	wrap_by_field(Field,Ts,Ws).
	
unwrap([],[]).
unwrap([[_,T]|Ws],[T|Ts]) :-
		unwrap(Ws,Ts).
		
%% add_sequence_field(+InputFiles,+Options,+OutputFiles)
% ==
% InputFiles = [ RangesFile, FastaFile ]
% ==
add_sequence_field([RangesFile,FastaFile],_Options,OutputFile) :-
	cl(genome),
	load_genome(FastaFile,GenomeTable),
	terms_from_file(RangesFile,Ranges),
	add_sequence_field_rec(Ranges,GenomeTable,UpdatedRanges),
	terms_to_file(OutputFile,UpdatedRanges).

add_sequence_field_rec([],_,[]).
add_sequence_field_rec([Gene|GenesRest],Genome,[UpdatedGene|UpdatedRest]) :-
	write('.'),
	gene_left(Gene,Left),
	gene_right(Gene,Right),
	gene_strand(Gene,Strand),
	get_range(Left,Right,Genome,Sequence1),
	((Strand == '+') ->
		Sequence = Sequence1
		;
		reverse(Sequence1,RevSeq),
		complement(RevSeq,Sequence)
	),
	gene_add_extra_field(Gene,sequence,Sequence,UpdatedGene),
	add_sequence_field_rec(GenesRest,Genome,UpdatedRest).

	


