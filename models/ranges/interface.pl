:- task(as_fasta([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(fasta))).
:- task(take([text(prolog(ranges(gene)))], [count(10)], [text(prolog(ranges(gene)))])).
:- task(translate([text(prolog(ranges(gene)))], [sequence_functor(sequence),genecode(11)], text(prolog(ranges(gene))))).

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

%% take(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% OutputFile contains the the N first elements of InputFile, where N is determined by option count
take([InputFile],Options,OutputFile) :-
	get_option(Options,count,Count),
	terms_from_file(InputFile,Terms),
	take(Count,Terms,Taken),
	terms_to_file(OutputFile,Taken).

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




