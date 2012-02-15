:- ['../../lost.pl'].

:- use(interface).

% Load the matcher code:
:- cl(matcher).

:- use(dnaseq). % for complement
:- use(genedb).

:- task(match([text(fasta)], [sequences([])], text(prolog(ranges(_))))).
:- task(extract([text(fasta)], [left(0),right(0),reverse_complement(false),sequence_identifier(na)], text(prolog(ranges(_))))).

%% match(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [FastaFile]
% ==
% Searches for particular sequences in Fasta file on both strands
% Assumes that the genome is circular.
match([FastaFile],Options,OutputFile) :-
	get_option(Options,sequences,Sequences),
	writeln('Option sequences: '), writeln(Sequences),
	match_sequences(Sequences,FastaFile,OutputFile).

%% extract(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [FastaFile]
% ==
% Extracts a region of of =|FastaFile|= identified by positions given with options =|left|= and =|right|=.
% If =|left|= is greater than =|right|=, then the sequence in =|FastaFile|= is treated as being circular.
% Additionally, the extracted sequence may be reverse complemented by setting the option =|reverse_complement|= to true.
% The extracted sequence is stored as a gene range fact, and the sequence identifier of fact is specified using
% the option =|sequence_identifier|=.
extract([FastaFile],Options,OutputFile) :-
	get_option(Options,left,Left),
	get_option(Options,right,Right),
	cl(extracter),
	extract_from_sequence(FastaFile,Left,Right,Extracted),
	get_option(Options,reverse_complement(RC)),
	(get_option(Options,reverse_complement(true)) ->
		reverse(Extracted,ReverseExtracted),
		complement(ReverseExtracted,ExtractedFinal),
		Strand = '-'
		;
		ExtractedFinal = Extracted,
		Strand = '+'
	),
	get_option(Options,sequence_identifier,SeqId),
	gene_create(SeqId,Left,Right,Strand,GeneRecord),
	gene_add_extra_field(GeneRecord,sequence,ExtractedFinal,UpdGeneRecord),
	terms_to_file(OutputFile,[UpdGeneRecord]).