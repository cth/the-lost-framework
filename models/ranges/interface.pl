:- task(as_fasta([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(fasta))).
:- task(take([text(prolog(ranges(gene)))], [count(10)], [text(prolog(ranges(gene)))])).
:- task(translate([text(prolog(ranges(gene)))], [sequence_functor(sequence),genecode(11)], text(prolog(ranges(gene))))).


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





