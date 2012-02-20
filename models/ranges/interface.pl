
:- task(as_fasta([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(fasta))).

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

