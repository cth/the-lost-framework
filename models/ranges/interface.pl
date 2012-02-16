
:- task(as_fasta([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(fasta))).

:- use(fasta).
:- use(genedb).
:- use(lists).

as_fasta([InputFile],Options,OutputFile) :-
	terms_from_file(InputFile,GeneTerms),
	get_option(Options,sequence_functor,SeqFunc),
	map(create_fasta_entry(input,SeqFunc,output),GeneTerms,FastaElements),
	flatten(FastaElements,FastaCodes),
	open(OutputFile,write,OutStream),
	forall(member(Code,FastaCodes),put_code(OutStream,Code)),
	close(OutStream).

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

