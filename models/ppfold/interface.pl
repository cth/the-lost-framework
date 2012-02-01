:- [ppfold].
:- use(genedb).
:- use(lists).

:- task(ppfold([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(prolog(ranges(gene))))).

%% ppfold(+InputFiles,+Options,+OutputFile)
% ==
% InputFile = [InputFile]
% == 
% Folds a sequence annotation given as an extra of each gene and adds this as a new extra field
% of the gene in the output file. The extra containing the sequence to fold is specified using the
% =|sequence_functor|= option.
ppfold(InputFile, Options, OutputFile) :-
	get_option(Options,sequence_functor,Functor),
	terms_from_file(InputFile,GeneTerms),
	map(ppfold_for_gene(in,Functor,out), GeneTerms,FoldedGeneTerms),
	terms_to_file(OutputFile,FoldedGeneTerms).
	
ppfold_for_gene(Gene,SequenceFunctor,UpdatedGene) :-
	write_fasta_file(Gene,SequenceFunctor,FastaFile),
	ppfold_run(FastaFile, Folding, Energy),
	gene_add_extra_field(Gene,folding,Folding,Gene1),
	gene_add_extra_field(Gene1,energy,Energy,UpdatedGene).
