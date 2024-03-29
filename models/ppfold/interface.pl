:- [ppfold].
:- use(genedb).
:- use(lists).

:- task(fold([text(prolog(ranges(gene)))], [sequence_functor(sequence)], text(prolog(ranges(gene))))).

:- task(sort_folded_by_energy([text(prolog(ranges(gene)))], [], text(prolog(ranges(gene))))).

:- task(filter_by_folding_constraints([text(prolog(ranges(_)))],[min_stem_length(0),min_base_pairs(0),min_energy(0)],text(prolog(ranges(_))))).
%:- task(filter_by_folding_constraints([text(prolog(ranges(_)))],[min_stem_length(4),min_base_pairs(20),min_energy(0.1)],text(prolog(ranges(_))))).

%% ppfold(+InputFiles,+Options,+OutputFile)
% ==
% InputFile = [InputFile]
% == 
% Folds a sequence annotation given as an extra of each gene and adds this as a new extra field
% of the gene in the output file. The extra containing the sequence to fold is specified using the
% =|sequence_functor|= option.
fold([InputFile], Options, OutputFile) :-
	get_option(Options,sequence_functor,Functor),
	terms_from_file(InputFile,GeneTerms),
	map(ppfold_for_gene(input,Functor,output), GeneTerms,FoldedGeneTerms),
	terms_to_file(OutputFile,FoldedGeneTerms).
% where,
ppfold_for_gene(Gene,SequenceFunctor,UpdatedGene) :-
	write_fasta_file(Gene,SequenceFunctor,FastaFile),
	ppfold_run(FastaFile, Folding, Energy),
	gene_add_extra_field(Gene,folding,Folding,Gene1),
	gene_add_extra_field(Gene1,energy,Energy,UpdatedGene).

%% sort_folded_by_energy(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% Given an InputFile with which has allready been folded, this task sorts the 
% terms in the file by folding energy.
sort_folded_by_energy([InputFile],Options,OutputFile) :-
	terms_from_file(InputFile,GeneTerms),
	map(by_energy(input,output),GeneTerms,GeneTermsByEnergy),
	sort(GeneTermsByEnergy,SortedGeneTermsByEnergy),
	map(by_energy(output,input),SortedGeneTermsByEnergy,SortedGeneTerms),
	terms_to_file(OutputFile,SortedGeneTerms).
% where, 
by_energy(GeneTerm,(Energy,GeneTerm)) :-
	gene_extra_field(GeneTerm,energy,Energy).
	
%% filter_by_folding_constraints(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% Removes candidates from input file which are invalid by given constraints
filter_by_folding_constraints([InputFile],Options,OutputFile) :-
	forall(member(Opt,Options),assert(Opt)),
	cl(folding_constraints),
	open(InputFile,read,InStream),
	open(OutputFile,write,OutStream),
	read_and_filter_terms(InStream,OutStream,0),
	close(InStream),
	close(OutStream).
