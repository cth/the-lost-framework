% Produce a list of hard to find genes in 
% the ecoli genome

:- ['../lost.pl'].
:- lost_include_api(interface).

hard_to_find_test :-
	lost_sequence_file(gb_fragment,GBFile), % Genbank (reference genes)
	lost_sequence_file(eg_fragment,EGFile), % Easygene predictions
	lost_sequence_file(gm_fragment,GMFile), % Genemark predictions
	
	get_annotation_file(hard_to_find_genes,
			    [GBFile,EGFile,GMFile],
			    [option(file_functor(GBFile),gb),
			     option(file_functor(EGFile),eg),
			     option(file_functor(GMFile),gm)],
			    OutputFile),
	write('File written to: '), write(OutputFile), nl.

  
  
