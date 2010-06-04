:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_api(erf_learn).

:- [stats_with_model].

% Option declaration
lost_option(annotate,type_gene,hard,'Specified with which kind of data the genefinder is set').
lost_option(annotate,use_parameter_file,yes,'Load parameters from the parameter file').
lost_option(annotate,optimized,false,'whether to split prediction in multiple processes').

% Input Format Specification
lost_input_formats(annotate,[prolog(sequence(_))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).

train([GenomeFile,GenesFile],Options,OutputFile) :-
        get_option(Options,number_of_clusters,NumClusters),
        % First, extract gene statistics 
        create_stats_with_model, 
        % Do the clustering of the genes
        atom_integer(NumClustersAtom,NumClusters),
        atom_concat('./cluster_genes.rb stats.csv stats.lbl ', NumClustersAtom, ClusterCommand), 
        system(ClusterCommand),
        ...learn...

