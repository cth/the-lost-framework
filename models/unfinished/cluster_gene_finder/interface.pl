:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_api(viterbi_learn).

:- [stats_with_model].
:- [create_training_data].

% Get rid of warnings about non-contigous predicates
:- multifile(lost_option/4).
:- multifile(lost_input_formats/2).
:- multifile(lost_output_format/3).

% FIXME: This should be configurable globally within the lost framework 
lost_concurrent_processes(10).

% Interface goal: cluster_genes
lost_option(cluster_genes,number_of_clusters,2,'The number of gene clusters to use in the gene finder.').  
lost_input_formats(cluster_genes,[text(prolog(sequence(_))),text(prolog(ranges(_)))]).
lost_output_format(cluster_genes,_,text(prolog(clusters(_)))).

% Interface goal: train
lost_option(train,number_of_clusters,2,'The number of gene clusters to use in the gene finder.').
lost_input_formats(train,[text(prolog(sequence(_))),text(prolog(ranges(_)))]).
lost_output_format(train,_,text(prolog(switches(_)))).

% Interface goal: annotate
lost_option(annotate,number_of_clusters,2,'The number of gene clusters to use in the gene finder.').
lost_option(annotate,prediction_mode,viterbi,'Which method to use for prediction.')-
lost_input_formats(annotate,[text(prolog(ranges(_))),text(prolog(switches(_)))]).
lost_output_format(annotate,_,text(prolog(ranges(_)))).


%% annotate(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [ OrfsFile, ParametersFile ]
% == 
% OrfsFile is a file with open reading frames to be classified as genes or non-genes by the model.
% =|ParametersFile|= is a parameterization of the model.
% The task takes two options, 
% =|prediction_mode|= which is the method to use for prediction. Possible values are =|viterbi|= which uses viterbi decoding 
% and =|posterior|= which uses posterior decoding.
annotate([ORF_File,ParametersFile],Options,OutputFile) :-
        nl, write('cluster_gene_finder annotate :'),nl,
        get_option(Options,prediction_mode,viterbi),
        get_option(Options,number_of_clusters,NumClusters),
        prismAnnot(clustgf),
        assert(coding_clusters(NumClusters)),
        restore_sw(ParametersFile),
        open(ORF_File,read,InStream),
        open(OutputFile,write,OutStream),
        read_and_predict_orf(InStream,OutStream),
        close(InStream),
        close(OutStream).

read_and_predict_orf(InStream,OutStream) :-
        read(InStream,ORF),
        ((ORF == end_of_file) ->
                true
                ;
                ORF =.. [_Functor,_SeqId,Left,Right,_Dir,_Frame,Extra],
                member(sequence(Sequence), Extra),
                check_or_fail(viterbiAnnot(cluster_hmm(Sequence,Annotation),_P),
                      (write(ORF),
                      error('Viterbi computation failed'))
                ),
                (is_noncoding_prediction(Annotation) ->
                        write(orf(Left,Right)), write(' predicted as non-coding. Not reporting.'),nl
                        ;
                        build_annotation_fact(ORF,Annotation,AnnotationFact),
                        write('reporting predicted gene in '),
                        write(orf(Left,Right)),nl,
                        write(AnnotationFact),nl,
                        writeq(OutStream,AnnotationFact),
                        write(OutStream,'.\n')
                ),
                !,
                read_and_predict_orf(InStream,OutStream)
        ).

is_noncoding_prediction(Annotation) :-
        delete(Annotation,0,[]).

build_annotation_fact(ORF,Annotation,clustgf_annotation(SeqId,AdjLeft,AdjRight,Strand,Frame,[])) :-
        ORF =.. [_Functor,SeqId,Left,Right,Strand,Frame,_Extra],
        first_coding_position(Annotation,Strand,Left,Right,AdjLeft,AdjRight).

first_coding_position([C|_],_, Left, Right, Left, Right) :-
        C \= 0.

first_coding_position([0|AnnotRest],'+', Left, Right, AdjLeft, AdjRight) :-
        NewLeft is Left + 1,
        first_coding_position(AnnotRest,'+', NewLeft, Right, AdjLeft, AdjRight).

first_coding_position([0|AnnotRest],'-', Left, Right, AdjLeft, AdjRight) :-
        NewRight is Right - 1,
        first_coding_position(AnnotRest,'+', Left, NewRight, AdjLeft, AdjRight).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cluster_genes interface goal:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% cluster_genes(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ GenomeFile, GenesFile ]
% ==
% Creates as clustering of the genes in =|GenesFile|=.
% The task takes an option =|number_of_clusters|= which is the number of clusters.
cluster_genes([GenomeFile,GenesFile],Options,OutputFile) :-
        write('cluster_genes called'),nl,
        get_option(Options,number_of_clusters,NumClusters),
        lost_tmp_directory(TmpDir),
        atom_concat(TmpDir,'clustgf_stats.csv',FileStatsCSV),
        atom_concat(TmpDir,'clustgf_stats.lbl',FileStatLabels),
        write('determined stats file names: '),nl,
        write(FileStatsCSV),nl,
        write(FileStatLabels),nl,
        % First, extract gene statistics 
        create_stats_with_model(GenesFile,GenomeFile,FileStatsCSV,FileStatLabels),
        % Do the clustering of the genes
        atom_integer(NumClustersAtom,NumClusters),
        intersperse(' ', ['./cluster_genes.rb',FileStatsCSV,FileStatLabels,NumClustersAtom,OutputFile],ClusterCommandList),
        atom_concat_list(ClusterCommandList,ClusterCommand),
        write('Running external command: '),nl,
        writeq(ClusterCommand),nl,
        system(ClusterCommand).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_training_data inferface goal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% create_training_data(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ ClustersFile, GenesFile, OrfsFile ]
% ==
% Create training data from a clusters file and file with known genes, =|GenesFile|= and a file with open reading frames, 
% =|OrfsFile|=.
% The result 
create_training_data([ClustersFile,GenesFile,ORFFile], _Options,TrainingDataFile) :-
        write(create_training_data(ClustersFile,GenesFile,ORFFile,TrainingDataFile)),nl,
        create_training_data(ClustersFile,GenesFile,ORFFile,TrainingDataFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% train interface goal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Should be rewritten (not to use parallelism)
train([GenomeFile,GenesFile,ORFFile],Options,OutputFile) :-
        write('train'),nl,
        lost_tmp_directory(TmpDir),
        get_option(Options,number_of_clusters,NumClusters),
        % Call the model it self to cluster the genes first 
        run_model(      cluster_gene_finder,
                        cluster_genes(  [GenomeFile,GenesFile],
                                        [number_of_clusters(NumClusters)],
                                        ClustersFile)),
        %atom_concat(TmpDir, 'clustgf_training_data.pl',TrainingDataFile),
        run_model(cluster_gene_finder,
                  create_training_data([ClustersFile,GenesFile,ORFFile],[],TrainingDataFile)),
        % Split the training data in multiple files
        atom_concat(TmpDir, 'clustgf_split_training_data',TrainingDataFilePrefix),
        split_file(TrainingDataFile,1000,TrainingDataFilePrefix,'.pl',ChunkFiles),
        write('split file into chunks:'),nl,
        write(ChunkFiles),nl,
        % Now learn parameters for model using viterbi_learn
        lost_concurrent_processes(ProcessCountInt),
        atom_integer(ProcessCountAtom,ProcessCountInt),
        atom_integer(NumClustersAtom,NumClusters),
        intersperse(' ', ['./parallel_train.sh',NumClustersAtom,ProcessCountAtom,OutputFile|ChunkFiles],TrainCommandList),
        atom_concat_list(TrainCommandList,TrainCommand),
        write('Running external command: '),nl,
        writeq(TrainCommand),nl,
        system(TrainCommand).
