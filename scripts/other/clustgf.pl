:- ['../lost.pl'].
:- lost_include_api(interface).

% Test script for cluster gene finder

number_of_clusters(5).

gene_range_left(1).
gene_range_right(100000).

input_data_files(GenomeFile,GenesFile) :-
        lost_sequence_file('U00096_fna',FNA),
        lost_sequence_file('U00096_gbk',GBK),
        lost_sequence_file('U00096_ptt',PTT),
        gene_range_left(Left),
        gene_range_right(Right),
        run_model(parser_fna,
                  annotate([FNA,GBK],[list(1000),range([Left,Right])],GenomeFile)),
        run_model(parser_ptt,
                  annotate([PTT],[],GenesFileOrig)),
        run_model(gene_filter,
                  annotate([GenesFileOrig,GenomeFile],[left(Left),right(Right)],GenesFile)).

cluster(ClustersFile) :-
        input_data_files(GenomeFile,GenesFile),
        number_of_clusters(NC),
        run_model(cluster_gene_finder,
                  cluster_genes([GenomeFile,GenesFile],[number_of_clusters(NC)],ClustersFile)).

train(ParamsFile) :-
        input_data_files(GenomeFile,GenesFile),
        chop_all_framedir(GenomeFile,ORFFile),
        number_of_clusters(NC),
        run_model(cluster_gene_finder,
                  train([GenomeFile,GenesFile,ORFFile],[number_of_clusters(NC)],ParamsFile)).

predict(PredictionsFile) :-
        input_data_files(GenomeFile,_),
        chop_all_framedir(GenomeFile,ORF_File),
        train(ParamsFile),
        number_of_clusters(NC),
        run_model(cluster_gene_finder,
                  annotate([ORF_File,ParamsFile],[prediction_mode(viterbi),number_of_clusters(NC)],PredictionsFile)).

predict_and_report(ReportFile) :-
        input_data_files(_,GenesFile), 
        predict(PredictionsFile),
        gene_range_left(Start),
        gene_range_right(End),
        run_model(accuracy_report,
                  annotate([GenesFile,PredictionsFile],[start(Start),end(End)],ReportFile)).
                

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For running the orf_chopper in all frames:
frame(1).
frame(2).
frame(3).

direction('-').
direction('+').

chop_all_framedir(SequenceFile,OutFile) :-
        findall([F,D],(frame(F),direction(D)),FrameDirList),
        chop_framedir_list(SequenceFile,FrameDirList,OutFiles),
        run_model(merge_files, annotate(OutFiles,[],OutFile)).

chop_framedir_list(_,[],[]).

chop_framedir_list(SequenceFile,[[Frame,Direction]|Rest],[OutFile1|FilesRest]) :-
	run_model('orf_chopper', annotate([SequenceFile], [frame(Frame),direction(Direction)],OutFile1)),
        chop_framedir_list(SequenceFile,Rest,FilesRest).
        
