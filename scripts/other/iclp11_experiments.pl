:- ['../lost.pl'].

sequence_id('NC_000913').

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

% Genome is expected to be in fasta format
chop_genome(GenomeFile) :-
	chop_all_framedir(GenomeFile,OutFile),
	write('all orfs in '), write(GenomeFile), write(' written to '), write(OutFile),nl.


test :-
        sequence_id(Id),
	lost_sequence_file(Id,File),
	write(File),nl.
	
% Produce file containing all ORFs of the e-coli genome
run :-
        % Parse the FNA file 
        sequence_id(ID),
        lost_sequence_file(ID,FNAFile),
        run_model(parser_fna,parse([FNAFile],[list(280)],ParsedFNA)), 
        chop_genome(ParsedFNA).

translate_chunks :-
        lost_data_directory(DataDir),
        ChunksFile = 'nc000913_all_orfs.pl',
        atom_concat(DataDir,ChunksFile,FullFile),
        run_model(chunk_translator,annotate([FullFile],[],TranslatedChunksFile)),
        write('wrote translated chunks file:'),
        write(TranslatedChunksFile),nl.

translate_mini :-
        lost_data_directory(DataDir),
        TrainingFile = 'mini_train.pl',
        TestFile = 'mini_test.pl',
        atom_concat(DataDir,TrainingFile,FullFile1),
        atom_concat(DataDir,TestFile,FullFile2),
        run_model(chunk_translator,annotate([FullFile1],[],TranslatedChunksFile1)),
        run_model(chunk_translator,annotate([FullFile2],[],TranslatedChunksFile2)).

translate_big :-
        lost_data_directory(DataDir),
        TrainingFile = 'train_orfs.pl',
        TestFile = 'test_orfs.pl',
        atom_concat(DataDir,TrainingFile,FullFile1),
        atom_concat(DataDir,TestFile,FullFile2),
        run_model(chunk_translator,annotate([FullFile1],[],TranslatedChunksFile1)),
        run_model(chunk_translator,annotate([FullFile2],[],TranslatedChunksFile2)).


%%%%%%%%%%%%%
% Merge blast files 
%

small_blast_training_files(Files) :-
	FileNames = [   'blast_results_mini_train_NC_000913.xml.pl',
                        'blast_results_mini_train_NC_004547.xml.pl',
		        'blast_results_mini_train_NC_008800.xml.pl',
                        'blast_results_mini_train_NC_009436.xml.pl',
		        'blast_results_mini_train_NC_009792.xml.pl',
                        'blast_results_mini_train_NC_010067.xml.pl',
		        'blast_results_mini_train_NC_010694.xml.pl',
                        'blast_results_mini_train_NC_011283.xml.pl'],
	lost_data_directory(DD),
	findall(FullFileName,(member(FName,FileNames),atom_concat(DD,FName,FullFileName)),Files).

big_blast_training_files(Files) :-
	FileNames = [   'blast_results_big_train_NC_000913.xml.pl',
                        'blast_results_big_train_NC_004547.xml.pl',
		        'blast_results_big_train_NC_008800.xml.pl',
                        'blast_results_big_train_NC_009436.xml.pl',
		        'blast_results_big_train_NC_009792.xml.pl',
                        'blast_results_big_train_NC_010067.xml.pl',
		        'blast_results_big_train_NC_010694.xml.pl',
                        'blast_results_big_train_NC_011283.xml.pl'],
	lost_data_directory(DD),
	findall(FullFileName,(member(FName,FileNames),atom_concat(DD,FName,FullFileName)),Files).

big_blast_test_files(Files) :-
	FileNames = [   'blast_results_big_test_NC_000913.xml.pl',
                        'blast_results_big_test_NC_004547.xml.pl',
		        'blast_results_big_test_NC_008800.xml.pl',
                        'blast_results_big_test_NC_009436.xml.pl',
		        'blast_results_big_test_NC_009792.xml.pl',
                        'blast_results_big_test_NC_010067.xml.pl',
		        'blast_results_big_test_NC_010694.xml.pl',
                        'blast_results_big_test_NC_011283.xml.pl'],
	lost_data_directory(DD),
	findall(FullFileName,(member(FName,FileNames),atom_concat(DD,FName,FullFileName)),Files).


merge_multiple :-
	small_blast_files(Files1),
	idseqs(Files1,Files2),
	merge_multiple(Files2,_MergedFile).

merge_multiple(InputFiles,OutputFile) :-
	run_model(process_blast_matches,merge_multiple(InputFiles,[],OutputFile)),
	write('outfile is :'), write(OutputFile), nl.

remove_duplicate_hits(InFiles, OutFiles) :-
        findall(OutFile,
                (member(File,InFiles), 
                run_model(process_blast_matches, remove_dups([File],[],OutFile))),
                OutFiles).

add_identity_track(InFiles,OutFiles) :-
        findall(OutFile,
                (member(File,InFiles), 
                run_model(process_blast_matches, identity([File],[],OutFile))),
                OutFiles).

create_blastgf_multitrack_training_file(TrainingDataFile) :-
        %small_blast_training_files(Files),
        big_blast_training_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
        merge_multiple(Files2,MergedFile),
        lost_sequence_file('NC_000913_ptt', PTTFile),
        run_model(parser_ptt, annotate([PTTFile],[genome_key('NC000913')],ParsedPTTFile)),
        run_model(chunk_ref_annot,add_reference_track([MergedFile,ParsedPTTFile],[],TrainingDataFile)),
	write('outfile is :'), write(TrainingDataFile), nl.

train_blastgf_multitrack_model(ParamsFile) :-
        create_blastgf_multitrack_training_file(TrainingData),
        run_model(blastgf,parallel_learn_multi_track([TrainingData],[],ParamsFile)). 

blastgf_multitrack_predict_trainset :-
        ParamsFile = '/tmp/ECML_DATA/blastgf_multitrack_params.pl',
        big_blast_training_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
        merge_multiple(Files2,MergedFile),
        run_model(blastgf,parallel_annotate_multi_track([ParamsFile,MergedFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.

blastgf_multitrack_predict_testset :-
        ParamsFile = '/tmp/ECML_DATA/blastgf_multitrack_params.pl',
        big_blast_test_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
        merge_multiple(Files2,MergedFile),
        run_model(blastgf,parallel_annotate_multi_track([ParamsFile,MergedFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.


%%%%%%
% Blastgf single track model:
%

create_blastgf_singletrack_training_file(TrainingDataFile) :-
        big_blast_training_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
	run_model(process_blast_matches,sum_multiple(Files2,[],MergedFile)),
        lost_sequence_file('NC_000913_ptt', PTTFile),
        run_model(parser_ptt, annotate([PTTFile],[genome_key('NC000913')],ParsedPTTFile)),
        run_model(chunk_ref_annot,add_reference_track([MergedFile,ParsedPTTFile],[],TrainingDataFile)),
	write('single track training file: '), write(TrainingDataFile), nl.

train_blastgf_singletrack_model(ParamsFile) :-
        create_blastgf_singletrack_training_file(TrainingData),
        run_model(blastgf,parallel_learn_single_track([TrainingData],[],ParamsFile)). 

blastgf_singletrack_predict_trainset :-
        ParamsFile = '/tmp/ECML_DATA/blastgf_singletrack_params.pl',
        big_blast_training_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
	run_model(process_blast_matches,sum_multiple(Files2,[],MergedFile)),
        run_model(blastgf,parallel_annotate_single_track([ParamsFile,MergedFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.

blastgf_singletrack_predict_testset :-
        ParamsFile = '/tmp/ECML_DATA/blastgf_singletrack_params.pl',
        big_blast_test_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
	run_model(process_blast_matches,sum_multiple(Files2,[],MergedFile)),
        run_model(blastgf,parallel_annotate_single_track([ParamsFile,MergedFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.

%% Add prediction track(s) to chunks
%
add_pred_track :-
        PredictionFile = '/tmp/ECML_DATA/blastgf_single_track_predict_test_orfs.pl',
        ChunkFile = '/tmp/ECML_DATA/test_orfs.pl',
        run_model(chunk_ref_annot,add_reference_track([ChunkFile,PredictionFile],[],OutFile)),
        write('here: '), write(OutFile), nl.

add_pred_track2 :-
        PredictionFile = '/tmp/ECML_DATA/blastgf_single_track_predict_training_orfs.pl',
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf.pl',
        run_model(chunk_ref_annot,add_reference_track([ChunkFile,PredictionFile],[],OutFile)),
        write('here: '), write(OutFile), nl.

add_pred_track3 :-
        PredictionFile = '/tmp/ECML_DATA/blastgf_single_track_predict_training_orfs.pl',
        ChunkFile = '/tmp/ECML_DATA/train_orfs.pl',
        run_model(chunk_ref_annot,add_reference_track([ChunkFile,PredictionFile],[],OutFile)),
        write('here: '), write(OutFile), nl.

add_pred_track4 :-
        PredictionFile = '/tmp/ECML_DATA/cod_pref_trainSet_predictions.pl',
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf.pl',
        run_model(chunk_ref_annot,add_reference_track([ChunkFile,PredictionFile],[],OutFile)),
        write('here: '), write(OutFile), nl.

add_pred_track5 :-
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_and_codpref.pl',
        lost_sequence_file('NC_000913_ptt', RefFile),
        run_model(parser_ptt,annotate([RefFile],[],RefFileParsed)),
        run_model(chunk_ref_annot, add_reference_track([ChunkFile,RefFileParsed], [], OrfRef)),
        write('here: '), write(OrfRef), nl.


%%%%%%
% Combiner model (Straight version)

train_combiner_model(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_and_codpref_and_ref.pl',
        run_model(cons_and_codon,parallel_learn_combiner([TrainingDataFile],[],ParamsFile)). 

combiner_predict_train(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_and_codpref.pl',
        train_combiner_model(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_combiner([ParamsFile,ChunkFile],[],PredictionsFile)). 

combiner_predict_test(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf_and_codpref.pl',
        train_combiner_model(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_combiner([ParamsFile,ChunkFile],[],PredictionsFile)). 

%%%%%%
% Combiner model (Length version)

train_combiner_length_model(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        run_model(cons_and_codon,parallel_learn_combiner_length([TrainingDataFile],[],ParamsFile)). 

train_combiner_length_model_test(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/tmptest.pl',
        run_model(cons_and_codon,parallel_learn_combiner_length([TrainingDataFile],[],ParamsFile)). 

combiner_length_predict_train(PredictionsFile) :-
        %ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl.test',
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        train_combiner_length_model(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_combiner_length([ParamsFile,ChunkFile],[],PredictionsFile)). 

combiner_length_predict_test(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf_and_codpref_and_length.pl',
        train_combiner_length_model(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_combiner_length([ParamsFile,ChunkFile],[],PredictionsFile)). 







%%%%%%
% codpref_w_length

codpref_w_length_train(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        run_model(cons_and_codon,parallel_learn_codpref_w_length([TrainingDataFile],[],ParamsFile)). 

codpref_w_length_tinytrain(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/tmptest.pl',
        run_model(cons_and_codon,parallel_learn_codpref_w_length([TrainingDataFile],[],ParamsFile)). 


codpref_w_length_predict_trainset(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        codpref_w_length_train(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_codpref_w_length([ParamsFile,ChunkFile],[],PredictionsFile)). 

codpref_w_length_predict_testset(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf_and_codpref_and_length.pl',
        codpref_w_length_train(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_codpref_w_length([ParamsFile,ChunkFile],[],PredictionsFile)). 



%%%%%%
% codpref_w_blast

codpref_w_blast_train(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        run_model(cons_and_codon,parallel_learn_codpref_w_blast([TrainingDataFile],[],ParamsFile)). 

codpref_w_blast_tinytrain(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/tmptest.pl',
        run_model(cons_and_codon,parallel_learn_codpref_w_blast([TrainingDataFile],[],ParamsFile)). 


codpref_w_blast_predict_trainset(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        codpref_w_blast_train(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_codpref_w_blast([ParamsFile,ChunkFile],[],PredictionsFile)). 

codpref_w_blast_predict_testset(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf_and_codpref_and_length.pl',
        codpref_w_blast_train(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_codpref_w_blast([ParamsFile,ChunkFile],[],PredictionsFile)). 


%%%%%%
% just codpref (but with grammar)

codpref_train(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        run_model(cons_and_codon,parallel_learn_codpref([TrainingDataFile],[],ParamsFile)). 

codpref_tinytrain(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/tmptest.pl',
        run_model(cons_and_codon,parallel_learn_codpref([TrainingDataFile],[],ParamsFile)). 


codpref_predict_trainset(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_codpref_length_and_ref.pl',
        codpref_train(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_codpref([ParamsFile,ChunkFile],[],PredictionsFile)). 

codpref_predict_testset(PredictionsFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf_and_codpref_and_length.pl',
        codpref_train(ParamsFile),
        run_model(cons_and_codon,parallel_annotate_codpref([ParamsFile,ChunkFile],[],PredictionsFile)). 



%%% merge stuff:

merge_stuff_train(OutFile) :-
        ChunkFile = '/tmp/ECML_DATA/train_orfs.pl',
        lost_sequence_file('NC_000913_ptt', RefFile),
        run_model(parser_ptt,annotate([RefFile],[],RefFileParsed)),
        run_model(chunk_ref_annot, add_reference_track([ChunkFile,RefFileParsed], [], OrfRef)),
        big_blast_training_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
        merge_multiple(Files2,MergedFile),
        run_model(chunk_ref_annot, merge_extra_fields([OrfRef,MergedFile],[],OutFile)).

merge_stuff_test(OutFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_orfs.pl',

        big_blast_test_files(Files),
        remove_duplicate_hits(Files,Files1),
        add_identity_track(Files1,Files2),
        merge_multiple(Files2,MergedFile),

        run_model(chunk_ref_annot, merge_extra_fields([ChunkFile,MergedFile],[],OutFile)).


%%%%%
% Joint model:
%

%%%%%%
% Combiner model:

train_joint_model(ParamsFile) :-
        TrainingDataFile = '/tmp/ECML_DATA/train_chunks_w_seq_identity_gb.pl',
        run_model(cons_and_codon,parallel_learn_joint_model([TrainingDataFile],[],ParamsFile)). 


%%% Create golden standard files:
%
%

create_test_ref_file(F) :-
        ChunkFile = '/tmp/ECML_DATA/test_orfs.pl',
        lost_sequence_file('NC_000913_ptt', PTTFile),
        run_model(parser_ptt, annotate([PTTFile],[genome_key('NC000913')],ParsedPTTFile)),
        run_model(chunk_ref_annot, report_matches_to_chunks([ChunkFile,ParsedPTTFile],[],F)).

create_train_ref_file(F) :-
        ChunkFile = '/tmp/ECML_DATA/train_orfs.pl',
        lost_sequence_file('NC_000913_ptt', PTTFile),
        run_model(parser_ptt, annotate([PTTFile],[genome_key('NC000913')],ParsedPTTFile)),
        run_model(chunk_ref_annot, report_matches_to_chunks([ChunkFile,ParsedPTTFile],[],F)).


%% length model

run_length_annot_train(AnnotatedChunkFile) :-
        ChunkFile = '/tmp/ECML_DATA/train_chunks_w_blastgf_and_codpref_and_ref.pl',
        create_train_ref_file(F),
        % Training is not really, necessary since we do not use the scores
        run_model(orf_length, simple_range_model_learn([F],[],ParamsFile)),
        run_model(orf_length, simple_range_model_annotate([ChunkFile,ParamsFile],[],AnnotatedChunkFile)).

run_length_annot_test(AnnotatedChunkFile) :-
        ChunkFile = '/tmp/ECML_DATA/test_chunks_w_blastgf_and_codpref.pl',
        create_train_ref_file(F),
        % Training is not really, necessary since we do not use the scores
        run_model(orf_length, simple_range_model_learn([F],[],ParamsFile)),
        run_model(orf_length, simple_range_model_annotate([ChunkFile,ParamsFile],[],AnnotatedChunkFile)).
p
