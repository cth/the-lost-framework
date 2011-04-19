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


%%% codon pref train

make_training_data(TrainingDataFile) :-
        lost_data_directory(DatDir),
        atom_concat(DatDir,'mini_train.pl',ChunkFile),
        lost_sequence_file('NC_000913_ptt', RefFile),
        run_model(parser_ptt,annotate([RefFile],[],RefFileParsed)),
        run_model(chunk_ref_annot, add_reference_track([ChunkFile,RefFileParsed], [], OrfRef)),
        run_model(codon_preference,parallel_learn([OrfRef],[],ParamsFile)).

cod_pref_predict :-
        lost_data_directory(DatDir),
        atom_concat(DatDir,'mini_train.pl',ChunkFile),
        ParamsFile = '/tmp/ECML_DATA/NC_000913_codpref_params.pl',
        run_model(codon_preference,parallel_annotate([ParamsFile,ChunkFile],[],OutputFile)),
        write('predictions written to :'), write(OutputFile),nl.

