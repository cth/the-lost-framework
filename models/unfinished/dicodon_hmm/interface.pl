:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_api(stats).
:- lost_include_api(viterbi_learn).

% Option declaration

% Input Format Specification
lost_input_formats(annotate,[prolog(sequence(_))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% codpref (with gene grammar)                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate([ParamsFile,InputFile],_Options,OutputFile) :-
    prism('codpref'), % Load the actual PRISM model
    restore_sw(ParamsFile),
    open(InputFile,read,StreamIn),
    open(OutputFile,write,StreamOut),
    compute_and_save_annotations(StreamIn,StreamOut,1),
    close(StreamOut),
    close(StreamIn).

parallel_annotate([ParamsFile,InputFile],_,OutputFile) :-
    split_file(InputFile,500,'annotate_input', '.pl', ResultingFiles),
    open('input_files.list',write,OutS),
    forall(member(File,ResultingFiles),(write(OutS,File),write(OutS,'\n'))),
    close(OutS),
    atom_concat_list(['./parallel_predict.sh ','12 ', 'annotate ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
    system(Cmd).

parallel_learn_codpref([ChunkFileWithRef],_,ParamsFile) :-
    lost_tmp_directory(Tmp),
    atom_concat_list([Tmp,'train_full.pl'],TrainingFile),
    make_training_file_codpref(ChunkFileWithRef,TrainingFile),
    split_file(TrainingFile,1000,'train_split', '.pl',ResultingFiles),
    open('training_files.list',write,OutS),
    forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
    close(OutS),
    atom_concat_list(['./parallel_train.sh ','12 ', 'dicodon ',  ParamsFile, ' training_files.list'], Cmd),
    system(Cmd).

learn_codpref([ChunkFileWithRef],_,ParamsFile) :-
    TrainingFile = 'training_data.pl',
    make_training_file(ChunkFileWithRef,TrainingFile),
    prism(dicodon),
    ['dicodon.psm'],
    viterbi_learn_file(TrainingFile),
    save_sw(ParamsFile).

compute_and_save_annotations(StreamIn,StreamOut) :-
    read(StreamIn,Chunk),
    ((Chunk == end_of_file) ->
        true,
        write('compute_and_save_annotations done.'),nl
        ;
        Chunk =.. [ _F,SeqId, Left, Right, Strand, Frame, Extra],
        member(sequence(Nucleotides),Extra),
        seq_triplets(Nucleotides,Triplets),
		findall(SeqWithStart,annotate_one_start(Triplets,SeqWithStart),SeqsWithStart),
		findall(P,(member(Seq,SeqsWithStart), prob(dicodon(Seq),P)),Probs),
		build_annotations(Chunk,SeqsWithStart,Probs,Annotations),
		
	
build_annotations(_Chunk,[],[],[]).
build_annotations(_Chunk,[Seq|SeqsRest],[P|Ps],[A|As]) :-
	nth(start(_),Seq,N),
	StartOffset is N*3,
    Chunk =.. [ _F,SeqId, Left, Right, Strand, Frame, Extra],
	((Strand='-') ->
		;
	)
		

make_training_file_codpref(Chunk_Annot,Training_File):-
    open(Chunk_Annot,read,Chunk_Stream),
    open(Training_File,write,Train_Stream),
    make_training_file_codpref_rec(Chunk_Stream, Train_Stream),
    close(Train_Stream),
    close(Chunk_Stream).
    
make_training_file_codpref_rec(InStream,OutStream) :-
    read(InStream,Chunk),
    ((Chunk==end_of_file) ->
        true 
        ;
        Chunk =.. [ _Functor, _SeqId, _Left, _Right, _Strand, _Frame, Extra],
        member(gb(RefAnnot),Extra),
                member(sequence(Nucleotides),Extra),
                nucleotide_triplets(Nucleotides,Triplets),
%                write(merge_nucleotides_and_prediction(Nucleotides,Prediction,Combined)),
                Goal =.. [ combiner, Triplets, RefAnnot ], 
        write(OutStream,Goal), 
        write(OutStream,'.\n'),
        make_training_file_codpref_rec(InStream,OutStream)).
