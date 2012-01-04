:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(prism_parallel).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(stats).
:- lost_include_api(viterbi_learn).

:- task(annotate([text(prolog(prism_parameters)),text(prolog(ranges(gene)))],[],text(prolog(ranges(gene))))).
:- task(parallel_annotate([text(prolog(prism_parameters)),text(prolog(ranges(gene)))],[],text(prolog(ranges(gene))))).
:- task(parallel_learn([text(prolog(ranges(gene)))],[],text(prolog(prism_parameters)))).

set_params:- restore_sw('params_half.gen').

%% annotate(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ ParamsFile, InputFile ]
% == 
% Model is first parameterized using ParamsFile. 
% Eaah putative gene/orf/chunk (range facts) in InputFile is annotated using the model.
% The gene range facts are expected to have a =|sequence|= extra field, which holds a list with nucleotide sequence spanned by the range.
% If a input orf is annotated entirely as non-coding, then it will _not_ be written to the output file.
% Otherwise, the orf will be written to the output file with and additional extra field =|codon_pref(AnnotList)|=. 
% AnnotList is a list of zeroes and ones where 0 means annotated as non-coding and 1 annotated as coding.
annotate([ParamsFile,InputFile],_Options,OutputFile) :-
	write('Codon preference genefinder: '),nl,
	prismAnnot('codon_pref'), % Load the actual PRISM model
	restore_sw(ParamsFile),
	% Building of the Input for the annotations
    consult(InputFile),
    findall(Chunk,get_chunk(Chunk),List_Chunk),
    findall([ID,Left,Right,Dir,Frame],chunk(ID,Left,Right,Dir,Frame,_),List_Ranges_Chunk),
	% Computation of annotations
    open(OutputFile,write,Stream_Out),
    compute_and_save_annotations(Stream_Out,1,List_Chunk,List_Ranges_Chunk).


%% parallel_annotate(+InputFiles,+Options,+OutputFile)
% Same as annotate/3 but runs with 10 threads in parallel.
parallel_annotate([ParamsFile,InputFile],_opts,OutputFile) :-
        split_file(InputFile,500,'cod_pref_input', '.pl',ResultingFiles),
        open('input_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_predict.sh ','10 ', ParamsFile, ' ', OutputFile, ' input_files.list'], Cmd),
        system(Cmd).

get_chunk(Chunk):-
		chunk(_, _, _, D, _,[sequence(ChunkRev),_,_]),
        (D = '-' -> reverse(ChunkRev,Chunk)
        ;
        Chunk = ChunkRev).

test :-
	annotate(['test_chunks.gen'],[], 'testout.pl').

% compute_and_save_annotations(+Stream,+ORF,+Type_Gene,+List_Ranges,+Dir,+Frame)
% Compute the annotation and write into Stream when a coding region is found.
compute_and_save_annotations(Stream_Out,_Nb_Iterations,[],[]) :-
        !,
        close(Stream_Out).

compute_and_save_annotations(Stream_Out,Nb_Iterations,[ORF|Rest_ORF],[Range|Rest_Ranges]) :-
        check_or_fail(viterbiAnnot(codpref_annot(ORF,Annotation),_P),
                      error('Viterbi computation failed (Lost GeneFinder)')
                     ),
        build_term_for_annotation(codon_pref,Range,Annotation,Term),
        (var(Term) ->
            true
        ;
            writeq(Stream_Out,Term),write(Stream_Out,'.'),nl(Stream_Out)
        ),
        Number is Nb_Iterations mod 100,
        (Number == 1 -> write(Nb_Iterations) ; write('.')),
        (Number == 0 ->
            table_remove(codpref_annot(_,_)),
            table_remove(codpref_annot_rec(_,_,_))            
        ;
            true
        ),
        Nb_Iterations1 is Nb_Iterations+1,
        !,
        compute_and_save_annotations(Stream_Out,Nb_Iterations1,Rest_ORF,Rest_Ranges).

% build_term_for_annotation : Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,[ID,Left,Right,Dir,Frame],AnnotationRev,Term) :-
        (Dir = '-' -> reverse(AnnotationRev,Annotation)
        ;
        Annotation = AnnotationRev),
        
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[ID,Functor,Left,Right,Dir,Frame,[codon_pref(Annotation),start(Start)]].


build_term_for_annotation(_Functor,_Range,_Annotation,_Term).

% first_coding(++Left,++Value,++Annotation,--Start)
first_coding(_Left,_Value,[],_Start) :-
        fail.

first_coding(Start,Value,[Value|_Rest_Annotations],Start) :-
        !.

first_coding(Left,Value,[_Annotation|Rest_Annotations],Start) :-
        !,
        Left1 is Left+1,
        first_coding(Left1,Value,Rest_Annotations,Start).


%%%%%%%%%%%%%%%
%%%% learning %
%%%%%%%%%%%%%%%

%% parallel_learn(+InputFiles, +Options, +OutputFile)
% ==
% InputFiles = [ TrainingDataFile ]
% ==
% Train the gene finder using the genes/orfs/chunks in TrainingDataFile. 
% TrainingDataFile is expected to contain two =|extra|= fields: =|sequence|= contains the nucleotide sequence as a list and a =|ref_annot|= which contains a list of zeros (non-coding) and ones (coding) according to a golden standard.
parallel_learn([InputFile],[],ParamsFile) :-
        lost_tmp_directory(Tmp),
        atom_concat_list([Tmp,'codon_pref_full_train.pl'],TrainingFile),
        make_training_file(InputFile,TrainingFile),
        split_file(TrainingFile,1000,'cod_pref', '.pl',ResultingFiles),
        open('training_files.list',write,OutS),
        forall(member(File,ResultingFiles), (write(OutS,File), write(OutS,'\n'))),
        close(OutS),
        atom_concat_list(['./parallel_train.sh ','10 ', ParamsFile, ' training_files.list'], Cmd),
        system(Cmd).


% new verision using refseq'ed chunk in chunk_ref_file
test_make_data :-	
	make_training_file('test_chunk_ref.gen', 'test_train.gen').

make_training_file(Chunk_Ref,Training_File):-
	open(Chunk_Ref,read,Chunk_Stream,[alias(chunkin)]),
	open(Training_File,write,Train_Stream,[alias(trainout)]),
		
	read(Chunk_Stream,Chunk_Term),
	make_training_file_rec(0,Chunk_Term, Chunk_Stream, Train_Stream),!,	% green cut, for tail-recursion optimization  
	
	close(Train_Stream),
	close(Chunk_Stream).


make_training_file_rec(Count,end_of_file, _Chunk_Stream, _Train_Stream):- nl, write('ok - '), write(Count), writeln(' training goals constructed').	

make_training_file_rec(Count,chunk(_Id,Left,Right,_Dir,_Frame,[ref_annot(Ref_Annot),sequence(Chunk_Annot)|_rest]), Chunk_Stream, Train_Stream):-
	NewCount is Count + 1,
	N is NewCount mod 500,
	(N = 0 -> write('.')
	; true
	),	
	Num is (Right-Left+1)mod 3, 
	append(Ref_Annot,[end(Num)],Ref_Annot2), % ... appending number of trailing nucleotides
	Training_Goal =.. [codpref,Chunk_Annot,Ref_Annot2],
	write(Train_Stream,Training_Goal),
	write(Train_Stream,'.'),
	nl(Train_Stream),
	read(Chunk_Stream,Next_Chunk_Term),
	make_training_file_rec(NewCount, Next_Chunk_Term, Chunk_Stream, Train_Stream).


makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
	N > 0,
	M is N-1,
	makelist(M,X,Rest).
	
test_train:-
	train('test_train.gen','test_params.gen').	
train(Training_file,Parameter_file):-
	prismAnnot('codon_pref'), % Load the actual PRISM model
	viterbi_learn_file(Training_file),
	save_sw(Parameter_file).

out_file_name(InFile,OutFile) :-
        atom_concat(InFile,'.processed',OutFile).


create_goals([],[],_Num,[]) :-
        !.

create_goals([FileIn|RestIn],[FileOut|RestOut],Num,[Goal|Rest_Goal]) :-
        Goal = (                 
                 [interface],
                 annotate([FileIn],_Options,FileOut)
                 ),
        Num1 is Num+1,
        create_goals(RestIn,RestOut,Num1,Rest_Goal).
