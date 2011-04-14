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

% 
annotate([InputFile],_Options,OutputFile) :-                                 
	write('Codon preference genefinder: '),nl,                                                         
        prismAnnot('codon_pref'), % Load the actual PRISM model                                         
        % Building of the Input for the annotations
        consult(InputFile),
        chunk(_ID,_Left,_Right, Dir, Frame,_Extra),
        findall(Data, chunk(_, _, _, _, _,[sequence(Data),_,_]),List_ORF),	 
        findall([Left,Right],chunk(_,Left,Right,_,_,_),List_Ranges_ORF),
	 % Computation of annotations
        open(OutputFile,write,Stream_Out),
        compute_and_save_annotations(Stream_Out,1,List_ORF,List_Ranges_ORF,Dir,Frame).
        % Save the annotations in the right format
        %write(save_annotation(lost_prediction,List_Ranges_ORF,List_Annotations,Dir,Frame,OutputFile)),nl,
        %save_annotation(lost_prediction,List_Ranges_ORF,Dir,Frame,List_Annotations,OutputFile). % Måske, something more generic to include in io 
        %save_annotation_to_sequence_file(genemark_genefinder,70,Annotation,OutputFile).

%annotate([InputFile],Options,OutputFile) :-
%	get_option(Options,optimized,true),
%	subtract(Options,[optimized(true)],NewOptions1),
%	append([optimized(false)], NewOptions1, NewOptions2),
%	terms_to_file('options.pl',[original_options(NewOptions2)]),
%	lost_tmp_directory(Tmp),
%	atom_concat(Tmp,'lost_genefinder_chunk',Prefix),
%	split_file(InputFile, 10, Prefix, '.pl'),
%	atom_concat(Tmp,'lost_genefinder_chunk*pl',InputFilePattern),
%	atom_concat_list(['sh parallel_predict.sh ', OutputFile, ' ', InputFilePattern], Cmd),
%	system(Cmd).
	

test :-
	annotate(['test_chunks.gen'],[], 'testout.pl').

% compute_and_save_annotations(++Stream,++ORF,++Type_Gene,++List_Ranges,++Dir,++Frame)
% Compute the annotation and write into Stream when a coding region is found.

compute_and_save_annotations(Stream_Out,_Nb_Iterations,[],[],_Dir,_Frame) :-
        !,
        close(Stream_Out).

compute_and_save_annotations(Stream_Out,Nb_Iterations,[ORF|Rest_ORF],[Range|Rest_Ranges],Dir,Frame) :-
        check_or_fail(viterbiAnnot(codpref_annot(ORF,Annotation),_P),
                      error('Viterbi computation failed (Lost GeneFinder)')
                     ),
        build_term_for_annotation(codon_pref,Range,Dir,Frame,Annotation,Term),
        (var(Term) ->
            true
        ;
            write(Stream_Out,Term),write(Stream_Out,'.'),nl(Stream_Out)
        ),
        Number is Nb_Iterations mod 500,
        (Number == 1 -> write(Nb_Iterations) ; write('.')),
        (Number == 0 ->
            table_remove(hmm_lost_annot(_,_)),
            table_remove(hmm_lost_annot(_,_,_))
        ;
            true
        ),
        Nb_Iterations1 is Nb_Iterations+1,
        !,
        compute_and_save_annotations(Stream_Out,Nb_Iterations1,Rest_ORF,Rest_Ranges,Dir,Frame) .
    



% build_term_for_annotation : Terms is a variable if the annotation is a list of 0, otherwise a term with a range for the coding region
build_term_for_annotation(Functor,[Left,Right],Dir,Frame,Annotation,Term) :-
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[Functor,Left,Right,Dir,Frame,[codon_pref(Annotation),start(Start)]].


build_term_for_annotation(_Functor,_Range,_Dir,_Frame,_Annotation,_Term).

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
test_make_data :-	
	make_training_file('test_chunks.gen','test_genes.gen', 'test_train.gen').

make_training_file(Chunk_Annot,Ref_Annot,Training_File):-
	open(Chunk_Annot,read,Chunk_Stream,[alias(chunkin)]),
	open(Ref_Annot,read,Ref_Stream,[alias(refin)]),
	open(Training_File,write,Train_Stream,[alias(trainout)]),
		
	read(Chunk_Stream,Chunk_Term),
	make_training_file_rec(0,Chunk_Term, Chunk_Stream, Ref_Stream, Train_Stream),!,	% green cut, for tail-recursion optimization  
	
	close(Train_Stream),
	close(Ref_Stream),
	close(Chunk_Stream).


make_training_file_rec(Count,end_of_file, _Chunk_Stream, _Ref_Stream, _Train_Stream):- nl, write('ok - '), write(Count), writeln(' training goals constructed').	

make_training_file_rec(Count,chunk(Id,Left,Right,Dir,Frame,[sequence(Chunk_Annot)|_rest]), Chunk_Stream, Ref_Stream, Train_Stream):-
	NewCount is Count + 1,
	N is NewCount mod 500,
	(N = 0 -> write('.')
	; true
	),	
	read(Ref_Stream,RefTerm),
	RefTerm =.. [genebank_annotation,Id,Left,Right,Dir,Frame,Extra_Ref_Info],	% ...or chunk mismatch?
	member(seq_annotation(Ref_Annot),Extra_Ref_Info),							% ...or missing annotation?
	Num is (Right-Left+1)mod 3, 
	append(Ref_Annot,[end(Num)],Ref_Annot2), % ... appending number of trailing nucleotides
	Training_Goal =.. [codpref,Chunk_Annot,Ref_Annot2],
	write(Train_Stream,Training_Goal),
	write(Train_Stream,'.'),
	nl(Train_Stream),
	read(Chunk_Stream,Next_Chunk_Term),
	make_training_file_rec(NewCount, Next_Chunk_Term, Chunk_Stream, Ref_Stream, Train_Stream).


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


