:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(stats).

% Option declaration
lost_option(lost_best_annotation,type_gene,hard,'Specified with which kind of data the genefinder is set').
lost_option(lost_best_annotation,use_parameter_file,yes,'Load parameters from the parameter file').

% Input Format Specification
lost_input_formats(lost_best_annotation,[prolog(sequence(_))]).
% Output Format Specification
lost_output_format(lost_best_annotation,_,text(prolog(ranges(_)))).


% 
lost_best_annotation([InputFile],Options,OutputFile) :-                                 
	write('Lost genefinder: '),nl,                                                         
        prismAnnot('lost_genefinder'), % Load the actual PRISM model                                         
        get_option(Options,use_parameter_file,UseParamFile),
        get_option(Options,type_gene,Type_Gene),
        (UseParamFile == yes ->
            atom_concat('./Parameters/',Type_Gene,Path),
            atom_concat(Path,'.prb',ParamFile),
            write(restore_sw(ParamFile)),nl,
            restore_sw(ParamFile)
        ;
            true),
       % Building of the Input for the annotations
        consult(InputFile),
        chunk(_Key,_Min,_Max,_Data,Dir,Frame,_,_),
        findall(Data,chunk(_Key,_,_,Data,_,_,_,_),List_ORF),
        findall([Min,Max],chunk(_Key,Min,Max,_,_,_,_,_),List_Ranges_ORF),
        % Computation of annotations
        compute_annotations(List_ORF,Type_Gene,List_Annotations),
        % Save the annotations in the right format
        write(save_annotation(lost_prediction,List_Ranges_ORF,List_Annotations,Dir,Frame,OutputFile)),nl,
        save_annotation(lost_prediction,List_Ranges_ORF,Dir,Frame,List_Annotations,OutputFile). % Måske, something more generic to include in io 
        %save_annotation_to_sequence_file(genemark_genefinder,70,Annotation,OutputFile).
				





compute_annotations([],_Type_Gene,[]) :-
        !.

compute_annotations([ORF|Rest_ORF],Type_Gene,[Annotation|Rest_Annotations]) :-
        check_or_fail(viterbiAnnot(hmm_lost_annot(ORF,Type_Gene,Annotation),_P),
                      error('Viterbi computation failed (Lost GeneFinder)')
                     ),
        write(Annotation),nl,
        compute_annotations(Rest_ORF,Type_Gene,Rest_Annotations).



save_annotation(lost_prediction,List_Ranges,Dir,Frame,List_Annotations,OutputFile) :-
        build_terms_for_annotation(lost_prediction,List_Ranges,Dir,Frame,List_Annotations,Terms),
        terms_to_file(OutputFile,Terms).


% Test save_annotations
%Annots = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
% Ranges = [[1,48],[49,114],[115,255],[256,2799]]

build_terms_for_annotation(_Functor,[],_Dir,_Frame,[],[]) :-
        !.

build_terms_for_annotation(Functor,[[Left,Right]|Rest_Ranges],Dir,Frame,[Annotation|Rest_Annotations],[Term|Rest_Terms]) :-
        first_coding(Left,1,Annotation,Start),
        !,
        Term =..[Functor,Start,Right,Dir,Frame,Annotation],
        build_terms_for_annotation(Functor,Rest_Ranges,Dir,Frame,Rest_Annotations,Rest_Terms).



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

%%%divided_file_by_frame() :-
%%%        findall([data,Min,Max,'+',Data],(gene(Min,Max,'+',Data),get_frame(Min,'+',1)),Result_Frame1)
        





%%%get_frame(Min,'+',Frame) :-
%%%        !,
%%%        Frame is Min mod 3.
      


%%%get_frame(Min,'-',Frame) :-
%%%        !,
%%%        Frame1 is Min mod 3,
%%%        (Frame1 = 3 ->
%%%            Frame = 0
%%%        ;
%%%            Frame1 = Frame
%%%        ).


