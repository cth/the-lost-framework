% Script to used the lost_genefinder: HMM based on a duration model


:- ['../lost.pl'].
:- lost_include_api(misc_utils).
:- lost_include_api(interface).
:- lost_include_api(stats).
:- lost_include_api(autoAnnotations).
:- ['../data/orf_chopper_test.seq'].
:- ['../data/gb_fragment.seq'].

%----------
% Learning:
%      Step1: ORF chopping from Ole
%      Step2: Select N ORF detected as hard or medium or easy gene
%      Step3: Select N ORF known as not coding
%      Step4: Generate the correct annotation
%      Step5: Learn from PRISM
%---------
script_learning(Gene_Type) :-
        consult_gene(Gene_Type),
        get_coding_ORF(Data_Coding,Starts_Position),
        get_non_coding_ORF(Data_nonCoding),
        generation_annotation_coding(Data_Coding,Starts_Position,Gene_Type,Annotation_Coding),
        build_annotation_noncoding(Data_nonCoding,Gene_Type,Annotation_Non_Coding),
        map(build_learning_terms,Annotation_Coding,Terms_Coding),
        map(build_learning_terms,Annotation_Non_Coding,Terms_Non_Coding),
        pick_a_given_number(400,Terms_Coding,Terms_Coding2),
        pick_a_given_number(400,Terms_Non_Coding,Terms_Non_Coding2),
        append(Terms_Coding2,Terms_Non_Coding2,Terms_Learning),
        prism('../models/lost_genefinder/lost_genefinder.psm'),
        learn(Terms_Learning),
        atom_concat('../models/lost_genefinder/Parameters/',Gene_Type,Proba_File),
        atom_concat(Proba_File,'.prb',Proba_File2),
        check_or_fail(
		      save_sw(Proba_File2),
		      error(could_not_save_parameter_to_file(_ParametersFile))).
        


% Learning for the combined models


script_learning_combined(Gene_Type) :-
        consult_gene(Gene_Type),
        get_coding_ORF(Data_Coding,Starts_Position),
        get_non_coding_ORF(Data_nonCoding),
        generation_annotation_coding(Data_Coding,Starts_Position,Gene_Type,Annotation_Coding),
        build_annotation_noncoding(Data_nonCoding,Gene_Type,Annotation_Non_Coding),
        map(build_learning_terms,Annotation_Coding,Terms_Coding),
        map(build_learning_terms,Annotation_Non_Coding,Terms_Non_Coding),
        pick_a_given_number(400,Terms_Coding,Terms_Coding2),
        pick_a_given_number(400,Terms_Non_Coding,Terms_Non_Coding2),
        append(Terms_Coding2,Terms_Non_Coding2,Terms_Learning),
        prism('../models/lost_genefinder/lost_genefinder.psm'),
        learn(Terms_Learning),
        atom_concat('../models/lost_genefinder/Parameters/',Gene_Type,Proba_File),
        atom_concat(Proba_File,'.prb',Proba_File2),
        check_or_fail(
		      save_sw(Proba_File2),
		      error(could_not_save_parameter_to_file(_ParametersFile))).

%---
% utils script learning
%---

consult_gene(Gene_Type) :-
        atom_concat('../tmp/',Gene_Type,Gene_File),
        atom_concat(Gene_File,'_to_find.pl',Gene_File2),
        consult(Gene_File2).


get_coding_ORF(Data_Coding,Starts_Position) :-
        findall([Data,(Min,Starts_Position)],(chunk(_,Min,Max,Data,Dir,_,_,_),hard_to_find(Starts_Position,Max,Dir,_)),Result),
        divide_list(Result,[Data_Coding,Starts_Position]).



get_non_coding_ORF(Data_nonCoding) :-
        findall(Max,gb(_,Max,_,_,_),Res),
        findall(Data,(chunk(_,_Min,Max1,Data,_,_,_,_),not_member(Max1,Res)),Data_nonCoding).
        
        
% Predicates usefull combine with a map
build_learning_terms(Params,Term) :-
        Term =.. [hmm_lost|Params].


divide_list([],[[],[]]) :-
        !.

divide_list([[A,B]|Rest],[[A|Rest_A],[B|Rest_B]]) :-
        divide_list(Rest,[Rest_A,Rest_B]).



generation_annotation_coding([],[],_Gene_Type,[]) :-
        !.
 
generation_annotation_coding([Data|Data_Coding],[(Start_ORF,Start_Coding)|Starts_Position],Gene_Type,[[Data,Gene_Type,Annotation]|Rest_Annotation]) :-
        generate_list_annotation(Data,Start_ORF,Start_Coding,Annotation),
        generation_annotation_coding(Data_Coding,Starts_Position,Gene_Type,Rest_Annotation).
        
        
generate_list_annotation([],_Start_ORF,_Start_Coding,[]) :-
        !.


generate_list_annotation([_N|Rest_Data],Start_ORF,Start_Coding,[1|Rest_Annot]) :-
        Start_ORF >= Start_Coding,
        !,
        generate_list_annotation(Rest_Data,Start_ORF,Start_Coding,Rest_Annot).
        

generate_list_annotation([_N|Rest_Data],Start_ORF,Start_Coding,[0|Rest_Annot]) :-
        Start_ORF1 is Start_ORF+1,
        generate_list_annotation(Rest_Data,Start_ORF1,Start_Coding,Rest_Annot).


build_annotation_noncoding([],_Gene_Type,[]) :-
        !.

build_annotation_noncoding([Data|Data_nonCoding],Gene_Type,[[Data,Gene_Type,Annotation]|Annotation_Non_Coding]) :-
        generate_list_noncoding(Data,Annotation),
        build_annotation_noncoding(Data_nonCoding,Gene_Type,Annotation_Non_Coding).



generate_list_noncoding([],[]) :-
        !.

generate_list_noncoding([_V|Rest_Data],[0|Rest_Annotation]) :-
        generate_list_noncoding(Rest_Data,Rest_Annotation).


pick_a_given_number(0,_,[]) :-
        !.

pick_a_given_number(_Num,[],[]) :-
        !.

pick_a_given_number(Num,[Elt|Rest],[Elt|Rest_Res]) :-
        !,
        Num1 is Num-1,
        pick_a_given_number(Num1,Rest,Rest_Res).


%--------------
% Annotation:
%      Step1: Input ORF chopping from Ole
%      Step2: Annotated all the ORF ()
%
%
%-------------

script_annotation(ChunkFile_Name,Type_Duration,Result_File) :-
        lost_sequence_file(ChunkFile_Name,ChunkFile),
        run_model(length_genefinder,
		  annotate([ChunkFile],
			   [type_duration(Type_Duration)], % Options Gene_Type
			   Result_File)),
        write('Lost annotations succeed!!'),nl.
        


test :-
        script_annotation('orf_chopper_test',range([100,600]),O).  


