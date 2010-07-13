:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(io).                                                                                                                                        
:- lost_include_api(misc_utils).

% WARNING: TOFIX option data_available no not supported yet.



% Input Format Specification
lost_input_formats(annotate,[text(prolog(ranges(_)))]).
lost_input_formats(learn,[text(prolog(ranges(_)))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).
lost_output_format(learn,_,text(prolog(prism_swithches))).

% Option declaration
% Annotate options
lost_option(annotate,use_parameter_file,yes,'Load parameters from the parameter file').
lost_option(annotate,data_available,yes,'Specify if the sequence data is available or not in Extra info of the terms that composed the Input file').
lost_option(annotate,data_functor,sequence,'Specify the name of the functor associated with the sequence data').
% Learning options
lost_option(learn,data_available,yes,'Specify if the sequence data is available or not in Extra info of the terms that composed the Input file').
lost_option(learn,data_functor,sequence,'Specify the name of the functor associated with the sequence data').
lost_option(learn,terms_number,undefined,'Define the number of terms used to learn the parameters of the models').


% 
annotate([InputFile],Options,OutputFile) :-                                 
	write('ecoparse_adph model: '),nl,                                                         
        prism('ecoparse_adph'), % Load the actual PRISM model                                         

        %Restoration of the switches
         get_option(Options,use_parameter_file,UseParamFile),
         (UseParamFile == yes ->
             atom_concat('./Parameters/','ecoparse_adph.prb',ParamFile),
             write(restore_sw(ParamFile)),nl,
             restore_sw(ParamFile),
             atom_concat('./Parameters/','ecoparse_adph_h.prb',ParamFile_H),
             write(restore_sw_h(ParamFile)),nl,
             restore_sw_h(ParamFile_H)
         ;
             true),
      % Building of the Input for the annotations
        get_option(Options,data_available,Data_Available),
        terms_from_file(InputFile,Terms),
        (Data_Available == yes ->
            get_option(Options,data_functor,Functor_Data),
            Sequence =..[Functor_Data,_],
            findall([Data,Key,Left,Right,Strand,Frame],(member(Term,Terms),
                                                        Term =.. [_,Key,Left,Right,Strand,Frame,Extra_Info],
                                                        member(Sequence,Extra_Info),
                                                        Sequence =.. [Functor_Data,Data]
                                                       ),
                    Data_Extraction)
        ;
            true
            % TODO something clever that call get_data_from_file
            ),
      % Computation of Viterbi and save of the information in the Outputfile
        open(OutputFile,write,Stream_Out),
        compute_and_save_annotations(Stream_Out,Data_Extraction).



learn([InputFile,RawGenome],Options,OutputFile) :-
        write('ecoparse_adph learning: '),nl,                                                         
        prism('ecoparse_adph'), % Load the actual PRISM model                                         
        terms_from_file(InputFile,Terms),
        get_option(Options,data_available,Data_Available),
        get_option(Options,terms_number,Num),
        (Data_Available == yes ->
            % Case: Data available in InputFile
            get_option(Options,data_functor,Functor),
            get_data_from_terms(Terms,Functor,Num,List_Data) % Warning: same name that the one used in io but not the same arity
        ;
            get_ranges_from_terms(Terms,Num,Ranges),
            get_data_from_file(RawGenome,[ranges(Ranges)],List_Data)
        ),
      % Learning process
        map(build_learning_terms,List_Data,Learning_Terms),
        check_or_fail(learn(Learning_Terms),
                      error(learning_computation_ecoparse_adph)
                     ),
        save_sw(OutputFile),
        save_sw_h('./Parameters/ecoparse_adph_h.prb').
        
        
%----
% utils annotate
%-----


compute_and_save_annotations(Stream,[]) :-
        close(Stream).



compute_and_save_annotations(Stream,[[Data,Key,Left,Right,Strand,Frame]|Rest_Data]) :-
        % Viterbi Computation
        check_or_fail(
                      viterbi(ecoparse_adph(Data),VProba),
                      error(viterbi_computation_ecoparse_adph)
                     ),
        % Probability computation
        check_or_fail(
                      prob(ecoparse_adph(Data),Proba),
                      error(probability_computation_ecoparse_adph)
                     ),
        
        Term =.. [annotation_ecoparse_adph,Key,Left,Right,Strand,Frame,[probability(Proba),viterbi_probability(VProba)]],
        write(Stream,Term),
        write(Stream,'.'),
        nl(Stream),
        compute_and_save_annotations(Stream,Rest_Data).


%----
% utils Learning
%----




% get_data_from_terms(Terms,Functor,Num,List_Data) 
%
% Warning: same name that the one used in io but the predicate has not the  same arity

% Default value for Num = undefined that means all the value of data.
get_data_from_terms(Terms,Functor,undefined,List_Data) :-
        !,
       Sequence =..[Functor,_],
       findall(Data,(member(Term,Terms),
                     Term =.. [_,_Key,_Left,_Right,_Strand,_Frame,Extra_Info],
                     member(Sequence,Extra_Info),
                     Sequence =.. [Functor,Data]
                    ),
                    List_Data
              ).

% Case: List Terms empty before Num = 0
get_data_from_terms([],_Functor,Num,[]) :-
        Num >= 0,
        !.

% Case: Num = 0, number of data to collect reached
get_data_from_terms(_Terms,_Functor,0,[]) :-
        !.


get_data_from_terms([Term|Rest_Terms],Functor,Num,[Data|Rest_Data]) :-
        Num > 0,
        !,
       Sequence =..[Functor,_],
       Term =.. [_,_Key,_Left,_Right,_Strand,_Frame,Extra_Info],
       member(Sequence,Extra_Info),
       Sequence =.. [Functor,Data],
       Num1 is Num-1,
       get_data_from_terms(Rest_Terms,Functor,Num1,Rest_Data).



% get_ranges_from_terms(Terms,Functor,Num,List_Data) 
% Default value for Num = undefined that means all the ranges
get_ranges_from_terms(Terms,undefined,List_Ranges) :-
        !,
       findall([Left,Right],(member(Term,Terms),
                     Term =.. [_,_Key,Left,Right,_Strand,_Frame,_Extra_Info]
                    ),
                    List_Ranges
              ).

% Case: List Terms empty before Num = 0
get_ranges_from_terms([],Num,[]) :-
        Num >= 0,
        !.

% Case: Num = 0, number of data to collect reached
get_ranges_from_terms(_Terms,0,[]) :-
        !.


get_ranges_from_terms([Term|Rest_Terms],Num,[[Left,Right]|Rest_Ranges]) :-
        Num > 0,
        !,
       Term =.. [_,_Key,Left,Right,_Strand,_Frame,_Extra_Info],
       Num1 is Num-1,
       get_ranges_from_terms(Rest_Terms,Num1,Rest_Ranges).



% build_learning_terms(+Data,-Learning_Term)

build_learning_terms(Data,Learning_Term) :-
        Learning_Term =.. [ecoparse_adph,Data].
