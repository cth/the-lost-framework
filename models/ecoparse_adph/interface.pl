:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(io).                                                                                                                                        


% WARNING: TOFIX option data_available no not supported yet.



% Input Format Specification
lost_input_formats(annotate,[text(prolog(ranges(_)))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).


% Option declaration
% Annotate options
lost_option(annotate,use_parameter_file,yes,'Load parameters from the parameter file').
lost_option(annotate,data_available,yes,'Specify if the sequence data is available or not in Extra info of the terms that composed the Input file').
lost_option(annotate,data_functor,sequence,'Specify the name of the functor associated with the sequence data').


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
                      
       

