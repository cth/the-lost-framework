:- ['../../lost.pl'].                                                                   
:- lost_include_api(interface).                                                         
:- lost_include_api(io).                                                                                                                                        


% WARNING: TOFIX option data_available no not supported yet.


:-set_prism_flag(scaling,log_exp).
:-set_prism_flag(log_viterbi,on).



% Input Format Specification
lost_input_formats(annotate,[text(prolog(ranges(_)))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).


% Option declaration
% Annotate options
% lost_option(annotate,use_parameter_file,yes,'Load parameters from the parameter file'). Not usefull for this model
lost_option(annotate,data_available,yes,'Specify if the sequence data is available or not in Extra info of the terms that composed the Input file').
lost_option(annotate,data_functor,sequence,'Specify the name of the functor associated with the sequence data').


% 
annotate([InputFile],Options,OutputFile) :-                                 
	write('Iid model: '),nl,                                                         
        prism('iid'), % Load the actual PRISM model
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
        check_or_fail(
                      viterbi(iid(Data),Proba),
                      error(viterbi_computation_iid)
                     ),
        Term =.. [viterbi_iid,Key,Left,Right,Strand,Frame,[viterbi_probability(Proba)]],
        writeq(Stream,Term),
        write(Stream,'.'),
        nl(Stream),
        compute_and_save_annotations(Stream,Rest_Data).
                      
       

