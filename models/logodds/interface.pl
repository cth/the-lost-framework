% Simple model to compute the logodds given two files that include viterbi probability of the same ORFs

:- ['../../lost.pl'].                                                                   


% Input Format Specification
lost_input_formats(annotate,[text(prolog(ranges(_))),text(prolog(ranges(_)))]).
% Output Format Specification
lost_output_format(annotate,_,text(prolog(ranges(_)))).



% Definition of annotate

annotate([InputFile_HMM,InputFile_Null_Model],[],OutputFile) :-
        write('Log Odds computation:'),nl,
        terms_from_file(InputFile_HMM,Terms_HMM),
        terms_from_file(InputFile_Null_Model,Terms_Null),
        open(OutputFile,write,Stream_Out),
        compute_and_save_annotations(Stream_Out,Terms_HMM,Terms_Null).



%----
% Utils annotate
%----

% End of the recursion
compute_and_save_annotations(Stream_Out,[],[]) :-
        !,
        close(Stream_Out).

% Error: InputFile with a different size
compute_and_save_annotations(_Stream_Out,Terms_HMM,[]) :-
        Terms_HMM \= [],
        !,
        write('Problem: Logodds computation should be made on the same set of ORFs'),nl.

% Error: InputFile with not the same size
compute_and_save_annotations(_Stream_Out,[],Terms_Null) :-
        Terms_Null \= [],
        !,
        write('Problem: Logodds computation should be made on the same set of ORFs'),nl.


% Normal case:

compute_and_save_annotations(Stream_Out,[Term_HMM|Rest_Terms_HMM],[Term_Null|Rest_Terms_Null]) :-
        Term_HMM =.. [Functor_HMM,Key,Left,Right,Strand,Frame,Extra_Term_HMM],
        Term_Null =.. [_Functor_Null,Key,Left,Right,Strand,Frame,Extra_Term_Null],
        !,
        member(viterbi_probability(P_HMM),Extra_Term_HMM),
        member(viterbi_probability(P_Null),Extra_Term_Null),
        LogOdds is P_HMM-P_Null,
        Annot =.. [Functor_HMM,Key,Left,Right,Strand,Frame,[logodds(LogOdds)|Extra_Term_HMM]],
        write(Stream_Out,Annot),
        write(Stream_Out,'.'),nl(Stream_Out),
        compute_and_save_annotations(Stream_Out,Rest_Terms_HMM,Rest_Terms_Null).


% Problem: ORF not the same
% Error: InputFile with not the same size
compute_and_save_annotations(_Stream_Out,_Terms_HMM,_Terms_Null) :-
        !,
        write('Problem: Logodds computation should be made on the same set of ORFs'),nl.

