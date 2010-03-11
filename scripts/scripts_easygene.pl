% Series of script that plays with Easygene


% Generation of a db based on a Easygene report
test_easygene_parser :-
        lost_sequence_file(eg_U00096,InputFile),
        get_annotation_file(parser_easygene,                               % Name of model
                            [InputFile],                                  % A list of Input Files
                            [],          % Options
                            OutputFile), % Output File
        write(OutputFile).
