% Series of script that plays with Easygene


% Generation of a db based on a Easygene report
test_easygene_parser(OutputName) :-
        lost_sequence_from_file(eg_U00096,InputFile),
        get_annotation_file(parser_easygene,                               % Name of model
                            [Input_File],                                  % A list of Input Files
                            [option(output_name,OutputName],               % Options
                            OutputFile), % Output File
        write(OutputFile).
