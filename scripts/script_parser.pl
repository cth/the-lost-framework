% Series of script for several parsers of data


% Parser of *.fna, Options default = [] or [list(Num)]

parser_fna(Name_FNA_File,Name_GBK_File,Options) :-
        lost_sequence_file(Name_FNA_File,FNA_File),
        lost_sequence_file(Name_GBK_File,GBK_File),
        get_annotation_file(parser_fna, % Name of model
                            [FNA_File,GBK_File], % A list of Input Files
                            Options,          % Options
                            OutputFile), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


% Parser of *.ptt from Genbank

parser_ptt(Name_PTT_File) :-
        lost_sequence_file(Name_PTT_File,PTT_File),
        get_annotation_file(parser_ptt, % Name of model
                            [PTT_File], % A list of Input Files
                            [],          % Options
                            OutputFile), % Output File
        write('Parsing succeeds!! see.: '),
        write(OutputFile).



% Parser of a Easygene report 
parser_easygene(Report_Name) :-
        lost_sequence_file(Report_Name,InputFile),
        get_annotation_file(parser_easygene, % Name of model
                            [InputFile],     % A list of Input Files
                            [],          % Options
                            OutputFile), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


% Parser of a Easygene report 
parser_genemark(Report_Name) :-
        lost_sequence_file(Report_Name,InputFile),
        get_annotation_file(parser_genemark, % Name of model
                            [InputFile],     % A list of Input Files
                            [],          % Options
                            OutputFile), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).
