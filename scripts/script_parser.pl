:- ['../lost.pl'].
:- lost_include_api(interface).

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


parser_fna(Name_FNA_File,Name_GBK_File,Options,OutputFile) :-
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


parser_ptt(Name_PTT_File,OutputFile) :-
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
parser_easygene(Report_Name,OutputFile) :-
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


% Parser of a Easygene report 
parser_genemark(Report_Name,OutputFile) :-
        lost_sequence_file(Report_Name,InputFile),
        get_annotation_file(parser_genemark, % Name of model
                            [InputFile],     % A list of Input Files
                            [],          % Options
                            OutputFile), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).



% Parser Blast output 2 Ole 
parser_blast(XML_File,List_Ids2,FirstPos,PosAfter,All_alignments) :-
        get_annotation_file(parser_blastxml,
                            [XML_File],
                            [],
                            Outputfile),
        consult(Outputfile),
        findall(Id,blast(_,Id,_),List_Ids),
        remove_dups(List_Ids,List_Ids2),
        blast(Query_Id,_,Data),
        member('Hsp_query-from'(FirstPos),Data),
        member('Hsp_query-to'(PosAfter),Data),
        PosAfter1 is PosAfter+1,
        member('Hsp_qseq'(QuerySeq),Data),
        Tuplet_Query = (Query_Id,FirstPos,PosAfter1,QuerySeq),
        build_uplets(Rest_tuplet),
        All_alignments = [Tuplet_Query|Rest_tuplet].



% build_uplets


build_uplets(Uplets) :-
        findall(Id,blast(_,Id,_),List_Ids),
        findall(Firstpos,(blast(_,_,Data),member('Hsp_hit-from'(Firstpos),Data)),List_First),
        findall(Posafter,(blast(_,_,Data),member('Hsp_hit-to'(Posafter),Data)),List_After),
        findall(Allignment,(blast(_,_,Data),member('Hsp_hseq'(Allignment),Data)),List_Allignements),
        build_uplets_rec(List_Ids,List_First,List_After,List_Allignements,Uplets).


build_uplets_rec([],[],[],[],[]) :-
        !.

build_uplets_rec([Id|Rest_Ids],[First|Rest_First],[After|Rest_After],[Allignment|Rest_Allignements],[Uplet|Rest_Uplets]) :-
        Uplet = (Id,First,After,Allignment),
        build_uplets_rec(Rest_Ids,Rest_First,Rest_After,Rest_Allignements,Rest_Uplets).




remove_dups([],[]) :-
        !.


remove_dups(List,Result) :-
        remove_dups_rec(List,[],Result).

remove_dups_rec([],_,[]) :-
        !.

remove_dups_rec([A|Rest],List,Result) :-
        member(A,List),
        !,
        remove_dups_rec(Rest,List,Result).


remove_dups_rec([A|Rest],List,[A|Rest_Result]) :-
        remove_dups_rec(Rest,[A|List],Rest_Result).
