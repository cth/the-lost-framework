:- ['../lost.pl'].
:- lost_include_api(interface).
% Series of script for several parsers of data


% Parser of *.fna, Options default = [] or [list(Num)]

parser_fna(Name_FNA_File,Name_GBK_File,Options) :-
        lost_sequence_file(Name_FNA_File,FNA_File),
        lost_sequence_file(Name_GBK_File,GBK_File),
        run_model(parser_fna, % Name of model
                  annotate([FNA_File,GBK_File], % A list of Input Files
                           Options,          % Options
                           OutputFile)), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


parser_fna(Name_FNA_File,Name_GBK_File,Options,OutputFile) :-
        lost_sequence_file(Name_FNA_File,FNA_File),
        lost_sequence_file(Name_GBK_File,GBK_File),
        run_model(parser_fna, % Name of model
                  annotate([FNA_File,GBK_File], % A list of Input Files
                           Options,          % Options
                           OutputFile)), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


% Parser of *.ptt from Genbank

parser_ptt(Name_PTT_File) :-
        lost_sequence_file(Name_PTT_File,PTT_File),
        run_model(parser_ptt, % Name of model
                  annotate([PTT_File], % A list of Input Files
                           [],          % Options
                           OutputFile)), % Output File
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


parser_ptt(Name_PTT_File,OutputFile) :-
        lost_sequence_file(Name_PTT_File,PTT_File),
        run_model(parser_ptt, % Name of model
                  annotate([PTT_File], % A list of Input Files
                           [],          % Options
                           OutputFile)), % Output File
        write('Parsing succeeds!! see.: '),
        write(OutputFile).



% Parser of a Easygene report 
parser_easygene(Report_Name) :-
        lost_sequence_file(Report_Name,InputFile),
        run_model(parser_easygene, % Name of model
                  annotate([InputFile],     % A list of Input Files
                           [],          % Options
                           OutputFile)), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).




% Parser of a Easygene report 
parser_easygene(Report_Name,OutputFile) :-
        lost_sequence_file(Report_Name,InputFile),
        run_model(parser_easygene, % Name of model
                  annotate([InputFile],     % A list of Input Files
                           [],          % Options
                           OutputFile)), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


% Parser of a Easygene report 
parser_genemark(Report_Name) :-
        lost_sequence_file(Report_Name,InputFile),
        run_model(parser_genemark, % Name of model
                  annotate([InputFile],     % A list of Input Files
                           [],          % Options
                           OutputFile)), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).


% Parser of a Easygene report 
parser_genemark(Report_Name,OutputFile) :-
        lost_sequence_file(Report_Name,InputFile),
        run_model(parser_genemark, % Name of model
                  annotate([InputFile],     % A list of Input Files
                           [],          % Options
                           OutputFile)), % Output File
        
        write('Parsing succeeds!! see.: '),
        write(OutputFile).



% Parser Blast output 2 Ole 
parser_blast(XML_File,List_Ids2,All_alignments) :-
        remove_2_lines(XML_File,XML_File2),
        run_model(parser_blastxml,
                  annotate([XML_File2],
                           [],
                           Outputfile)),
        consult(Outputfile),
        blast(_Query_Id,_,Data),
        (Data = [] ->  % No Hits Founds
            List_Ids2 = [],
            All_alignments = []
        ;
            findall(Id,blast(_,Id,_),List_Ids),
            remove_dups(List_Ids,List_Ids2),
            build_uplets(All_alignments)
        ).



% build_uplets


build_uplets(Uplets) :-
        findall(Id,blast(_,Id,_),List_Ids),
        findall(Firstpos,(blast(_,_,Data),member('Hsp_query-from'(Firstpos),Data)),List_First),
        findall(Posafter,(blast(_,_,Data),member('Hsp_query-to'(Posafter),Data)),List_After),
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




remove_2_lines(XML_File,XML_File2) :-
        lost_config(lost_base_directory,Lost_Dir),
        atom_concat(Lost_Dir,'models/chunk_aa_conservation/tblastn2.aln',XML_File2),
        open(XML_File,read,Stream_In),
        open(XML_File2,write,Stream_Out),
        read_line(Stream_In,_),
        read_line(Stream_In,_),
        read_line(Stream_In,CodeList),
        remove_2_lines_rec(Stream_In,CodeList,Stream_Out),
        close(Stream_In),
        close(Stream_Out).


remove_2_lines_rec(_Stream_In,[eof],_Stream_Out) :-
        !.


remove_2_lines_rec(Stream_In,Codelist,Stream_Out) :-
        atom_codes(Atom,Codelist),
        write(Stream_Out,Atom),
        nl(Stream_Out),
        read_line(Stream_In,New_CodeList),
        remove_2_lines_rec(Stream_In,New_CodeList,Stream_Out).
