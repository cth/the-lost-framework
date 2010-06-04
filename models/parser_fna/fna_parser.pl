%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NAME :
%      fna_parser.pl -- LOST tool v0.0
%
% FUNCTION :
%        Extraction of information from FNA file
%
% HISTORIQUE :
%      O.T.L 23/10/2009: creation
%      M.P   18/01/2010: Integration in the LOST toolbox and few modifications
%      M.P   04/06/2010: Range option
%  
% DESCRIPTION :
%            Generation of terms from a FNA file
%            Terms Format: data(Genbank_key,Position_Start,Position_End,List_Data) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%---------
% Parser of *.fna
%---------
% parser_fna(++Options,++Fna_File,++GBK_File,--Output_File)
% parser_fna(++Options,++Fna_File,++GenBank_Key,++N_BP--Output_File)
% Description: Parser of FNA. Different output format can be used
% given Options
% Options: - default = []: generation of a list of nucleotids
%          - list(Size): chunking of the genome into list of nucleotids with a length Size
%          - range([Min,Max])
%----------
parser_fna(Options,FNA_File,GBK_File,Output_File) :-
        open(GBK_File,read,GBK_Stream),
        get_info_gbk(GBK_Stream,GeneBank_Key,N_BP),
        close(GBK_Stream),
        open(FNA_File,read,FNA_Stream),
        open(Output_File,write,Output_Stream),
        readline(FNA_Stream,First_Line),
        atom_codes(Comment,[37|First_Line]),
        write(Output_Stream,Comment),
        nl(Output_Stream),
        parser_fna_data(Options,FNA_Stream,GeneBank_Key,N_BP,Output_Stream),
        close(FNA_Stream),
        close(Output_Stream).

 

parser_fna(Options,FNA_File,GeneBank_Key,N_BP,Output_File) :-
        open(FNA_File,read,FNA_Stream),
        open(Output_File,write,Output_Stream),
        readline(FNA_Stream,First_Line),
        atom_codes(Comment,[37|First_Line]),
        write(Output_Stream,Comment),
        nl(Output_Stream),
        parser_fna_data(Options,FNA_Stream,GeneBank_Key,N_BP,Output_Stream),
        close(FNA_Stream),
        close(Output_Stream).



%Default 
parser_fna_data(Options,FNA_Stream,GeneBank_Key,N_BP,Output_Stream) :-
        member(list('none'),Options),
        !,
        member(range([Min,Max]),Options),
        ((Min = min, Max = max)->
            Range = [1,N_BP]
        ;
            Range = [Min,Max]
        ),
        compute_data(FNA_Stream,GeneBank_Key,Range,N_BP,Output_Stream).

% List
parser_fna_data(Options,FNA_Stream,GeneBank_Key,N_BP,Output_Stream) :-
        member(list(Size),Options),
        Size \= 'none',
        member(range([Min,Max]),Options),
        ((Min = min, Max = max)->
            Range = [1,N_BP]
        ;
            Range = [Min,Max]
        ),
        compute_data(FNA_Stream,GeneBank_Key,Range,Size,Output_Stream).



% compute_data(GenBank_Key,Range,Size,Stream).


compute_data(In_Stream,GenBank_Key,Range,Size,Out_Stream) :-
        get_code(In_Stream,Code),
        var(L),
        compute_data_rec(In_Stream,Code,GenBank_Key,Range,Size,1,1,_Left,L-L,Out_Stream).


% EOF

compute_data_rec(_In_Stream,-1,GeneBank_Key,_Range,_Size,Position,_Size_Num,Left,Data-[],Out_Stream) :-
        !,
        Right is Position-1,
        Data = data(GeneBank_Key,Left,Right,Data),
        writeq(Out_Stream,Data),write(Out_Stream,'.'),nl(Out_Stream).
       


% Case Before Range

compute_data_rec(In_Stream,Code,GenBank_Key,[Min,Max],Size,Position,Size_Num,Left,Data1-Data2,Out_Stream) :-
           nucleotid_letter(Code),
           Position < Min,
           !,
           get_code(In_Stream,New_Code),
           Position1 is Position+1,
           compute_data_rec(In_Stream,New_Code,GenBank_Key,[Min,Max],Size,Position1,Size_Num,Left,Data1-Data2,Out_Stream).

% Case After Range

compute_data_rec(_In_Stream,_Code,GeneBank_Key,[_Min,Max],_Size,Position,_Size_Num,Left,Data-[],Out_Stream) :-
           Position > Max,
           !,
           (Data = [] ->
               true
           ;
               Right is Position-1,
               Term = data(GeneBank_Key,Left,Right,Data)
           ),
           writeq(Out_Stream,Term),write(Out_Stream,'.'),nl(Out_Stream).


% Case in Range
% End of a sub_terms
compute_data_rec(In_Stream,Code,GeneBank_Key,[Min,Max],Size,Position,Size_Num,Left,Data-Data2,Out_Stream) :-
           nucleotid_letter(Code),
           Position >= Min,
           Position =< Max,
           Size = Size_Num,
           !,
           (Position = Min ->
               Left = Min
           ;
               true
           ),
           Minuscule_Code is Code +32,
           atom_codes(Nuc,[Minuscule_Code]),
           Data2 = [Nuc],
           Term = data(GeneBank_Key,Left,Position,Data),
           writeq(Out_Stream,Term),write(Out_Stream,'.'),nl(Out_Stream),
           get_code(In_Stream,New_Code),
           Position1 is Position+1,
           var(L),
           compute_data_rec(In_Stream,New_Code,GeneBank_Key,[Min,Max],Size,Position1,1,Position1,L-L,Out_Stream).

% Inside a sub_terms
compute_data_rec(In_Stream,Code,GeneBank_Key,[Min,Max],Size,Position,Size_Num,Left,Data1-Data2,Out_Stream) :-
           nucleotid_letter(Code),
           Position >= Min,
           Position =< Max,
           Size_Num <Size,
           !,
              (Position = Min ->
               Left = Min
           ;
               true
           ),
           Size_Num1 is Size_Num+1,
           get_code(In_Stream,New_Code),
           Position1 is Position+1,
           Minuscule_Code is Code +32,
           atom_codes(Nuc,[Minuscule_Code]),
           Data2 = [Nuc|Data3],
           compute_data_rec(In_Stream,New_Code,GeneBank_Key,[Min,Max],Size,Position1,Size_Num1,Left,Data1-Data3,Out_Stream).

        
%% Non interesting character
compute_data_rec(In_Stream,_Code,GenBank_Key,Range,Size,Positon,Size_Num,Left,Data1-Data2,Out_Stream) :-
        get_code(In_Stream,New_Code),
        compute_data_rec(In_Stream,New_Code,GenBank_Key,Range,Size,Positon,Size_Num,Left,Data1-Data2,Out_Stream).



%%%list_data_rec(_Size,_FNA_Stream,_Genbank_Key,N_BP,Position,_Output_Stream) :-
%%%        N_BP =< Position,
%%%        !.

%%%list_data_rec(Size,FNA_Stream,Genbank_Key,N_BP,Position,Output_Stream) :-
%%%        compute_data(FNA_Stream,Size,List_Nuc),
%%%        length(List_Nuc,Length),
%%%        Position_End is Position+Length-1,
%%%        Data = data(Genbank_Key,Position,Position_End,List_Nuc),
%%%        write(Output_Stream,Data),
%%%        write(Output_Stream,'.'),
%%%        nl(Output_Stream),
%%%        Position2 is Position_End+1,
%%%        list_data_rec(Size,FNA_Stream,Genbank_Key,N_BP,Position2,Output_Stream).



%%%% compute_data(++FNA_Stream,++Range,--[Left,Right],++Size,--Data)

%%%compute_data(Stream,++RanSize,Data) :-
%%%        get_code(Stream,Code),
%%%        compute_data_rec(Stream,Code,1,Size,Data).

%%%% EOF
%%%compute_data_rec(_Stream,-1,_Position,_Size,[]) :-
%%%        !.

%%%% Last Element of the list
%%%compute_data_rec(_Stream,Code,Size,Size,[Nuc]) :-
%%%        nucleotid_letter(Code),
%%%        !,
%%%        Minuscule_Code is Code +32,
%%%        atom_codes(Nuc,[Minuscule_Code]).


%%%compute_data_rec(Stream,Code,Position,Size,[Nuc|Rest]) :-
%%%        nucleotid_letter(Code),
%%%        !,
%%%        Minuscule_Code is Code +32,
%%%        atom_codes(Nuc,[Minuscule_Code]),
%%%        Position1 is Position+1,
%%%        get_code(Stream,New_Code),
%%%        compute_data_rec(Stream,New_Code,Position1,Size,Rest).

%%%% Non interesting character
%%%compute_data_rec(Stream,_Code,Position,Size,Data) :-
%%%        get_code(Stream,New_Code),
%%%        compute_data_rec(Stream,New_Code,Position,Size,Data).


% get_info_gbk

get_info_gbk(GBK_Stream,Genbank_Key,N_BP) :-
        readline(GBK_Stream,List_Codes),
        parser_line(List_Codes,[9,32],[_Locus,Genbank,N_BP|_Rest]),
        atom_codes(Genbank,L),
        append([39|L],[39],L2),
	atom_codes(Genbank_Key,L2).



%%%%
% Utils
%%%%

nucleotid_letter(Code) :-
        member(Code,[65,67,71,84]),
        !.
       






%%%% Generation of a list of nucleotides given a position and a length

%%%listdata_generation(Stream,Start_Position,Length,Result) :-
%%%        get_code(Stream,Caractere),
%%%        listdata_generation_rec(Caractere,Stream,1,Start_Position,1,Length,Result).


%%%% end of Stream
%%%listdata_generation_rec(-1,_Stream,_Num_Start,_Start_Position,_Num_Length,_Length,[]) :-
%%%        !.

%%%listdata_generation_rec(_Caractere,_Stream,_Num_Start,_Start_Position,Num_Length,Length,[]) :-
%%%        Num_Length > Length,
%%%        !.

%%%listdata_generation_rec(Caractere_Ignore,Stream,Num_Pos,Start_Position,Num_Length,Length,Result) :-
%%%        lists:nonmember(Caractere_Ignore,[65,67,71,84]),  % Warning not enought sufficient to remove wrong caracter
%%%        !,
%%%        get_code(Stream,Caractere),
%%%        listdata_generation_rec(Caractere,Stream,Num_Pos,Start_Position,Num_Length,Length,Result).


        
%%%listdata_generation_rec(_Caractere,Stream,Num_Pos,Start_Position,Num_Length,Length,Result) :-
%%%        Num_Pos < Start_Position,
%%%        !,
%%%        get_code(Stream,Caractere_New),
%%%        Num_Pos1 is Num_Pos+1,
%%%        listdata_generation_rec(Caractere_New,Stream,Num_Pos1,Start_Position,Num_Length,Length,Result).



%%%listdata_generation_rec(Caractere,Stream,Num_Pos,Start_Position,Num_Length,Length,Result) :-
%%%        Num_Pos >= Start_Position,
%%%        !,
%%%        Num_Pos1 is Num_Pos+1,
%%%        Caractere2 is Caractere+32, % Translation Majuscule => Minuscule
%%%        atom_codes(Nucleotid,[Caractere2]),
%%%        Result = [Nucleotid|Result2],
%%%        get_code(Stream,Caractere_New),
%%%        Num_Length1 is Num_Length+1,
%%%        listdata_generation_rec(Caractere_New,Stream,Num_Pos1,Start_Position,Num_Length1,Length,Result2).

