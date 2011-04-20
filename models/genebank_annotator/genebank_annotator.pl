% TODO comment + headline of the file


genebank_annotation(Chunk_File,Filtered_Genebank,Output_File) :-
       consult(Chunk_File), 
	chunk(Key,_,_,Dir,Frame,_),
        !, 
        findall([Min,Max],chunk(_Key,Min,Max,_Dir,_Frame,_),Ranges), 
        open(Output_File,write,Output_Stream),
        Ranges = [[Left,Right]|Rest_Ranges],
        var(Term),
	 % testing debug 
	 %get_data_from_file(Filtered_Genebank,[range(Left,Right),term(Term)],GB_Annotation),
        load_annotation_from_file(db,[range(Left,Right),range_position(2,3),term(Term)],Filtered_Genebank,GB_Annotation),
        Annot =.. [genebank_annotation,Key,Left,Right,Dir,Frame,[seq_annotation(GB_Annotation)]],
        writeq(Output_Stream,Annot),write(Output_Stream,'.'),nl(Output_Stream),
        genebank_annotation_rec(Rest_Ranges,Key,Dir,Frame,Term,Filtered_Genebank,Output_Stream).


% Recursive call
genebank_annotation_rec([],_Key,_Dir,_Frame,_Term,_Filtered_Genebank,Output_Stream) :-
        !,
        close(Output_Stream).


genebank_annotation_rec([[Left,Right]|Rest_Ranges],Key,Dir,Frame,Term,Filtered_Genebank,Output_Stream) :-
        load_annotation_from_file(db,[range(Left,Right),range_position(2,3),term(Term)],Filtered_Genebank,GB_Annotation),
	 % get_data_from_file(Filtered_Genebank,[range(Left,Right),term(Term)],GB_Annotation),

        Annot =.. [genebank_annotation,Key,Left,Right,Dir,Frame,[seq_annotation(GB_Annotation)]],
	 writeq(Output_Stream,Annot),write(Output_Stream,'.'),nl(Output_Stream),
        genebank_annotation_rec(Rest_Ranges,Key,Dir,Frame,Term,Filtered_Genebank,Output_Stream).

