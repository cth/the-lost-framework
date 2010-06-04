% TODO comment + headline of the file


orf_annotation(Input_File,Output_File) :-
        write('test'),nl,
        terms_from_file(Input_File,Terms),
        write('start annotation'),nl,
        term2annotation(Terms,Terms_Annotation),
        write('annotation computed'),nl,
        terms_to_file(Output_File,Terms_Annotation).


term2annotation([],[]) :-
        !.


term2annotation([Chunk|Rest_Chunks],Result) :-
        !,
        Chunk = chunk(Id,Left,Right,Dir,Frame,Extras_Infos),
        member(starts(Starts),Extras_Infos),
        member(stop(Stop),Extras_Infos),
        (Stop = [] ->
            Result = Rest_Annotations    % TODO something
        ;
            gen_annotation(Dir,Left,Right,Starts,Seq_Annotation),
            Annotation = orf_annotation(Id,Left,Right,Dir,Frame,[seq_annotation(Seq_Annotation)]),
            Result = [Annotation|Rest_Annotations]
        ),
        term2annotation(Rest_Chunks,Rest_Annotations).



% Direct Strand
gen_annotation('+',Left,Right,[First_Start|Rest_Starts],Annotation) :-
        Length is First_Start - Left,
        makelist(Length,'.',Annot),
        append(Annot,Rest_Annot,Annotation),
        gen_annotation_rec('+',Left,Right,First_Start,Rest_Starts,Rest_Annot).     

% Reverse strand
gen_annotation('-',Left,Right,[Start],Annotation) :-
        !,
        Length is Start-Left-5,
        makelist(Length,'-',Rest_Annot),
        append(['>','>','>'|Rest_Annot],['<','<','<'],Annot2),
        Length2 is Right-Start,
        makelist(Length2,'.',Annot3),
        append(Annot2,Annot3,Annotation).
        

gen_annotation('-',Left,Right,Starts,Annotation) :-
        length(Starts,N),
        N>1,
        !,
        sort(Starts,[First_Start|Rest_Starts]),
        Length is First_Start-Left-5,
        makelist(Length,'-',Annot),
        append(['>','>','>'|Annot],['<','<','<'],Annot2),
        append(Annot2,Rest_Annot,Annotation),
        gen_annotation_rec('-',Left,Right,First_Start,Rest_Starts,Rest_Annot).     



% +
gen_annotation_rec('+',_Left,Right,First_Start,[],Annotation) :-
        !,
        Length is Right-First_Start-5,
        makelist(Length,'-',Annot),
        append(['<','<','<'|Annot],['>','>','>'],Annotation).


gen_annotation_rec('+',Left,Right,Start,[New_Start|Rest_Starts],Annotation) :-
        !,
        Length is New_Start-Start-3,
        makelist(Length,'-',Annot),
        append(['<','<','<'|Annot],Rest_Annot,Annotation),
        gen_annotation_rec('+',Left,Right,New_Start,Rest_Starts,Rest_Annot).



%-
%%%gen_annotation_rec('-',Left,Right,Start,[],Annotation) :-
%%%        !,
%%%        Length is Start-Left-6,
%%%        makelist(Length,'-',Annot),
%%%        append(Annot,['>','>','>'],Annot2),
%%%        Length2 is Right - Start,
%%%        makelist(Length2,'.',Annot3),
%%%        append(Annot2,Annot3,Annotation).


gen_annotation_rec('-',_Left,Right,Start,[First_Start],Annotation) :-
        !,
        Length is First_Start-Start-3,
        makelist(Length,'-',Annot),
        append(Annot,['<','<','<'],Annot2),
        Length2 is Right - First_Start,
        makelist(Length2,'.',Annot3),
        append(Annot2,Annot3,Annotation).

gen_annotation_rec('-',Left,Right,Start,[First_Start|Rest_Starts],Annotation) :-
        Rest_Starts \= [],
        !,
        Length is First_Start-Start-3,
        makelist(Length,'-',Annot),
        append(Annot,['<','<','<'],Annot2),
        append(Annot2,Rest_Annot,Annotation),
        gen_annotation_rec('-',Left,Right,First_Start,Rest_Starts,Rest_Annot).


        





makelist(0,_,[]).

makelist(N,X,[X|Rest]):-
	N > 0,
	M is N-1,
	makelist(M,X,Rest).







