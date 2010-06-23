:- ['../../lot.pl'].
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).

annotate([GenbankFile,PredictionsFile],Options,OutputFile) :-
        init_model(PredictionsFile,GenbankFile),
        list_from_prediction_file(PredictionsFile,PredictionFrames),
        viterbit(model(PredictionFrames),Tree),
        flatten(Tree,TF),findall(S,member(msw(emit(S),_),TF),AllStats).
        % Mangler at sammenholde med PredictionsFile
        
switch_to_statename(msw(A
init_model(PredictionsFile,DataFile) :-
        % pre-fix delete prob to zero to learn
        % transition probs only
        fix_sw(goto_delete, [0,1]),
        list_from_genbank_file(DataFile,GenbankList),
        learn(model(GenbankList)),
        unfix_sw(goto_delete),

        list_from_prediction_file(PredictionsFile,PredList),
        length(GenbankList,NumGenes),
        length(PredList,NumPredictions),
        UnDeleteProb is NumGenes / NumPredictions,
        DeleteProb is 1 - UnDeleteProb,
        fix_sw(goto_delete, [DeleteProb,UnDeleteProb]).

list_from_genbank_file(File,List) :-
        open(File,read,Stream),
        read(Stream,model(List)),
        close(Stream).

list_from_prediction_file(File,List) :-
        open(File,read,Stream),
        list_from_stream(Stream,List),
        close(Stream).
        
list_from_stream(Stream,List) :-
        read(Stream,Term),
        ((Term == end_of_file) ->
                List = []
                ;
                functor(Term,F,_),
                ((F=='genemark_gene_prediction') ->
                    Term =.. [genemark_gene_prediction,_start,_end,Strand,Frame,_],
                    ((Strand == '+') ->
                        Frame6 = Frame
                        ;
                        Frame6 is 3 + Frame),
                    List = [Frame6|ListRest]
                    ;
                    List = ListRest),
                !,
                list_from_stream(Stream,ListRest)).

