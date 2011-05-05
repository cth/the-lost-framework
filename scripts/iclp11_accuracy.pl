:- ['../lost.pl'].

display_file(F) :-
        atom_concat('more ', F, Cmd),
        system(Cmd).

store_report_file(OrigFile,LostFile) :-
        atom_concat(OrigFile,'.accuracy',ReportFile),
        write(ReportFile),nl,
        load_clauses(LostFile,C),
        save_clauses(ReportFile,C).

train_codpref(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_predictions_trainset.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).

train_codpref_w_length(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_w_length_predictions_trainset.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).
        
train_codpref_w_blast(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_w_blast_predictions_trainset.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).

train_codpref_w_blast_and_length(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_w_blast_and_length_predictions_trainset.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).


test_codpref(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_predictions_testset.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).

test_codpref_w_length(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_w_length_predictions_testset.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).
        
test_codpref_w_blast(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_w_blast_predictions_testset.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).

test_codpref_w_blast_and_length(Acc) :-
        PRED = '/tmp/ECML_DATA/new_predictions/codpref_w_blast_and_length_predictions_testset.pl', 
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        store_report_file(PRED,Acc),
        display_file(Acc).


create_all :-
       train_codpref(_),
        train_codpref_w_length(_),
        train_codpref_w_blast(_),
        train_codpref_w_blast_and_length(_),
       test_codpref(_), 
       test_codpref_w_length(_),
       test_codpref_w_blast(_),
       test_codpref_w_blast_and_length(_).

