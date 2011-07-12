:- ['../lost.pl'].

display_file(F) :-
        atom_concat('more ', F, Cmd),
        system(Cmd).





train_blastgf_multi_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/blastgf_multi_track_predict_training_orfs.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

train_blastgf_single_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/blastgf_single_track_predict_training_orfs.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).
        
train_codon_pref_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/cod_pref_trainSet_predictions.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

train_combiner_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/combiner_predict_training_orfs.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

train_voting_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/cp_cn_voting_predict_train.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

train_combiner_length_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/combiner_length_predictions_trainset.pl',
        REF = '/tmp/ECML_DATA/refseq_train.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).
    





test_blastgf_multi_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/blastgf_multi_track_predict_test_orfs.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

test_blastgf_single_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/blastgf_single_track_predict_test_orfs.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).
        
test_codon_pref_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/cod_pref_testSet_predictions.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

test_combiner_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/combiner_predict_test_orfs.pl',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)),
        display_file(Acc).

test_voting_acc(Acc) :-
        PRED = '/tmp/ECML_DATA/not_there_yet',
        REF = '/tmp/ECML_DATA/refseq_test.pl',
        run_model(accuracy_report,annotate([REF,PRED],[],Acc)).
  
