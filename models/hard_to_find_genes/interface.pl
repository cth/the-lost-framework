:- ['../../lost.pl'].

:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(accuracy).

lost_best_annotation([GoldenStandardFile|PredictionsFiles], Options, OutFile) :-
	forall(member(File,[GoldenStandardFile|PredictionsFiles]),consult(File)),
	file_functor(Options,GoldenStandardFile,RefFunctor),
	map(file_functor(Options,input,output),PredictionsFiles,PredFunctors),
	(((lost_option(Options,start,Start)),lost_option(Options,end,End)) ->
	 true ; Start=1, db_annotation_max(RefFunctor,_,_,End)),
	gene_finding_difficulty_report(RefFunctor,PredFunctors,Start,End,DiffGeneList),
	terms_to_file(OutFile,DiffGeneList).

file_functor(Options,File,Functor) :-
	lost_required_option(Options,file_functor(File),Functor).
