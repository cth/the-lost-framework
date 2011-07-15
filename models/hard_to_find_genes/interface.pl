:- ['../../lost.pl'].

:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(accuracy).
:- lost_include_api(io).

lost_option(annotate,start,min, 'An positive integer indicating the start of the range.').
lost_option(annotate,end,max, 'A positive integer indicating the end of the range.').
lost_option(annotate,gene_match_criteria,start_and_stop,'Whether to match both \'start_and_stop\' or just \'stop\' codons').
lost_option(annotate,sort,default,'sort-order (either default=position or difficulty)').

lost_option_values(annotate,gene_match_criteria,[start_and_stop,stop]).

lost_input_formats(annotate,[text(prolog(ranges(gene))),star(text(prolog(ranges(gene))))]).
lost_output_format(annotate,_options,text(prolog(ranges(gene)))).

load_prediction_files([],[]).
load_prediction_files([File|Files],[Functor|Functors]) :-
%	write(load_prediction_files([File|Files],[Functor|Functors])),nl,
	(catch(current_file_id(Id),_,Id=0)),
	!,
	NextId is Id + 1,
	asserta(current_file_id(NextId)),
	terms_from_file(File,Terms),
	atom_integer(NextIdAtom,NextId),
	atom_concat('pred_', NextIdAtom,Functor),
%	write(Functor),nl,
	assert_prediction_terms(Functor,Terms),
	load_prediction_files(Files,Functors).
	
assert_prediction_terms(_Functor, []).
assert_prediction_terms(Functor, [T|Ts]) :-
		T =.. [ _, _, Left, Right, Strand, Frame, _ ],
		NewT =.. [ Functor, na, Left, Right, Strand, Frame, []],
		assert(NewT),
		assert_prediction_terms(Functor,Ts).

annotate([GoldenStandardFile|PredictionsFiles], Options, OutFile) :-
	load_prediction_files(PredictionsFiles,PredFunctors),
	file_functor(GoldenStandardFile,RefFunctor),
	consult(GoldenStandardFile),
%    write('prediction functors: '),nl, 
%    write(PredFunctors),nl,
	get_option(Options,start,StartOptionValue),
    ((StartOptionValue == min) ->
        Start = 1
        ;
        Start = StartOptionValue),

        get_option(Options,end,EndOptionValue),
        ((EndOptionValue == max) ->
            db_annotation_max(RefFunctor,_,_,End)
            ;
            End = EndOptionValue),
	get_option(Options,gene_match_criteria,MatchCriteria),
    % Produce hard 2 find list
	gene_finding_difficulty_report(RefFunctor,PredFunctors,Start,End,MatchCriteria,DiffGeneList),
    % save file
	get_option(Options,sort,SortOrder),
	((SortOrder = difficulty) ->
		map(pref_diff1, DiffGeneList, DiffIndexed),
		sort(DiffIndexed,DiffSorted),
		reverse(DiffSorted,DiffSortedRev),
		map(pref_diff2,DiffSortedRev,FinalList)
		;
		FinalList = DiffGeneList
	),
	terms_to_file(OutFile,FinalList).
	
pref_diff1(P,[D,P]) :-
	write(P),nl,
	P =.. [ _func, _id, _left, _right, _strand, _frame, Extra ],
	member(gene_finding_difficulty_score(D),Extra).
	
pref_diff2([_D,P],P).