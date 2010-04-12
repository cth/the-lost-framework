:- ['../../lost.pl'].

:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(accuracy).

lost_option(lost_best_annotation,start,min, 'An positive integer indicating the start of the range.').
lost_option(lost_best_annotation,end,max, 'A positive integer indicating the end of the range.').

lost_input_formats(lost_best_annotation,[text(prolog(ranges(gene))),star(text(prolog(ranges(gene))))]).
lost_output_format(lost_best_annotation,_options,[text(prolog(ranges(gen)))]).

lost_best_annotation([GoldenStandardFile|PredictionsFiles], Options, OutFile) :-
        % Consult all input files 
	forall(member(File,[GoldenStandardFile|PredictionsFiles]),consult(File)),
        % Find main functors for all input files:
	file_functor(Options,GoldenStandardFile,RefFunctor),
	map(file_functor(Options,input,output),PredictionsFiles,PredFunctors),
        % Parse options
	lost_option(Options,start,StartOptionValue),
        ((StartOptionValue == min) ->
                Start = 1
                ;
                Start = StartOptionValue),

        lost_option(Options,end,EndOptionValue),
        ((EndOptionValue == max) ->
                db_annotation_max(RefFunctor,_,_,End),
                ;
                End = EndOptionValue),
        % Produce hard 2 find list
	gene_finding_difficulty_report(RefFunctor,PredFunctors,Start,End,DiffGeneList),
        % save file
	terms_to_file(OutFile,DiffGeneList).

