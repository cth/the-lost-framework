:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(accuracy).


lost_option(lost_best_annotation,start,min,'An positive integer indicating the start of the range.').
lost_option(lost_best_annotation,end,max,'A positive integer indicating the end of the range.').

% TODO: implement support text(prolog(accuracy_report) of this option 
lost_option(lost_best_annotation,output_format,text(flat(accuracy_report)),
	    'Indicates the output format the accuracy report. Options are text(flat(accuracy_report)) and text(prolog(accuracy_report))').

lost_input_formats(lost_best_annotation, [text(prolog(ranges(gene))), text(prolog(ranges(gene)))]).

% Get output format from option specification
lost_output_format(lost_best_annotation, Options, OutputFormat) :-
	get_option(Options,output_format,OutputFormat).

lost_best_annotation([ReferenceFile,PredictionFile],Options,OutputFile) :-
	file_functor(ReferenceFile,ReferenceFunctor),
	file_functor(PredictionFile,PredictionFunctor),
	
	get_option(Options,start,Start),
	get_option(Options,end,End),
	
	check_or_fail(consult(ReferenceFile),error(cannot_consult(ReferenceFile))),
	check_or_fail(consult(PredictionFile),error(cannot_consult(PredictionFile))),

	((Start == min) ->
	 db_annotation_min(ReferenceFunctor,_,_,ActualStart)
	; integer(Start) -> ActualStart = Start 
	; throw(error(bad_option_start(Start)))),

	((End == max) ->
	 db_annotation_max(ReferenceFunctor,_,_,ActualEnd)
	; integer(End) -> ActualEnd = End
	; throw(error(bad_option_end(End)))),

	write(accuracy_stats(ReferenceFunctor,PredictionFunctor,ActualStart,ActualEnd,OutputFile)),nl,
	accuracy_stats(ReferenceFunctor,PredictionFunctor,ActualStart,ActualEnd,OutputFile).
