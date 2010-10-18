:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(autoAnnotations).
:- lost_include_api(misc_utils).
:- lost_include_api(io).
:- lost_include_api(viterbi_learn).



lost_input_formats(annotate, [text(prolog(ranges(gene))),text(prolog(ranges(gene))),text(prolog(prism_switches))]).
lost_output_format(annotate, _Options, text(prolog(ranges(gene)))).

lost_option(annotate,direction,'+','+ for forward strand and - for reverse strand').
lost_option_values(annotate,direction,['+','-']).


train_direct(Trainfile,Paramsfile):-
	prismAnnot('consorf_genefinder_direct',direct),
	viterbi_learn_file(Trainfile),
	save_sw(Paramsfile).

train_reverse(Trainfile,Paramsfile):-
	prismAnnot('consorf_genefinder_reverse',direct),
	viterbi_learn_file(Trainfile),
	save_sw(Paramsfile).

/**************************************************************************************/
% Recusive call of the prediction routine
%
% requ9ires cons-, orf-annotations and params to exist beforehand 
% options include : direction Dir, that desides which version of the repdicter is to be used. (consorf_genefinder_direct.psm or consorf_genefinder_reverse.psm)

/* commented for testing in script consorf
sequential_consorf_beta(Input_Orf_File,Input_Cons_File,ParameterFile,Options,Prediction_File_prefix):-	
	atom_concat(Prediction_File_prefix,'orf_in',Orf_file_name),
	split_file(Input_Orf_File,1000,Orf_file_name,'.seq',Orf_In),
	atom_concat(Prediction_File_prefix,'cons_in',Cons_file_name),
	split_file(Input_Cons_File,1000,Cons_file_name,'.seq',Cons_In),
	sequential_consorf_beta_rec(1,Orf_In,Cons_In,ParameterFile,Options,Prediction_File_prefix).
	
sequential_consorf_beta_rec(_,[],_,_,_,_):-!.
sequential_consorf_beta_rec(_,_,[],_,_,_):-!.

sequential_consorf_beta_rec(N,[Input_Orf_File|OrfFiles],[Input_Cons_File|ConsFiles],ParameterFile,Options,Prediction_File_prefix):-
	term2atom(N,Atom_N),
	atom_concat(Prediction_File_prefix,'pred_out_',Prediction_File_Name),
	atom_concat(Prediction_File_Name,Atom_N,Prediction_File),
	annotate([Input_Orf_File,Input_Cons_File,ParameterFile], 						
			    Options,   
			    Prediction_File),!,
	%initialize_table,
	table_remove(_), 			
	write('Resulting consorf prediction file'),nl,
	writeln(Prediction_File),
	M is N+1,
	sequential_consorf_beta_rec(M,OrfFiles,ConsFiles,ParameterFile,Options,Prediction_File_prefix),!
	.

*/

consorf_beta(Dir,OrfFile,ConsFile,Param_File,AnnotFile):-
	annotate([OrfFile,ConsFile,Param_File],[direction(Dir)],AnnotFile),	
	write('Resulting consorf prediction file'),nl,
	writeln(AnnotFile).


% This is what is used to get the best annotation
annotate([OrfFile,ConsFile,ParamFile],Options,OutputFile) :-
	write('LoSt consorf genefinder: '),nl,
	get_option(Options,direction,Dir),
	(
	Dir = '+' ->
			  prismAnnot('consorf_genefinder_direct') % Load the actual PRISM model
			  ;
			  prismAnnot('consorf_genefinder_reverse')
	),
	restore_sw(ParamFile), % Restore switch values,
			  
			  open(OrfFile, read, OrfIn,[alias(orfin)]),
			  open(ConsFile, read, ConsIn,[alias(consin)]),
			  open(OutputFile,write,OutStream,[alias(predout)]),
			  consorf_main(OrfIn,ConsIn,OutStream),!,
			  close(orfin),
			  close(consin),
        close(OutStream),
				write('LoSt consorf genefinder terminated successfully.'),nl.

/**************************************************************************************/
% Recusive call of the prediction routine
%
consorf_main(OrfInStream,ConsInStream,OutStream):-
		% writeq('read(orfin,OrfTerm)'),nl,
		initialize_table,
		read(OrfInStream,OrfTerm),
		read(ConsInStream,ConsTerm),
		(OrfTerm \= end_of_file, ConsTerm \= end_of_file ->
                    OrfTerm =.. [_,Id,Start,Stop,Dir,Frm,[seq_annotation(InputOrf)|_]],
                    ConsTerm =.. [_,Id,Start,Stop,Dir,Frm,[cons_annotation(Cons_Annot)|_]],
                   ( Cons_Annot = [] ->
                        L is Stop - Start + 1, 
                        makelist(L,0,InputCons)
                    ;
                        InputCons = Cons_Annot
                    ),
      % Derive an annotation somehow
                    check_or_fail(viterbiAnnot(consorf(InputOrf,InputCons,OutputAnnotation),_),
                    error(errorpair(InputOrf,InputCons))),
			!,
                    OutputEntry =.. [consorf_prediction,Id,Start,Stop,Dir,Frm,OutputAnnotation],
                    writeq(OutStream,OutputEntry),writeln(OutStream,'.'),
                    consorf_main(OrfInStream,ConsInStream,OutStream),! % Green cut, tail-recursion optimization
		;
                    true
		).



