% This script is for the toplogy of a the consorf genefinder relying on annotations from
% open reading frame annotator and conservation annotator.
% The consorf genefinder is based on a paired hidden markov model that should be properly parameterized.
% While the genefinder itself does not at this point care about what chunks it predicts, the inout files must
% reflect the same chunking of data. As usual, we take care of that by including the toplogy of prediction models in this script.

:- ['../lost.pl'].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).
:- lost_include_api(utils_parser_report).
:- lost_include_api(prism_parallel).
:- [script_parser].
:- [filter_genes].

% topology 

%--------------------------------------------------------------------------------------------------
% First the chunking. 
%--------------------------------------------------------------------------------------------------
% 	Here required options are:
%			frame(reading-frame) and 
%			direction(direction)?
%		
% 	OrfChunk_File consist of facts:
%			chunk(Id, Start, Stop, ChunkSequence, Dir Frame, List_of_potenital_start_codons, List_of_potential_stop_codons).
%--------------------------------------------------------------------------------------------------
run_orf_chopper(Sequence_File,Options,OrfChunk_File) :-
	lost_sequence_file(Sequence_File,Sequence),
        run_model(orf_chopper,annotate([Sequence],Options,OrfChunk_File)).
			    
%--------------------------------------------------------------------------------------------------
% Translation to amino-acids. 
%--------------------------------------------------------------------------------------------------
% 	Required options ase:
%			mode(translation_mode)
%		
%		where translation_mode = 0 means entire chunk is translated, 
%		and translation_mode = 1 means only longest orf is translated
%
%   Translated_Chunk_File consist of translated chunks in fastaformat:
%			....explanation
%		
%--------------------------------------------------------------------------------------------------
run_chunk_translator(Sequence_File,Options,Translated_Chunk_File) :-
	run_orf_chopper(Sequence_File,Options,Orf_Chunk_File),
        run_model(chunk_translator,annotate([Orf_Chunk_File],Options,Translated_Chunk_File)).

%--------------------------------------------------------------------------------------------------
% Computing conservation annotation
%--------------------------------------------------------------------------------------------------
% 	Possible options include scoring mode for non-gap mismatches : nmScore(Score)
%			Score = 0, nongap-mismatches score nothing, 
%			Score = 1, nongap-mismathces score fully
%
%		and an optional : alignment outputfile : alignments(Alnfile)
% 	also direction and frame are needed
%--------------------------------------------------------------------------------------------------
run_chunk_conservation(Sequence_File,Options,Conservation_File) :-
	run_chunk_translator(Sequence_File,Options,Translated_Chunk_File),
        run_model(chunk_aa_conservation,annotate([Translated_Chunk_File], Options, Conservation_File)).

% testgoal:
% run_chunk_conservation('U00096',[direction(+),frame(1),mode(0),genecode(11)],O)


%-------------
% Run orf annotator
%-----------
% From the sequence file, generation of the orf annotation from the result
% of the orf chopper:
%     orf_annotation(Key,Left,Right,Dir,Frame,[seq_annot([Annot])]
% where Annot = [.,.,.,.,.,<,<,<,-,-,-,-,>,>,>]
%-----------
run_orf_annotator(Sequence_File,Options,Orf_annot_File) :-
        run_orf_chopper(Sequence_File,Options,Chunk_File),
       writeln('ok'), 
	run_model(orf_annotator,annotate([Chunk_File],[],Orf_annot_File)).

%-------------
% Run Genebank Annotator
%-----------
% From the sequence file, generation of an
% annotation from Genebank information
%     genebank_annotation(Key,Left,Right,Dir,Frame,[genbank_annotation(Annot)]
%-----------
% Precond: requires options
run_genebank_annotator(Sequence_File,File_PTT,Options,GeneBank_annot_File) :-
        ((member(direction(Dir),Options),member(frame(Frame),Options)) ->
            Options_Chopper = [direction(Dir),frame(Frame)],
            Options_Filter = [match_strands([Dir]),match_frames([Frame])]
        ;
            write(" Options missing"),nl
        ),
        run_orf_chopper(Sequence_File,Options_Chopper,Chunk_File),
        lost_sequence_file(File_PTT,Seq_File_PTT),
        parser_ptt(Seq_File_PTT,PTT_Parsed),
        filter_genes(PTT_Parsed,Sequence_File,Options_Filter,GeneBank_Filtered),
        run_model(genebank_annotator, annotate([Chunk_File,GeneBank_Filtered], [], GeneBank_annot_File)).


% Example: run_genebank_annotator('U00096','U00096_ptt',[direction(+),frame(1)],O).



%--------------------------------------------------------------------------------------------------
% driving run-predicate for testing and debugging
%--------------------------------------------------------------------------------------------------
consorf(OrfFile,ConsFile,Param_File,AnnotFile):-
	annotate([OrfFile,ConsFile,Param_File],_Options,AnnotFile),	
	write('Resulting consorf prediction file'),nl,
	writeln(AnnotFile).
	

%--------------------------------------------------------------------------------------------------
% driving run-predicate once required models have been ported to LoSt framework
%--------------------------------------------------------------------------------------------------
run_consorf(Sequence_File,ParameterFile,Options,Prediction_File):-
  run_orf_annotator(Sequence_File,Options,Input_Orf_File),
	run_chunk_conservation(Sequence_File,Options,Input_Cons_File),
	%lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),
	run_model(consorf_genefinder,
			    annotate([Input_Orf_File,Input_Cons_File,ParameterFile], 						
			    [],   
			    Prediction_File)),     			
	write('Resulting consorf prediction file'),nl,
	writeln(Prediction_File).

%--------------------------------------------------------------------------------------------------
% driving run-predicate sequential version
%--------------------------------------------------------------------------------------------------
	
sequential_consorf(Sequence_File,ParameterFile,Options,Prediction_File_prefix):-	
	run_orf_annotator(Sequence_File,Options,Input_Orf_File),
	run_chunk_conservation(Sequence_File,Options,Input_Cons_File),
	split_file(Input_Orf_File,1000,orf_in,'seq',Orf_In),
	split_file(Input_Cons_File,1000,cons_in,'seq',Cons_In),
	sequential_consorf_rec(1,Orf_In,Cons_In,ParameterFile,Options,Prediction_File_prefix).
	
sequential_consorf_rec(_,[],_,_,_,_):-!.
sequential_consorf_rec(_,_,[],_,_,_):-!.

sequential_consorf_rec(N,[Input_Orf_File|OrfFiles],[Input_Cons_File|ConsFiles],ParameterFile,Options,Prediction_File_prefix):-
	atom_concat(Prediction_File_prefix,[N],Prediction_File),
	run_model(consorf_genefinder,
			    annotate([Input_Orf_File,Input_Cons_File,ParameterFile], 						
			    Options,   
			    Prediction_File)),     			
	write('Resulting consorf prediction file'),nl,
	writeln(Prediction_File),
	M is N+1,
	sequential_consorf_rec(M,[OrfFiles],[ConsFiles],ParameterFile,Options,Prediction_File_prefix)
	.

	




% test predicate

testgoal:-run_chunk_conservation('U00096',[direction(+),frame(1),mismatch_score(1),mode(0),genecode(11)],Output), write('Output :'),writeln(Output).				

% conservation computation on one file already slipped

run_conservation_split(Dir,Frame,Name) :-
	atom_concat(tblastn_,Name,BlastFileName),
	lost_tmp_directory(TmpDir),
	atom_concat(TmpDir,Name,FileName),
	run_model(chunk_aa_conservation,annotate([FileName],[direction(Dir),frame(Frame),blast_file_name(BlastFileName),mismatch_score(1)],_Conservation)).
	
	

sequential_conservation(File,Dir,Frame) :-
	 term2atom(Frame,Frameatom),
	 lost_sequence_file(File,Sequence),
	 run_model(
		orf_chopper,
		annotate([Sequence],	[direction(Dir), frame(Frame)], OrfChunk_File)
	 ),
	 run_model(
		chunk_translator,annotate([OrfChunk_File], [mode(0),genecode(11)], Orf_FASTA)),
	 atom_concat(Dir,Frameatom,DirFrame),
	 atom_concat(File,'_',File_),
	 atom_concat(File_,DirFrame,File_DirFrame),
	 atom_concat(split_fasta_,File_DirFrame, Splitfile_Name),    
	 split_file_fasta(Orf_FASTA,1000,Splitfile_Name,'seq',R),
	 atom_concat(tblastn_,DirFrame,BlastFileName),
	 sequential_conservation_rec(Dir,Frame,BlastFileName,R).

sequential_conservation_rec(_,_,_,[]).

sequential_conservation_rec(Dir,Frame,BlastFileName,[File|Rest]) :-
%       term2atom(Frame,Frameatom),
	 run_model(chunk_aa_conservation,annotate([File],[direction(Dir),frame(Frame),blast_file_name(BlastFileName),mismatch_score(1)],_Conservation_org_file)),
writeln(her),
%	 writeln(Conservation_org_file),
/*	 atom_concat(Dir,Frameatom,DirFrame),
	 atom_concat(DirFrame,Conservation_org_file,Conservation_new_file),	 
	 
	 lost_data_directory(Path),
	 atom_concat(Path,Conservation_org_file, Org_file),
	 atom_concat(Path,Conservation_new_file, New_file),
	 writeq(move_data_file(Org_file,New_file)),
	 move_data_file(Org_file,New_file),
*/
        sequential_conservation_rec(Dir,Frame,BlastFileName,Rest).



parallel_conservation :-
	lost_sequence_file('U00096',Sequence),
       run_model(orf_chopper,annotate([Sequence],[direction(+),frame(1)],OrfChunk_File)),
       run_model(chunk_translator,annotate([OrfChunk_File],[mode(0),genecode(11)],Orf_FASTA)),
       split_file_fasta(Orf_FASTA,500,split_fasta,'seq',InFileParts),
       map(out_file_name,InFileParts,OutFileParts),
       write(OutFileParts),nl,
       create_goals(InFileParts,OutFileParts,1,FilePartGoals),
       prism_parallel(FilePartGoals).


out_file_name(InFile,OutFile) :-
        atom_concat(InFile,'.processed',OutFile).


create_goals([],[],_Num,[]) :-
        !.

create_goals([FileIn|RestIn],[FileOut|RestOut],Num,[Goal|Rest_Goal]) :-
        Goal = (
                 
                 [consorf],
                 number_codes(Num,Code),
                 atom_codes(AtomNum,Code),
                 atom_concat_list(['tblastn',AtomNum],Blast_Name),
                 safe_run_model(chunk_aa_conservation,annotate([FileIn],[blast_file_name(Blast_Name),mismatch_score(1)],FileOut))
                 ),
        Num1 is Num+1,
        create_goals(RestIn,RestOut,Num1,Rest_Goal).


