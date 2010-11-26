:- ['../lost.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For running the orf_chopper in all frames:
frame(1).
frame(2).
frame(3).

direction('-').
direction('+').

chop_all_framedir(SequenceFile,OutFile) :-
        findall([F,D],(frame(F),direction(D)),FrameDirList),
        chop_framedir_list(SequenceFile,FrameDirList,OutFiles),
        run_model(merge_files, annotate(OutFiles,[],OutFile)).

chop_framedir_list(_,[],[]).

chop_framedir_list(SequenceFile,[[Frame,Direction]|Rest],[OutFile1|FilesRest]) :-
	run_model('orf_chopper', annotate([SequenceFile], [frame(Frame),direction(Direction)],OutFile1)),
        chop_framedir_list(SequenceFile,Rest,FilesRest).

% Genome is expected to be in fasta format
chop_genome(GenomeFile) :-
	chop_all_framedir(GenomeFile,OutFile),
	write('all orfs in '), write(GenomeFile), write(' written to '), write(OutFile),nl.


test :-
	lost_sequence_file('U00096',File),
	write(File),nl.
	
