:- use(annotation_index).
:- use(path).

% Create a annotation index with a few file
testcase(lost_file_index_get_filenames) :-
	lost_tmp_file('test-annotation-index',IndexFile),
	Model = 'testmodel',
	Goal = 'testgoal',
	InputFiles = [file1,file2,file3],
	Options = [opt1(check),opt2(no),opt3(yes)],
	Filenames = [F1,F2],
	writeln('index file: '),
	writeln(IndexFile),
	lost_file_index_get_filenames(IndexFile,Model,Goal,InputFiles,Options,Filenames),
	writeln('after first call'),
	ground(F1), ground(F2), % Filenames should now be ground
	% Second call with same parameters should just resolve to same filenames and succeed:
	lost_file_index_get_filenames(IndexFile,Model,Goal,InputFiles,Options,Filenames).
	

	
	



