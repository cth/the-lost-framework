:- use(path).

testcase(clean_file) :-
	File = '//file//somewhere',
	CleanFile = '/file/somewhere',
	clean_file(File,CleanFile).
	
testcase(dirname) :-
	dirname('/path/to/somefile','/path/to/').
