:- module(unittest,[test_and_report/0,test_and_report/1]).
/** <module> unittest

This is a library for setting up unit tests for the Lost framework.

All tests are located in the test/ directory.

Prolog files in this directory may contain one or more testcases. 
A testcase is a predicate with the prefix =|testcase|=.

@author: Christian Theil Have

*/
:- use([interface, lists, terms, errorcheck]).


%% test_and_report
% Runs all testcases and writes a report to standard out
test_and_report :-
	writeln('================== RUNNING TESTS ======================'),
	run_all_tests(Res),
	writeln('=============== REPORTING RESULTS ======================'),
	report_file_results(Res).
	
%% test_and_report(+File)
% Runs all testcases and writes a report to File
test_and_report(File) :-
	writeln('================== RUNNING TESTS ======================'),
	run_all_tests(Res),
	write('=============== Writing results to file: '),
	writeln(File),
	tell(File),
	report_file_results(Res),
	told.
	
%% test_and_report_single_suite(+TestSuiteFile)
% Runs all testcases and writes a report to File
test_and_report_single_suite(TestSuiteFilePartial) :-
	writeln('================== RUNNING TESTS ======================'),
	expand_file_spec(TestSuiteFilePartial,TestSuiteFileFull),
	write('-->'),
	writeln(TestSuiteFileFull),
	run_testcases_in_file(TestSuiteFileFull,Results),
	writeln('=============== RESULTS: '),
	report_file_results([[TestSuiteFileFull,Results]]).
	
expand_file_spec(PartialFileName,FullFileName) :-
	% Check .pl file suffix,
	atom_codes(PartialFileName,PartialCodes),	
	(append(_,".pl",PartialCodes) ->
		PartialWithSuffix = PartialCodes
		;
		append(PartialCodes, ".pl", PartialWithSuffix)
	),
	writeln(PartialWithSuffix),
	testcase_files(TestCaseFiles),
	map(atom_codes,TestCaseFiles,TestCaseFileCodes),
	writeln(here),	
	member(OneFileCodes,TestCaseFileCodes),
	append(_Prefix,PartialWithSuffix,OneFileCodes),
	atom_codes(FullFileName,OneFileCodes).

%% run_all_tests
% Runs all test cases specified in files in the testcase directory
run_all_tests(AllResults) :-
	testcase_files(TestCaseFiles),
	findall([File,Results],(member(File,TestCaseFiles),run_testcases_in_file(File,Results)),AllResults).

%% report_test_results_text(+Results)
% Reports the results of running a test-suite as simple text with one line per testcase result
report_file_results([]).
report_file_results([[File,FileTestResults]|Rs]) :-
	lost_config(lost_base_directory,TestDir),
	clean_file(TestDir,CleanTestDir),
	clean_file(File,CleanFile),
	atom_codes(CleanFile,FileCodes),
	atom_codes(CleanTestDir,TestDirCodes),
	append(TestDirCodes,BaseNameCodes,FileCodes),
	atom_codes(BaseFile,BaseNameCodes),
	write(BaseFile),
	writeln(':'),
	sort(FileTestResults,SortedFileTestResults),
	report_test_results(SortedFileTestResults),
	report_file_results(Rs).

report_test_results([]).
report_test_results([Result|ResultsRest]) :-
	Result =.. [Status,testcase(TestCaseName),Details],
	write('\t'),
	write(TestCaseName),
	write('\t\t\t'),
	write(Status),
	write('\t\t'),
	write(Details),
	nl,
	report_test_results(ResultsRest).

%% testcase_files(-TestCaseFiles)
% Produce a list of files containing testcases
testcase_files(TestCaseFiles) :-
	lost_testcase_directory(TestCaseDir),
	testcase_files_rec([TestCaseDir],TestCaseFiles).
	
%% testcase_files_rec(+Directories,-TestCaseFiles)
% recursively scan Directories for files containing testcases
testcase_files_rec([D|Ds],TestCaseFiles) :-
	directory_files(D,AllDirEntries),
	subtract(AllDirEntries,['.','..'],RealDirEntries),
	% Which are directories
	findall(SubDir,(member(SubDir,RealDirEntries),atom_concat(D,SubDir,AbsSubDir),directory_exists(AbsSubDir)),SubDirs),
	findall(AbsDir,(member(D1,SubDirs),atom_concat(D,D1,AbsDir)),AbsDirs),
	write(AbsDirs),nl,
	subtract(RealDirEntries,SubDirs,PlainFiles),
	% Only consider Prolog files:
	findall(File,(member(File,PlainFiles),atom_concat(_prefix,'.pl',File)),PrologFiles),
	findall(AbsFile,(member(File1,PrologFiles),atom_concat(D,'/',D1), atom_concat(D1,File1,AbsFile)),AbsFiles),
	append(AbsDirs,Ds,ExploreDirs),
	testcase_files_rec(ExploreDirs,RestFiles),
	append(AbsFiles,RestFiles,TestCaseFiles).
	
testcase_files_rec([],[]).

%%  run_testcases_in_file(+Filename,-TestCaseResults)
% Runs all testcases in a particular testcase file.
run_testcases_in_file(Filename,TestCaseResults) :-
	nl, write('===== Runnning test suite in file: '), write(Filename), write(' ====='),nl,
	terms_from_file(Filename,FilePredicates),
	findall(TC,(member(FP,FilePredicates),FP=..[:-,TC,_],functor(TC,testcase,1)),TCPreds),
	run_testcase_rules(Filename,TCPreds,TestCaseResults).

run_testcase_rules(_,[],[]).
run_testcase_rules(TCFile,[TC|TCRest],[Status|StatusRest]) :-
	write(run_single_testcase_process(TCFile,TC,Status)),nl,
	run_single_testcase_process(TCFile,TC,Status),
	run_testcase_rules(TCFile,TCRest,StatusRest).
	
run_single_testcase(TestCase,Status) :-
	(catch(call(TestCase),Exception,Status=failure(TestCase,Exception)) ->
		(ground(Status) ; Status=success(TestCase,'n/a'))
		;
		Status=failure(TestCase,fail)).

run_single_testcase_process(TCFile,TC,Status) :-
	writeln('here'),
	lost_config(lost_base_directory,LostBaseDir),
	lost_tmp_directory(TmpDir),
	atom_concat(TmpDir, '/unittest_report.pl',ReportFile),
	delete_file(ReportFile),
	write(TC),nl,
	writeln(LostBaseDir),
	writeln(TCFile),
	term_to_atom(TC, TCAtom),
	atom_concat_list([
	                    '[\'',LostBaseDir,'/lost.pl','\'],',
						'lost_include_api(path),',
	                    'lost_include_api(unittest),',
					    '[\'',TCFile,'\'],',
						'write(test),',					
					    'run_single_testcase(',TCAtom,',Status),',
						'writeln(done),',
					    'tell(\'', ReportFile,'\'),',
					    'portray_clause(Status),',
					    'told.'
					 ],Goal),
	writeln(Goal),
	launch_prism_process(TCFile,Goal),
	open(ReportFile,read,InStream),
	read(InStream,Status),
	close(InStream).
	
invoke_testcase(should_fail(TC),Status) :-
	(catch(call(TestCase),Exception,Status=failure(TestCase,Exception)) ->
		Status=success(TestCase)
		;
		Status=failure(TestCase,fail)),
	not(should_fail(TC)).
	
call_should_succeed(Goal) :- call(Goal).
	
% Test unittesting


	
	
