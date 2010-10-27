%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit testing for LoSt framework 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lost_include_api(interface).
:- lost_include_api(misc_utils).

testtest :-
	run_all_tests(Res),
	report_test_results_text(Res).

%% run_tests/0
% Runs all test cases specified in files in the testcase directory
run_all_tests(SortedResults) :-
	testcase_files(TestCaseFiles),
	findall(Results,(member(File,TestCaseFiles),run_testcases_in_file(File,Results)),AllResults),
	flatten(AllResults,FlatResults),
	sort(FlatResults,SortedResults).

%% report_test_results_text(--Results)
% Reports the results of running a test-suite as simple text
% with one line per testcase result
report_test_results_text([]).
report_test_results_text([R|Rs]) :-
	R =.. [Status,TestCaseName,Details],
	write(TestCaseName),
	write('\t\t'),
	write(Status),
	write('\t'),
	write(Details),nl,
	report_test_results_test(Rs).

%% testcase_files(--TestCaseFiles)
% Produce a list of files containing testcases
testcase_files(TestCaseFiles) :-
	lost_testcase_directory(TestCaseDir),
	testcase_files_rec([TestCaseDir],TestCaseFiles).
	
%% testxase_files_rec(++Directories,--TestCaseFiles)
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

%%  run_testcases_in_file(++Filename,--TestCaseResults)
% Runs all testcases in a particular testcase file.
run_testcases_in_file(Filename,TestCaseResults) :-
	nl, write('===== Runnning test suite in file: '), write(Filename), write(' ====='),nl,
	terms_from_file(Filename,FilePredicates),
	findall(TC,(member(FP,FilePredicates),FP=..[:-,TC,_],functor(TC,testcase,1)),TCPreds),
	run_testcase_rules(Filename,TCPreds,TestCaseResults).

run_testcase_rules(_,[],[]).
run_testcase_rules(TCFile,[TC|TCRest],[Status|StatusRest]) :-
	write(run_single_testcase_process(TCFile,TC,Status)),nl,
	run_testcase_rules(TCFile,TCRest,StatusRest).
	
run_single_testcase(TestCase,Status) :-
	(catch(call(TestCase),Exception,Status=failure(TestCase,Exception)) ->
		Status=success(TestCase)
		;
		Status=failure(TestCase,fail)).

run_single_testcase_process(TCFile,TC,Status) :-
	lost_base_directory(LostBaseDir),
	lost_tmp_directory(TmpDir),
	atom_concat(TmpDir, '/unittest_report.pl',ReportFile),
	write(TC),nl,
%	atom_concat_list(['(catch(call(', TestCase, '),Exception,Status=failure(TestCase,Exception))->',
%					 'Status=success(', TestCase, ') ; ',
%					 'Status=failure(', TestCase, ', fail)), ',
%					 'tell(ReportFile),',
%					 'portray_clause(Status),',
%					 'told.'],Goal),
	atom_concat_list([
	                    '[\'',LostBaseDir,'/lost.pl','\'],',
	                    'lost_include_api(unittest),',
					    'cl(\'',TCFile,'\'),',
					    'run_single_testcase(',TestCase,',Status),',
					    'tell(ReportFile),',
					    'portray_clause(Status),',
					    'told.'
					 ],Goal),
	launch_prism_process(TCFile,Goal),
	open(ReportFile,read,InStream),
	read(InStream,Status),
	close(InStream).
	
	
invoke_testcase(should_fail(TC),Status) :-
	(catch(call(TestCase),Exception,Status=failure(TestCase,Exception)) ->
		Status=success(TestCase)
		;
		Status=failure(TestCase,fail)).
	not(should_fail(TC)).
	
invoke_testcase
	
call_should_succeed(Goal) :- call(Goal).
	
	
call_should_succeed(TC) :-
	
	