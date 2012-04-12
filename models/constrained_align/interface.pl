:- task(align_edit_distance(text(prolog(ranges(_))),[min_stem_length(0),max_pairing_mismatch(10)],text(prolog(alignments)))).
:- task(align_pmcomp(text(prolog(ranges(_))),[min_stem_length(0),max_pairing_mismatch(10)],text(prolog(alignments)))).
:- task(align_clustalw(text(prolog(ranges(_))),[min_stem_length(0),max_pairing_mismatch(10)],text(prolog(alignments)))).
:- task(as_phylip_matrix(text(prolog(alignments)),[],text(phylip))).

:- use(lists).

% This needs to point to a B-Prolog installation.
% PRISM (with CHR) may be used in place of B-Prolog..
% ...but should be based on at least B-Prolog 7.7 to run this efficiently.
config(bprolog_with_chr,'/opt/BProlog/bp leuven_chr.out -g ').
		
	
%% align_edit_distance(+InputFile,+AlignmentsFile,+Constraints)
% Invoked in external B-Prolog
% Currently, does not terminate as it should!
align_edit_distance([InputFile],Options,AlignmentsFile) :-
	writeln(here),
	map(fact_assertion,[align_method(edit_align)|Options],AssertConstraints),
	map(term2atom,AssertConstraints,AtomAssertConstraints),
	intersperse(',',AtomAssertConstraints,AtomAssertConstraintsSeparated),
	config(bprolog_with_chr,PrologWithCHR),
	%lost_tmp_file('rna_alignments',AlignmentsFile),
	CmdListNested = [
		PrologWithCHR,
		' "',
		'cl(edit_dist), cl(constrained_align), ',
		AtomAssertConstraintsSeparated,
		',',
		'build_alignments(\'',InputFile,'\',\'', AlignmentsFile, '\'),',
		'halt.',
		'"'],
	flatten(CmdListNested,CmdList),
	atom_concat_list(CmdList,InvokeCommand),
	writeln('INVOKE COMMAND:'),
	writeln(InvokeCommand),
	system(InvokeCommand),
	check_or_fail(file_exists(AlignmentsFile), 'alignments file not produced!').
	% where
	fact_assertion(Fact,assert(Fact)).

%% align_pmcomp(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [InputFile]
% ==
% Align sequences in InputFile using pmcomp.pl
align_pmcomp([InputFile],Options,OutputFile) :-
	assert(align_method(pmcomp_align)),
	forall(member(Opt,Options),assert(Opt)),
	cl(pmcomp),
	cl(constrained_align),
	write('calling build alignments'),
	build_alignments(InputFile,OutputFile),
	check_or_fail(file_exists(OutputFile), 'alignments file not produced!').

%% align_clustalw(+InputFiles,+Options,+OutputFile)
% == 
% InputFiles = [InputFile]
% ==
% Align sequences in InputFile using ordinary clustalw primary sequence alignment
align_clustalw([InputFile],Options,OutputFile) :-
	assert(align_method(clustalw_align)),
	forall(member(Opt,Options),assert(Opt)),
	cl(clustalw),
	cl(constrained_align),
	build_alignments(InputFile,OutputFile),
	check_or_fail(file_exists(OutputFile), 'alignments file not produced!').

	
%% sort_alignments(+InputFiles,+Options,+OutputFile)
sort_alignments([AlignmentsFile],_Options,OutputFile) :-
	terms_from_file(AlignmentsFile,Alignments),
	length(Alignments,NumAligns),
	write(NumAligns), write(' alignments..'),nl,
	writeln('Sorting alignments (may take some time)'),
	sort(Alignments,SortedAlignments),
	writeln(done),
	terms_to_file(OutputFile,SortedAlignments).

%% as_pylip_matrix
% Convert prolog fact distance matrix to the phylip distance matrix format
% suitable for using as input to rapidnj
as_phylip_matrix([AlignmentsFile],_Options,OutputFile) :-
	cl(phylip),
	build_phylip_matrix(AlignmentsFile,OutputFile).