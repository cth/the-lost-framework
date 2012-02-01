:- use(script).
:- use(prologdb).
:- use(fileformat).

a_sample_orf_with_pylis(O) :-
	O = orf(na,30357,30536,'+',1,[pylis([t,a,g,t,g,g,g,t,a,t,a,t,a,a,t,c,c,c,t,t,t,t,t,t,g,a,a,t,g,g,a,g,a,c,g,a,a,t,a,t,a,t,c,c,c,a,a,g,t,g,t,c,a,a,g,a,t,g,c,t,a,a,a,a,a,t,a,t,t,a,t,t,a,c,c,t,g,g,a,a,t,a,g,t,t,a,t,a,a,t,g,a,g,t,c,t,t,t,t,t]),length(180),stop(30534),starts([30357,30417,30420,30426,30471,30501,30504]),in_frame_stops([30435]),sequence([a,t,t,c,t,c,t,c,a,a,g,a,t,a,t,t,g,c,a,a,a,a,a,t,g,a,t,a,a,g,g,a,t,t,a,t,t,t,t,c,c,c,t,a,t,t,g,t,c,t,t,g,t,t,t,g,g,g,t,c,a,t,t,a,t,t,t,t,t,a,t,a,t,t,t,t,g,c,t,a,g,t,g,g,g,t,a,t,a,t,a,a,t,c,c,c,t,t,t,t,t,t,g,a,a,t,g,g,a,g,a,c,g,a,a,t,a,t,a,t,c,c,c,a,a,g,t,g,t,c,a,a,g,a,t,g,c,t,a,a,a,a,a,t,a,t,t,a,t,t,a,c,c,t,g,g,a,a,t,a,g,t,t,a,t,a,a,t,g,a,g,t,c,t,t,t,t,t,a,a])]).

sample_inputs <- a_sample_orf_with_pylis(O)	| file::from_terms([],[terms([O])]).

folded <- ppfold::fold([sample_inputs], [sequence_functor(pylis)]).

testcase(fold) :-
	Target = folded,
	rerun(Target),
	get_result_file(Target,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).

