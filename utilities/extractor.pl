:- ['../lost.pl'].

:- use(lists).

go(Model) :- 
	lost_interface_tasks(Model,Tasks),
	findall(Name,(member(Task,Tasks),task_name(Task,Name)),TaskNames), 
	zip(TaskNames,Tasks,TaskSignatureMap),
	open('/tmp/target.pl',write,OutStream),
	write(OutStream,':- module('),
	write(OutStream,Model),
	write(OutStream,',['),
	write_exports(OutStream,TaskNames),
	nl(OutStream),
	write_fake_doc_header(OutStream,Model),!, 
	lost_model_interface_file(Model,InterfaceFile),!,
	%readFile(InterfaceFile,InterfaceFileContents),
	%forall(member(C,InterfaceFileContents),(atom_codes(A,[C]),write(A))),
	open(InterfaceFile,read,InStream),
	interface_file_with_type_comments(InStream,OutStream,TaskSignatureMap),
	close(InStream),
	close(OutStream).

interface_file_with_type_comments(InStream,OutStream,TaskSignatureMap) :-
	current_input(StdIn),
	set_input(InStream),
	readLine(Line),
	set_input(StdIn),
%	forall(member(C,Line),(atom_codes(A,[C]),write(A))),
	((Line = []) ->
		true
		;
		forall(member(C,Line),(atom_codes(A,[C]),write(OutStream,A))),
		(task_comment_line(Task,Line,[]) ->
			writeln('TASK COMMENT LINE'),
			write('"'),write(Task), write('"'),nl,
			write(TaskSignatureMap),nl,
			member([Task,TaskDeclaration],TaskSignatureMap),
			TaskDeclaration =.. [ Task, InputFileTypes, Options, OutputFileType ],
			write(here),nl,			
			writeln(TaskDeclaration),
			writeln(OutStream,'% Type signature: '),
			writeln(OutStream,'% == '),
			writeln(OutStream,'% InputFiles:'),
			findall(N,nth1(N,InputFileTypes,Type),Indices),
			forall(member(Idx,Indices),
					(
					nth1(Idx,InputFileTypes,Type),
					write(OutStream, '%     '),
					write(OutStream, Idx),
					write(OutStream, '. '),
					writeq(OutStream,Type),
					nl(OutStream)
					)),
			write(here),
			writeln(OutStream,'% OutputFile:'),
			write(OutStream,'%     '),
			writeln(OutStream,OutputFileType),
			writeln(OutStream, '% Options:'),
			forall(member(Opt,Options),
				(Opt =.. [ Key, Value ],
				write(OutStream,'%     - '),
				write(OutStream,Key),
				write(OutStream,' (default value: '), 
				write(OutStream,Value),
				writeln(OutStream,')')
				)),
			writeln(OutStream,'% ==')
			;
			true), 
		interface_file_with_type_comments(InStream,OutStream,TaskSignatureMap)
	).

task_comment_line(Task) -->
	"%%",
	spaces,
	functor(TaskCodes), "(",
	remainder,
	{atom_codes(Task,TaskCodes)}.


/*	
task_comment_line(Task,InputFiles,OutputFile) -->
	"%%",
	spaces,
	functor(TaskCodes), "(",
	"[",comma_sep_list(InputFiles),"]",
	comma_space,
	comma_sep_list(_Options),
	comma_space,
	variable(OutputFileCodes),")",
	remainder,
	{
		atom_codes(Task,TaskCodes),
		atom_codes(OutputFile,OutputFileCodes)
	}.
*/

comma_space -->	spaces, ",", spaces.

spaces --> space, spaces. % Greedy
spaces --> [].

space --> [ 9 ]. % tab character
space --> [ 32 ]. % normal space character
space --> { atom_codes('\t',Codes) }, Codes.

functor([]) --> [].
functor([Symbol|Rest]) --> [Symbol], { [Symbol] \= "(" }, functor(Rest).

comma_sep_list([]) --> [].
comma_sep_list([Item]) --> spaces,variable(ItemCodes),spaces, {atom_codes(Item,ItemCodes)}.
comma_sep_list([Item|Rest]) -->
	variable(ItemCodes),
	{atom_codes(Item,ItemCodes)},
	comma_space,
	comma_sep_list(Rest).

variable([]) --> [].
variable([Symbol|Rest]) --> [Symbol], { [Symbol] \= ",", [Symbol] \= " ", Symbol \= "[", Symbol \= "]" }, variable(Rest).

remainder --> [].
remainder --> [_], remainder.



write_exports(Stream,[OneTask]) :-
	write(Stream,OneTask), writeln(Stream,'/3]).'), nl(Stream).

write_exports(Stream,[OneTask|Rest]) :-
	write(Stream,OneTask), write(Stream,'/3,'),
	write_exports(Stream,Rest).

write_fake_doc_header(Stream,Model) :-
	write(Stream,'/** <module> '),
	writeln(Stream,Model),
	lost_model_directory(Model,Dir),
	atom_concat(Dir,'/README',ReadmeFile),
	(file_exists(ReadmeFile) ->
		readFile(ReadmeFile,ReadmeContents),
		forall(member(C,ReadmeContents),(atom_codes(A,[C]),write(Stream,A)))
		;
		writeln(Stream,'No README for model')
	),
	nl(Stream),
%&	writeln(Stream,' ---++ Tasks'), % FIXME: Make subsection
	writeln(Stream,'*/').
