:- task(parse([text(rnt)],[],text(prolog(ranges(gene))))).

%% parse(InputFiles,Options,OutputFile)
% Parse a file in the .rnt format
parse([InputFile],_Options,OutputFile) :-
	cl(parser),
	parse_rnt_file(InputFile,OutputFile).
