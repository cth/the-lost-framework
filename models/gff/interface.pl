:- task(parse([text(gff)],[],text(prolog(ranges(gene))))).

%% parse(InputFiles,Options,OutputFile)
% Parse a file in the GFF format
parse([InputFile],_Options,OutputFile) :-
	cl(parser),
	parse_gff_file(InputFile,OutputFile).