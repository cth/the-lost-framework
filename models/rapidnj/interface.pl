:- task(cluster(text(phylip),[],text(newick))).

%% cluster(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% Clusters alignment distances in InputFile and writes a tree (newick format) to OutputFile
cluster([InputFile],Options,OutputFile) :-
	atom_concat_list([rapidnj, ' ', InputFile, ' > ', OutputFile],Cmd),
	writeln(Cmd),
	system(Cmd).