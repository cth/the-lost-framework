:- task(cluster(text(phylip),[],text(newick))).

config(rapidnj,'/opt/rapidnj/rapidnj').

%% cluster(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [InputFile]
% ==
% Clusters alignment distances in InputFile and writes a tree (newick format) to OutputFile
cluster([InputFile],Options,OutputFile) :-
        config(rapidnj,RapidNJ),
	atom_concat_list([RapidNJ, ' ', InputFile, ' > ', OutputFile],Cmd),
	writeln(Cmd),
	system(Cmd).
