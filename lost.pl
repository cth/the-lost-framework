%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lost_config(prism_command,'/opt/prism/bin/prism').
lost_config(lost_base_directory, '/home/cth/code/lost/').

% Basic rule to glue in other APIs
lost_include_api(Name) :-
	lost_config(lost_base_directory, Basedir),
	atom_concat(Basedir,'shared/',SharedDir),
	atom_concat(SharedDir,Name,DirAndName),
	atom_concat(DirAndName,'.pl',FullName),
	cl(FullName).