%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lost_config(prism_command,'/usr/local/bin/prism ').
lost_config(lost_base_directory, '/home/cteilhave/temp/lost/').

% Basic rule to glue in other APIs
lost_include_api(Name) :-
	lost_config(lost_base_directory, Basedir),
	atom_concat(Basedir,'shared/',SharedDir),
	atom_concat(SharedDir,Name,DirAndName),
	atom_concat(DirAndName,'.pl',FullName),
	consult(FullName).


% Basic rule to glue in other APIs
lost_include_script(Name) :-
	lost_config(lost_base_directory, Basedir),
	atom_concat(Basedir,'scripts/',ScriptDir),
	atom_concat(ScriptDir,Name,DirAndName),
	atom_concat(DirAndName,'.pl',FullName),
	consult(FullName).
