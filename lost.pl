%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lost_config(prism_command,'prism').
lost_config(lost_base_directory,'/change/to/your/local/lost/directory').
lost_config(platform,'to specify unix or windows').

lost_include_api(_) :-
        lost_config(lost_base_directory,'/change/to/your/local/lost/directory'),
        throw('Please set lost_base_directory!!!').
lost_include_api(_) :-
        lost_config(platform,'to specify unix or windows'),
        throw('Please set your plateform!!!').



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
