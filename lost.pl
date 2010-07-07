%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% Make changes to these three lines:
lost_config(prism_command,'prism').
lost_config(lost_base_directory,'/change/to/your/local/lost/directory/').
lost_config(platform,'to specify unix or windows').
lost_config(concurrent_processes,5).

% Do not change below this line

lost_include_api(_) :-
        lost_config(lost_base_directory,'/change/to/your/local/lost/directory'),
        throw('Please set lost_base_directory!!!').
lost_include_api(_) :-
        lost_config(platform,'to specify unix or windows'),
        throw('Please set your plateform!!!').

% Basic rule to glue in other APIs

lost_include_api(Name) :-
	catch(lost_api_loaded(Name),_,fail), !.

lost_include_api(Name) :-
	lost_config(lost_base_directory, Basedir),
	atom_concat(Basedir,'lib/',SharedDir),
	atom_concat(SharedDir,Name,DirAndName),
	atom_concat(DirAndName,'.pl',FullName),
	consult(FullName),
	assert(lost_api_loaded(Name)).

% Basic rule to glue in other APIs
lost_include_script(Name) :-
	lost_config(lost_base_directory, Basedir),
	atom_concat(Basedir,'scripts/',ScriptDir),
	atom_concat(ScriptDir,Name,DirAndName),
	atom_concat(DirAndName,'.pl',FullName),
	consult(FullName).

:- lost_include_api(interface).

