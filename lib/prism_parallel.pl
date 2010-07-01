%% This library is a utility to run PRISM processes in parallel 
%

%% prism_parallel(+Tasklist)
% 
prism_parallel(Tasklist) :-
        write_tasklist_to_file(Tasklist,TaskFile),
        lost_utilities_directory(UtilsDir),
        lost_config(prism_command,PRISM),
        lost_config(concurrent_processes,P),
        atom_integer(PA,P),
        atom_concat_list([UtilsDir,'prism_parallel.sh ',PRISM,' ',PA,' ',TaskFile],Command),
        writeq(Command),nl,
        system(Command).
        
write_tasklist_to_file(Tasklist,File) :-
        lost_tmp_directory(TmpDir),
        atom_concat(TmpDir,'tasklist.pl', File),
        tell(File),
        write_tasklist(Tasklist),
        told.

write_tasklist([]).
write_tasklist([Goal|GoalsRest]) :-
        portray_clause(Goal),
        !,
        write_tasklist(GoalsRest).

