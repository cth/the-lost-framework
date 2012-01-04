:- module(scheduler,[scheduler_init/0, scheduler_shutdown/0, scheduler_loop/2]).

/** <module> Task scheduler

The Task scheduler is a mechanism for running PRISM processes with dependencies in parallel.
It is used behing the scenes by the script language.
It uses the scheduler tree structure to repreent tasks and their dependencies and
calls an external shell script ($LOST_BASE_DIR/utilities/procrun.sh) to launch new processes. 
Communicates with the shell script via two files:

It writes tasks to be run to pending_tasks_file.txt and reads signals about completion 
of tasks from the file completed_tasks_file.txt.

@author Christian Theil Have

*/

:- lost_include_api(scheduler_tree).
:- lost_include_api(annotation_index).

scheduler_writeln(L) :-
	write('scheduler: \t'),
	writeln(L).

%% scheduler_init
% Open communication input and output files of the scheduler and starts
% the shell script based PRISM process runner.
scheduler_init :-
	lost_tmp_directory(TmpDir),
	atom_concat(TmpDir, '/pending_tasks_file.txt',PendingTasksFile),
	atom_concat(TmpDir, '/completed_tasks_file.txt',CompletedTasksFile),
	(file_exists(PendingTasksFile) ->
		delete_file(PendingTasksFile)
		;
		true),
	(file_exists(CompletedTasksFile) ->
		delete_file(CompletedTasksFile)
		;
		true),
	tell(CompletedTasksFile), told, % Create the file
	open(PendingTasksFile,write,_PendingStream,[alias(pending_tasks_file)]),
	open(CompletedTasksFile,read,_CompletedStream,[alias(completed_tasks_file)]),
	% Launch shell script scheduler!
	lost_utilities_directory(UtilsDir),
	lost_config(concurrent_processes,MaxThreads),
	atom_integer(MaxThreadsAtom,MaxThreads),
	lost_config(prism_command,PRISM),
	atom_concat_list([UtilsDir,'/scheduler.sh "', PRISM,'" "',MaxThreadsAtom,'" "',PendingTasksFile, '" "', CompletedTasksFile, '" '],SchedulerScriptCommand),
	scheduler_writeln('running scheduler.sh:'),
	scheduler_writeln(SchedulerScriptCommand),
	system(SchedulerScriptCommand).


%% scheduler_shutdown
% Close files associated with the scheduler
scheduler_shutdown :-
	scheduler_wait_for_shutdown(10),
	scheduler_writeln('shutting down.'),
	close(pending_tasks_file),
	close(completed_tasks_file).
	
%% scheduler_loop(+SchedulerTree,+RunQueue)
% This is the main loop that runs the scheduler

%  Loop termination: When both the SchedulerTree and the RunQueue are empty
scheduler_loop(SchedulerTree,[]) :-
	scheduler_tree_empty(SchedulerTree),
	scheduler_writeln('All tasks completed.').

% There is a ready task and the room it the queue for scheduling it
% -> schedule it
scheduler_loop(SchedulerTree,RunQueue) :-
	lost_config(concurrent_processes,MaxThreads),
	length(RunQueue,QueueLen),
	QueueLen < MaxThreads,
	scheduler_tree_ready_task(SchedulerTree,ReadyTask),	!,
	scheduler_writeln('Ready task:'),
	scheduler_writeln(ReadyTask),
	scheduler_tree_lookup(ReadyTask,SchedulerTree,[node(ReadyTask,ready,Model,Goal,_)]),!,
	schedule_task(ReadyTask,Model,Goal),!,
	scheduler_tree_set_running(ReadyTask,SchedulerTree,SchedulerTreeNext),!,
	scheduler_tree_print(SchedulerTreeNext),
	scheduler_loop(SchedulerTreeNext,[ReadyTask|RunQueue]).
	
%scheduler_loop([],Queue) :-
	

% Otherwise: i.e if there is either not a ready task to run or the queue is saturated, 
% then wait for a task to complete. If, after the task completes, the scheduler tree has
% no more ready tasks, then send the hangup signal to the process scheduler
scheduler_loop(SchedulerTree,RunQueue) :-
	not(scheduler_tree_empty(SchedulerTree)),
	scheduler_wait(SchedulerTree,RunQueue,SchedulerTreeUpdated,QueueUpdated),
	(scheduler_tree_lookup(_,SchedulerTreeUpdated,[node(_,ready,_,_,_)]) ->
		true
		;
		scheduler_writeln('No more tasks to be scheduled. Sending hangup.'),
		writeln(pending_tasks_file,'hangup\n') % Cause shell script scheduler to shut down when done
	),
	scheduler_loop(SchedulerTreeUpdated,QueueUpdated).

%% schedule_task(+TaskId,+Model,+Goal)
% Start a new tas
schedule_task(TaskId,Model,Goal) :-
	scheduler_writeln(schedule_task(TaskId,Model,Goal)),
	lost_model_directory(Model,ModelDirectory),	
	write(pending_tasks_file, TaskId), nl(pending_tasks_file),
	write(pending_tasks_file, ModelDirectory), nl(pending_tasks_file),
	writeq(pending_tasks_file, Goal), nl(pending_tasks_file).

%% scheduler_wait(+SchedulerTree,+Queue,-UpdatedSchedulerTree,-UpdatedQueue)
% This will block until a task has completed
% When the task has completed, it will be removed from the queue and the scheduler tree
% resulting in the updated ouput variables.
scheduler_wait(Tree,[],Tree,[]).

scheduler_wait(SchedulerTree,Queue,SchedulerTreeUpdated,QueueUpdated) :-
	scheduler_wait_for(TaskId),
	subtract(Queue,[TaskId],QueueUpdated),
	scheduler_tree_lookup(TaskId,SchedulerTree,[node(TaskId,_,Model,Goal,_)]),
	scheduler_tree_set_completed(TaskId,SchedulerTree,SchedulerTreeUpdated),
	scheduler_update_index(Model,Goal).

%% scheduler_wait_for(-TaskId)
% Read the next task id from the completed tasks file and unify TaskId to this id
% If there are no (next) completed task yet, then it will wait for 500 msecs and loop
scheduler_wait_for(TaskId) :-
	current_input(StdIn),
	set_input(completed_tasks_file),
	readLine(TaskLine),
	set_input(StdIn),
	((TaskLine = []) -> % When the end of file is reached, [] is return by readLine
		TaskLine1 = []
		;
		reverse(TaskLine,[10|TaskLine1])), % The case for empty lines
	((TaskLine1 = []) ->
		scheduler_writeln('No new input from process runner.'),
		sleep(1000),
		scheduler_wait_for(TaskId)
		;
		atom_codes(TaskAtom,TaskLine1),
		atom_integer(TaskAtom,TaskId),
		atom_concat_list(['Task with id=', TaskAtom, ' completed'],Line),
		scheduler_writeln(Line)
	).
	
%% scheduler_wait_for_shutdown(+Count)
% Gracefully wait for scheduler shutdown. Wait for hangup signal from process runner for a number of seconds
% before terminating.
scheduler_wait_for_shutdown(0) :-
	scheduler_writeln('Time out for process runner.').
	
scheduler_wait_for_shutdown(CountDown) :-
	current_input(StdIn),
	set_input(completed_tasks_file),
	readLine(Line),
	set_input(StdIn),
	(atom_codes('hangup\n',Line) ->
		true
		;
		sleep(1000),
		NextCountDown is CountDown - 1,		
		scheduler_wait_for_shutdown(NextCountDown)).


%% scheduler_update_index(+Model,+Goal)
% This will update the state and result file of the task associated Model and Goal
% to symbolize the the task completed.
scheduler_update_index(Model,Goal) :-
	Goal =.. [ Functor, Inputs, Options, Filename ],
	lost_data_index_file(AnnotIndex),
	lost_file_index_get_filename(AnnotIndex,Model,Functor,Inputs,Options,Filename),
	check_or_fail(file_exists(Filename),interface_error(missing_annotation_file(Filename))),
	lost_file_index_update_file_timestamp(AnnotIndex,Filename).
