:- module(scheduler_tree,[]).

:- lost_include_api(misc_utils).

/** <module> Scheduler tree data structure

@author: Christian Theil Have

The scheduler tree is a data tree data structure to represent call graphs
of scripts written for the lost framework. 

It is used by the process scheduler to infer which tasks can be run in parallel.

A Task may be in several states
- ready (in the tree, but not yet running)
- Running (but not yet completed)
- Completed (in which case it is removed from the tree)

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Scheduler tree data structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% scheduler_tree_create(-EmptyTree)
% Creates a new scheduler tree which is initially empty 
scheduler_tree_create([0,[]]).

%% scheduler_tree_add(+Model,+Goal,+ParentId,+Tree,-UpdatedTree,-NextId)
% UpdatedTree is Tree with a node inserted as a child to ParentId. 
% NextId is the Id of the newly inserted node.
% If there are allready an existing node for the same Model and Gaol, but possibly
% a different parent, then the new node is given the same task id
scheduler_tree_add(Model,Goal,ParentId,[MaxId,Tree],[NextId,UpdatedTree],NextId) :-
	NextId is MaxId + 1,
	scheduler_tree_add_rec(NextId,Model,Goal,ParentId,Tree,UpdatedTree).

% This one adds to the top of the tree (e.g. the grand parent)
scheduler_tree_add_rec(NextId,Model,Goal,nil,InTree,[node(NextId,ready,Model,Goal,InTree)]).

scheduler_tree_add_rec(NextId,Model,Goal,ParentId,
		[node(ParentId,ParentState,ParentModel,ParentGoal,Children)],
		[node(ParentId,ParentState,ParentModel,ParentGoal, [node(NextId,ready,Model,Goal,[])|Children])]).
			
scheduler_tree_add_rec(NextId,Model,Goal,Parent,
					[node(OtherTaskId,ready,OtherModel,OtherGoal,Children)],
					[node(OtherTaskId,ready,OtherModel,OtherGoal,UpdatedChildren)]) :-
	findall(UpdatedChild,
		(
			member(Child,Children),
			(scheduler_tree_add_rec(NextId,Model,Goal,Parent,[Child],[UpdatedTree]) ->
				UpdatedChild = UpdatedTree
				;
				UpdatedChild = Child)
		),
		UpdatedChildren).

		
%% scheduler_tree_remove(+TaskId,+InitialTree,-UpdatedTree)
% UpdatedTree is the initial tree without the subtrees rooted at the nodes with TaskId 
scheduler_tree_remove(TaskId,[MaxId,Tree],[MaxId,UpdatedTree]) :-
	scheduler_tree_remove_rec(TaskId,Tree,UpdatedTree).

scheduler_tree_remove_rec(TaskId,[node(TaskId,_State,_Model,_Goal,_Children)],[]).

scheduler_tree_remove_rec(TaskId,[node(OtherTaskId,State,Model,Goal,[])],[node(OtherTaskId,State,Model,Goal,[])]) :-
	TaskId \= OtherTaskId.

scheduler_tree_remove_rec(TaskId,
	[node(OtherTaskId,State,OtherModel,OtherGoal,Children)],
	[node(OtherTaskId,State,OtherModel,OtherGoal,UpdatedChildren)]) :-
		length(Children,X),
		1 =< X,
		findall(UpdatedChild,
			(
				member(Child,Children), 
				Child=node(ChildTaskId,_,_,_,_), 
				ChildTaskId \= TaskId,
				scheduler_tree_remove_rec(TaskId,[Child],[UpdatedChild])
			),
			UpdatedChildren).

%% scheduler_tree_replace(+Node,+Tree,UpdatedTree)
% Node replaces the subtree of the root node has the same task id as node
scheduler_tree_replace(Node,[MaxId,Tree],[MaxId,UpdatedTree]) :-
	scheduler_tree_replace_rec(Node,Tree,UpdatedTree).

scheduler_tree_replace_rec(node(TaskId,State,Model,Goal,Children),[node(TaskId,_,_,_,_)],[node(TaskId,State,Model,Goal,Children)]).

scheduler_tree_replace_rec(node(TaskId,_,_,_,_),[node(OtherTaskId,State,Model,Goal,[])],[node(OtherTaskId,State,Model,Goal,[])]) :-
	TaskId \== OtherTaskId.

scheduler_tree_replace_rec(node(TaskId,State,Model,Goal,Children),
		[node(OtherTaskId,OtherState,OtherModel,OtherGoal,OtherChildren)],
		[node(OtherTaskId,OtherState,OtherModel,OtherGoal,OtherUpdatedChildren)]) :-
	TaskId \== OtherTaskId,
	length(OtherChildren,X),
	1 =< X,
	findall(UpdatedChild,(member(Child,OtherChildren),scheduler_tree_replace_rec(node(TaskId,State,Model,Goal,Children),[Child],[UpdatedChild])),OtherUpdatedChildren).
	
%% scheduler_tree_replace_by_taskid(+TaskId,+Node,+Tree,UpdatedTree)
% Node replaces the subtree of the root node has the same task id as node
scheduler_tree_replace_by_taskid(TaskId,Node,[MaxId,Tree],[MaxId,UpdatedTree]) :-
	scheduler_tree_replace_by_taskid_rec(TaskId,Node,Tree,UpdatedTree).

scheduler_tree_replace_by_taskid_rec(TaskId,UpdatedNode,[node(TaskId,_,_,_,_)],UpdatedNode).

scheduler_tree_replace_by_taskid_rec(TaskId,_UpdatedNode,[node(OtherTaskId,State,Model,Goal,[])],[node(OtherTaskId,State,Model,Goal,[])]) :-
	TaskId \== OtherTaskId.

scheduler_tree_replace_by_taskid_rec(TaskId,UpdatedNode,
		[node(OtherTaskId,OtherState,OtherModel,OtherGoal,OtherChildren)],
		[node(OtherTaskId,OtherState,OtherModel,OtherGoal,OtherUpdatedChildren)]) :-
	TaskId \== OtherTaskId,
	length(OtherChildren,X),
	1 =< X,
	findall(UpdatedChild,(member(Child,OtherChildren),scheduler_tree_replace_by_taskid_rec(TaskId,UpdatedNode,[Child],[UpdatedChild])),OtherUpdatedChildren).
	
%% scheduler_tree_reduce(+Tree,-ReducedTree)
% This reduces a scheduler tree by compacting nodes which are structurally the same (i.e. only differing in the task id)
% Such task will be given the same task id
scheduler_tree_reduce([MaxId,Tree],[NewMaxId,ReducedTree]) :-
	scheduler_tree_reduce_rec([MaxId,Tree],[NewMaxId,ReducedTree]).
	
scheduler_tree_reduce_rec([MaxId,Tree],[NewMaxId,ReducedTree]) :-
	scheduler_tree_reduce_once([MaxId,Tree],[NewMaxId1,ReducedTree1]),
	!,
	scheduler_tree_reduce_rec([NewMaxId1,ReducedTree1],[NewMaxId,ReducedTree]).
	
scheduler_tree_reduce_rec([MaxId,Tree],[MaxId,Tree]).
	
scheduler_tree_reduce_once([MaxId,Tree],[NewTaskId,ReducedTree]) :-
	scheduler_tree_lookup(TaskId1,[MaxId,Tree],Subtree1),
	scheduler_tree_lookup(TaskId2,[MaxId,Tree],Subtree2),
	Subtree1 \= Subtree2,
	TaskId1 \= TaskId2,
	Subtree1 = [node(TaskId1,State,Model,Goal,Children1)],
	Subtree2 = [node(TaskId2,State,Model,Goal,Children2)],
	NewTaskId is MaxId + 1,
	UpdatedSubtree1 = [node(NewTaskId,State,Model,Goal,Children1)],
	UpdatedSubtree2 = [node(NewTaskId,State,Model,Goal,Children2)],
	scheduler_tree_replace_by_taskid(TaskId1,UpdatedSubtree1,[MaxId,Tree],[MaxId,UpdatedTree1]),
	scheduler_tree_replace_by_taskid(TaskId2,UpdatedSubtree2,[MaxId,UpdatedTree1],[MaxId,ReducedTree]).
	
%% scheduler_tree_lookup(+TaskId,+Tree,-Node)
% Node is the subtree which rooted at the node identified by TaskId
scheduler_tree_lookup(TaskId,[_,Tree],Subtree) :-
	scheduler_tree_lookup_rec(TaskId,Tree,Subtree).
	
scheduler_tree_lookup_rec(TaskId,[node(TaskId,State,Model,Goal,Children)],[node(TaskId,State,Model,Goal,Children)]).

scheduler_tree_lookup_rec(TaskId,[node(OtherTaskId,_,_,_,Children)],SubTree) :-
	TaskId \== OtherTaskId,
	member(Child,Children),
	scheduler_tree_lookup_rec(TaskId,[Child],SubTree).

%% scheduler_tree_change_taskid(+TaskId,+TaskIdUpdated,+TreeIn,+TreeOut)
%scheduler_tree_change_taskid(TaskId,TaskIdUpdated,TreeIn,TreeOut) :-
%	scheduler_tree_lookup(TaskId,TreeIn,[node(TaskId,State,Model,Goal,Children)]),
%	scheduler_tree_replace(TaskId,[node(TaskId,State,Model,Goal,Children)],TreeIn,TreeOut).

%% scheduler_tree_ready_task(+Tree,-TaskId)
% Find a process in the tree which is ready to run. This process must be a node which 
% a) is a leaf node and b) has the state 'ready'.
scheduler_tree_ready_task(Tree,TaskId) :-
	scheduler_tree_lookup(TaskId,Tree,[node(TaskId,ready,_,_,[])]).



%% scheduler_tree_set_running(+TaskId,+Tree,-UpdatedTree)
% True if, a) TaskId points to leaf node and b) that node has state 'ready'
scheduler_tree_set_running(TaskId,Tree,UpdatedTree) :-
	scheduler_tree_lookup(TaskId,Tree,[node(TaskId,ready,Model,Goal,[])]),
	scheduler_tree_replace(node(TaskId,running,Model,Goal,[]),Tree,UpdatedTree).


%% scheduler_tree_set_completed(+TaskId,+InitialTree,-UpdatedTree)
% 1) TaskId --> leaf node
% 2) Node as state 'running'
scheduler_tree_set_completed(TaskId,Tree,UpdatedTree) :-
	scheduler_tree_lookup(TaskId,Tree,[node(TaskId,running,_,_,[])]),
	scheduler_tree_remove(TaskId,Tree,UpdatedTree).


%% scheduler_tree_empty(+Tree)
% True if Tree is empty
scheduler_tree_empty([_,[]]).

%% scheduler_tree_print(+Tree)
% Pretty-prints a scheduler tree
scheduler_tree_print([_,[]]) :-
	writeln('Tree is empty!').

scheduler_tree_print([_,Tree]) :-
	scheduler_tree_print_rec(0,Tree).

scheduler_tree_print_rec(_,[]).
		
scheduler_tree_print_rec(Indent,[node(TaskId,State,Model,Goal,Children)]) :-
		((Indent > 0) ->
			PrevIndent is Indent - 1,
			foreach(_ in 1..PrevIndent, write('   ')), write('|'), write('--')
			;
			true
		),
		(simplify_goal(Goal,SimpleGoal) ; SimpleGoal = Goal),
		write('+'), write(TaskId), write(' '), write(State), write(' '), write(Model), write('::'), write(SimpleGoal), nl,
		NextIndent is Indent + 1,
		!,
		foreach(Child in Children, scheduler_tree_print_rec(NextIndent,[Child])).

simplify_goal(Goal,Simplified) :-
	Goal =.. [ Functor, InFiles, Opts, OutFile ],
	file_base_name(OutFile,SimplerOutFile),
	map(file_base_name,InFiles,SimplerInFiles),
	Simplified =.. [ Functor, SimplerInFiles, Opts, SimplerOutFile ].
	
%% Testing of scheduler tree predicates:
/*

make_small_tree(Tree4) :-
	scheduler_tree_create(Tree1),
	scheduler_tree_add(model1,task1,nil,Tree1,Tree2,TaskId1),
	scheduler_tree_add(model2,task2,TaskId1,Tree2,Tree3,_TaskId2),
	scheduler_tree_add(model3,task3,TaskId1,Tree3,Tree4,_TaskId3).
	
make_large_tree(Tree10) :-
	scheduler_tree_create(Tree1),
	scheduler_tree_add(model1,task1,nil,Tree1,Tree2,TaskId1),
	scheduler_tree_add(model2,task2,TaskId1,Tree2,Tree3,TaskId2),
	scheduler_tree_add(model3,task3,TaskId2,Tree3,Tree4,TaskId3),
	scheduler_tree_add(model3,task3,TaskId3,Tree4,Tree5,TaskId4),
	scheduler_tree_add(model4,task4,TaskId3,Tree5,Tree6,_TaskId5),
	scheduler_tree_add(model5,task5,TaskId1,Tree6,Tree7,TaskId6),
	scheduler_tree_add(model6,task6,TaskId4,Tree7,Tree8,_TaskId7),
	scheduler_tree_add(model7,task7,TaskId6,Tree8,Tree9,TaskId8),
	scheduler_tree_add(model7,task7,TaskId4,Tree9,Tree10,_TaskId9).
	
test_is_leaf :-
	make_small_tree(T1),
	scheduler_tree_print(T1),
	scheduler_tree_is_leaf_node(2,T1),
	scheduler_tree_is_leaf_node(3,T1),
	not(scheduler_tree_is_leaf_node(1,T1)).	

test_ready :-
	make_large_tree(T1),
	scheduler_tree_print(T1),!,
	findall(TaskId,scheduler_tree_ready_task(T1,TaskId),AllReady),
	writeln('All tasks that are ready to run: '),
	writeln(AllReady).
	
test_graph_add :-
	make_small_tree(T),
	scheduler_tree_print(T).
	
test_remove :-
	make_small_tree(T),
	scheduler_tree_print(T),!,
	scheduler_tree_remove(1,T,T2),!,
	writeln(T2),
	scheduler_tree_print(T2).
	
test_replace_node :-
	make_small_tree(T),
	scheduler_tree_print(T),!,
	scheduler_tree_lookup(2,T,SubTree1),!,
	SubTree1 = [node(Id,State,_,_,Children)],
	SubTree1_new = node(Id,State,replaced_model,replace_goal,Children),
	scheduler_tree_replace(SubTree1_new,T,T2),!,
	writeln('----- AFTER REPLACE: ------'),	
	scheduler_tree_print(T2).
	
test_set_running(T2) :-
	make_small_tree(T1),
	writeln('----- BEFORE SET RUNNING TASK 7 ------'),
	scheduler_tree_print(T1),
	scheduler_tree_set_running(2,T1,T2),
	writeln('----- AFTER SET RUNNING TASK 7 ------'),
	scheduler_tree_print(T2).

test_set_running2 :-
	make_small_tree(T1),
	writeln('----- BEFORE SET RUNNING TASK 4 ------'),
	scheduler_tree_print(T1),!,
	scheduler_tree_set_running(4,T1,T2),!,
	writeln('----- AFTER SET RUNNING TASK 4 ------'),
	scheduler_tree_print(T2).
	
test_set_completed(NewT) :-
	test_set_running(T),
	scheduler_tree_set_completed(7,T,NewT),
	writeln('----- AFTER SET COMPLETED TASK 7 ------'),
	scheduler_tree_print(NewT).
	
test_all_completed :-
	make_small_tree(T1),
	writeln('-----'),
	scheduler_tree_print(T1),
	scheduler_tree_set_running(3,T1,T2),
	scheduler_tree_set_completed(3,T2,T3),!,
	writeln('-----'),	
	scheduler_tree_print(T3),
	scheduler_tree_set_running(2,T3,T4),
	scheduler_tree_set_completed(2,T4,T5),!,
	writeln('-----'),	
	scheduler_tree_print(T5),
	scheduler_tree_set_running(1,T5,T6),!,
	writeln('T6-----'),		
	scheduler_tree_print(T6),!,
	writeln(scheduler_tree_set_completed(1,T6,T7)),
	scheduler_tree_set_completed(1,T6,T7),!,
	writeln('-----!'),
	scheduler_tree_print(T7).
	
	
	

test_lookup_by_model_goal :-
	make_small_tree(T),!,
	scheduler_tree_lookup(TaskId,T,[node(_,_,model1,task1,_)]),
	writeln(TaskId),
	scheduler_tree_print(T).
	
test_bug :-
	Tree = [3,[node(1,ready,partest,top([mid_48,mid_48],[],top_49),[node(2,ready,partest,mid([bottom_47,bottom_47],[],mid_48),[node(3,ready,partest,bottom([],[],bottom_47),[])])])]],
	scheduler_tree_print(Tree),
	scheduler_tree_lookup(3,Tree,[node(TaskId,_,partest,bottom([],[],bottom_47),_Children)]).

test_bug2(A,B) :- scheduler_tree_add(partest,bottom([],[],bottom_47),2,[3,[node(1,ready,partest,top([mid_48,mid_48],[],top_49),[node(2,ready,partest,mid([bottom_47,bottom_47],[],mid_48),[node(3,ready,partest,bottom([],[],bottom_47),[])])])]],A,B).
	
	
*/
		
		
		
