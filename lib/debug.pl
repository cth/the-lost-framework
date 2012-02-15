:- module(debug,[debug/2]).
/** <module> debug
@author Christian Theil Have

Module used for debugging purposes. The =|debug/2|= predicate is used to write debugging information
to standard out. The first argument is a source identifier which identifies where the information is 
written from. For instance, if the debug message comes from the script module, we would write
==
debug(script, 'Some message'),
==
which would result in the message,

==
DEBUG(script): Some message
==

being written to standard out.

*/

:- use(lists).

debug(off) :-
        assert(lost_no_debug).

debug(Source,Message) :-
        catch(lost_no_debug,_,fail),
        !.

%% debug(+Source,+MessageList)
% Write each element of MessageList to standard out with Source prepended to it.
debug(Source,MessageList) :-
	is_list(MessageList),
	!,
	write('DEBUG('),
	write(Source),
	write('): '),
	forall(member(Message,MessageList), write(Message)),
	nl.

%% debug(+Source,+Message)
% Write Message to standard out with Source prepended to it.
debug(Source,Message) :-
	write('DEBUG('),
	write(Source),
	write('): '),
	write(Message),
	nl.


	
		
