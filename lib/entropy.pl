entropy(Paramfile,H):-
	open(Paramfile,read, PStream,[alias(paramsin)]),
	get_declarations(PStream),
	close(PStream),
	restore_sw(Paramfile),
	findall(Switch, get_sw(Switch,_), Switches),
	entropy_list(Switches,H).
		
entropy_list([],0).
entropy_list([S|Rest],H):-
	entropy_switch(S,HS),
	entropy_list(Rest,HRest),
	H is HS + HRest.

entropy_switch(Switch,H):-
	get_sw(Switch,[_fixed,_range,Dist]),
	entropy_switch_rec(Dist,PosH),
	H is - PosH.

entropy_switch_rec([],0).
entropy_switch_rec([X|Tail],H):-
	Log2_X is log(2,X),
	This_H is X * Log2_X,
	entropy_switch_rec(Tail,Rest_H),
	H is This_H + Rest_H.
	
get_declarations(Stream):-
	read(Stream,Term),
	(
	Term \= end_of_file ->
		Term =.. [switch,Name,_,Range,_],
		assert(values(Name,Range)),
		get_declarations(Stream)
	;
	true
	).