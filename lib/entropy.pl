%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  NAME :
%      entropy.pl
%
% VERSION :
%     1.0
%
% AUTHOR : Ole Torp Lassen
%
% FUNCTION :
%      		entropy/1, entropy/2
%					tools for calculating entropy of distribtions
% 
% HISTORIC :
%  April 15 2011: creation                           OTL
%
% REMARKS : any problem, contact otl@ruc.dk
%
% NOTE TO THE USER: entropy/2 works for 
%
%				- single random variables that have been declared using values/2 and have been instatniated, i.e.:
%					values(t,[1,2]).
%					msw(t,V),
%					entropy(t,1.0.)
%
%				- lists of random variables that have been declared using values/2 and have been instatniated, i.e.:
%					values(t,[1,2]).values(u,[1,2]).
%					msw(t,T),msw(u,U),
%					entropy([t,u],2.0).
%
%				- parameterfiles that been saved with save_sw/1
%					entropy(Filename,E).
%
%		entropy/1 writes to output. 	 
%
% REFERENCE: 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


entropy(Params):-
	entropy(Params,H),
	current_output(Stream),
	writeln(Stream,H).

entropy(Params,H):-
	(
	atom(Params),file_exists(Params) ->
	 	consult(Params),
	 	findall(Switch, switch(Switch,_,_,_), Switches),
	 	entropy_list(Switches,H)
	;
	Params = [_Head|_Tail] -> 
		entropy_list(Params,H)
	;
	get_sw(Params,[_fix,_domain,Dist]) ->
		entropy_switch_rec(Dist,PosH),
		H is - PosH
	).

entropy_list([],0).
entropy_list([S|Rest],H):-
	entropy_switch(S,HS),
	entropy_list(Rest,HRest),
	H is HS + HRest.

entropy_switch(Switch,H):-
	(
	clause(switch(Switch,_fix,_domain,Dist),true)
	;
	clause(values(Switch,_),true),
	get_sw(Switch,[_fix,_domain,Dist])
	),!,
	entropy_switch_rec(Dist,PosH),
	H is - PosH.

entropy_switch_rec([],0).
entropy_switch_rec([X|Tail],H):-
	Log2_X is log(2,X),
	This_H is X * Log2_X,
	entropy_switch_rec(Tail,Rest_H),
	H is This_H + Rest_H.
	
joint_entropy() :-
	
	
conditional_entropy(Switch1,Switch2) :-
	entropy_switch(Switch1,Entropy1),
	entropy_switch(Switch2,Entropy2),
	
	