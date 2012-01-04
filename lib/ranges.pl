:- module(ranges,[overlaps/4, sum_range_list/2]).
/** <module> working with ranges

Predicates for working ranges. A range is usually specified by two integer denoting the beginning and end
of the range (both inclusive).

@author Christian Theil Have

*/

%% overlaps(+Start1,+End1,+Start2,+End2)
% Determine if two ranges overlap
overlaps(Start1,End1, Start2,_) :-
        Start1 =< Start2,
        End1 >= Start2.
overlaps(Start1,_, Start2,End2) :-
        Start2 =< Start1,
        End2 >= Start1.

% Sums the number of positions in a list of ranges
sum_range_list([],0).
sum_range_list([[From,To]|Rest],Sum) :-
	LocalSum is To - From + 1,
	sum_range_list(Rest, RestSum),
	Sum is LocalSum + RestSum.
