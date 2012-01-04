% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- lost_include_api(interface).

a <- m::a(b,c).
b <- m::b(d).
c <- m::c(d).
d <- m::d(file('input.txt')).
