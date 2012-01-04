% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- lost_include_api(interface).

top <- partest::top(mid(1),mid(2)).
mid(1) <- partest::mid(bottom(1)).
mid(2) <- partest::mid(bottom(2),bottom(3)).

bottom(X) <- member(X,[1,2,3,4]) | partest::bottom(file(X)).
