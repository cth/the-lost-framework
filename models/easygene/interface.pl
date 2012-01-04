:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

:- task(parse([text(easygene_report)],[],[test(prolog(ranges(gene)))])).

%% parse(+InputFiles,+Options,+OutputFile)
% Parse a report in the easygene format.
parse([Easygene_Report],_Options,OutputFile) :-
	consult('eg_parser.pl'),  % Not nice 
	eg_parser(Easygene_Report,OutputFile).
