:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

:- [parser].

% Input Format Specification
lost_input_formats(parse,[text(genemark_hmm_report)]).
% Output Format Specification
lost_output_format(parse,_,[text(prolog(ranges(_)))]).

parse([InputFile],_Options,OutputFile) :-
	parse_genemark_hmm(InputFile,OutputFile).


test :-
	parse(['/Users/cth/code/lost//data/http_get_4.gen'],[],'/Users/cth/code/lost//data/genemark_hmm_parse_11.gen').