:- ['../../lost.pl'].
:- lost_include_api(interface). 
:- lost_include_api(utils_parser_report).

% Generate a file of predicate based on Easygene report.

lost_input_formats(parse, [text(ptt)]).
lost_output_format(parse, _Options, text(prolog(ranges(gene)))).


% Option
lost_option(parse,genome_key,'U00096', 'Specify the genome key of GeneBank.'). % Note: information not available in the PTT file

parse([InputFile],Options,OutputFile) :-
        consult(ptt_parser),
        get_option(Options,genome_key,Key),
        gb_parser(InputFile,Key,OutputFile).
