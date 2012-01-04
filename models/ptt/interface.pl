%:- ['../../lost.pl'].
:- use(interface). 
:- use(utils_parser_report).
:- use(errorcheck).

:- task(parse([text(ptt)],[genome_key('n/a')], text(prolog(ranges(gene))))). 

%% parse(+InputFiles, +Options, +OutputFile) 
% ==
% InputFiles = [ PTTFile ]
% ==
% Parses a PTT file creates a gene fact for each line in ptt file.
% The value supplied option =|genome_key|= is the first argument in the gene records
% produced in the OutputFile.
parse([InputFile],Options,OutputFile) :-
        consult(ptt_parser),
        get_option(Options,genome_key,Key),
        check_or_fail(gb_parser(InputFile,Key,OutputFile),error('Parser error')),
		writeln('here').	
