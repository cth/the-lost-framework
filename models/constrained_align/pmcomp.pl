% 
% Alignment using pmcomp

:- ['../../lost.pl'].

:- use(genedb).
:- use(fasta).
:- use(terms).
:- use(debug).
:- use(lists).

pmcomp('/opt/pmcomp/pmcomp.pl').

rnafold('/usr/local/bin/RNAfold').
	
pmcomp_align(A,B,Distance) :-
	% create fasta file
	debug(pmcomp_align, 'Creating fasta sequence: align_input.fasta'),
	gene_extra_field(A,pylis_sequence,SeqA),
	gene_extra_field(B,pylis_sequence,SeqB),
	to_fasta('sequenceA',SeqA,FastaA),
	to_fasta('sequenceB',SeqB,FastaB),
	append(FastaA,FastaB,Fasta),
	open('align_input.fasta',write,Stream),
	forall(member(Code,Fasta),put_code(Stream,Code)),
	close(Stream),
	debug(pmcomp_align, 'Wrote fasta sequence: align_input.fasta'),
	% Run RNAFold
	rnafold(RNAFOLD),
	atom_concat_list([RNAFOLD,' -nolp -p < ', 'align_input.fasta'],RNAFoldCmd),
	debug(pmcomp_align, ['running rnafold: ', RNAFoldCmd]),
	system(RNAFoldCmd),
	% Run pmcomp.pl
	pmcomp(PMCOMP),
	atom_concat_list([PMCOMP,' sequenceA_dp.ps sequenceB_dp.ps > pmcomp_results.dat'],PMCOMPCmd),
	debug(pmcomp_align, ['running pmcomp.pl: ', PMCOMPCmd]),
	system(PMCOMPCmd),
        debug(pmcomp_align,'parsing results:'),
	parse_pmcomp('pmcomp_results.dat',Distance),
        debug(pmcomp_align,'parsed pmcomp results').
	
test_align :-
	terms_from_file('two_sequences.pl',[A,B]),
	pmcomp_align(A,B,Distance),
	debug(test, ['distance=',Distance]).



test_parse :- parse_pmcomp('pmcomp_output.pl',Score),writeln(Score).

parse_pmcomp(File,Score) :-
	readFile(File,Codes),
	pmcomp_results(Score,Codes,[]).

pmcomp_results(Score) -->
	something,
	score(Score),
	something.


score(Score) -->
    	"recalculated score: ",
		integer(NonFractionPart),
		".",
        /*
        digit(F1),
        digit(F2), % Just the two most significant digits
        digits(_Rest), % If there are more digits than we can handle
        */
        most_significant_digits(SigDigits),
        {
                atom_codes(Atom,SigDigits),
                atom_integer(Atom,Fractional),
                move_after_comma(Fractional,FractionFloat),
                Score is NonFractionPart + FractionFloat
        },
        newline.

score(Score) -->
    	"recalculated score: ",
        "-",
	integer(NonFractionPart),
	".",
        most_significant_digits(SigDigits),
        {
                atom_codes(Atom,SigDigits),
                atom_integer(Atom,Fractional),
                move_after_comma(Fractional,FractionFloat),
                Score is -1 * (NonFractionPart + FractionFloat)
        },
        newline.


most_significant_digits([F1,F2]) --> 
        digit(F1),
        digit(F2), % Just the two most significant digits
        digits(_Rest). % If there are more digits than we can handle

most_significant_digits([F1]) --> 
        digit(F1),
        digits(_Rest). % If there are more digits than we can handle

integer(Integer) -->
	digits(Digits),
	{
		Digits \= [],
		atom_codes(Atom,Digits),
		atom_integer(Atom,Integer)
	}.
	
move_after_comma(X,Y) :-
    Y is X / 10,
    Y < 1.
move_after_comma(X,Y) :-
    Z is X / 10,
    Z >= 1,
    move_after_comma(Z,Y).

digits([]) --> [].
digits([D|Ds]) --> digit(D), digits(Ds).

digit(D) --> [D], { atom_codes('0123456789',Digits), member(D,Digits) }.

something --> [].
something --> [_], something.

newline --> "\n".

	
	
