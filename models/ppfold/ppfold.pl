% Interface to the ppfold program

:- ['../../lost.pl'].

:- use(genedb).
:- use(fasta).

:- [config]. % Load configuration 

ppfold_run(FastaFile,Folding,FreeEnergy) :-
	java(Java),
	ppfold_jar(Jar),
	atom_concat_list([Java, ' -jar ', Jar, ' ', FastaFile],PPFoldCommand),
	get_cwd(Here),
	dirname(FastaFile,There),
	chdir(There),
	system(PPFoldCommand),
	chdir(Here),
	writeln(ppfold_run),nl,
	ppfold_seq_file(FastaFile,SeqFile),
	ppfold_parse_seq_file(SeqFile,Folding),
	ppfold_ct_file(FastaFile,CTFile),
	ppfold_parse_ct_file(CTFile,FreeEnergy).

write_fasta_file(Gene,SequenceFunctor,FastaFile) :-
	lost_tmp_file('ppfold','.fasta',FastaFile),
	writeln(write_fasta_file),
	writeln(Gene),
	writeln(FastaFile),
	gene_extra_field(Gene,SequenceFunctor,Sequence),
	write('To be folded: '), writeln(Sequence),
	fasta_save_sequence(FastaFile,Sequence,'ppfold').

ppfold_seq_file(InputFile,SeqFile) :-
	atom_codes(InputFile,InputFileCodes),
	atom_codes('.fasta',FastaCodes),
	append(FilePrefix,FastaCodes,InputFileCodes),
	atom_codes('.seq',SeqCodes),
	append(FilePrefix,SeqCodes,SeqFileCodes),
	atom_codes(SeqFile,SeqFileCodes).

ppfold_ct_file(InputFile,SeqFile) :-
	atom_codes(InputFile,InputFileCodes),
	atom_codes('.fasta',FastaCodes),
	append(FilePrefix,FastaCodes,InputFileCodes),
	atom_codes('.ct',SeqCodes),
	append(FilePrefix,SeqCodes,SeqFileCodes),
	atom_codes(SeqFile,SeqFileCodes).

% We assume that there is only one folding
ppfold_parse_seq_file(SeqFile,Folding) :-
	readFile(SeqFile,List),
	seq_file(Folding,List,_RestOfFile).

ppfold_parse_ct_file(CTFile,Energy) :-
	readFile(CTFile,List),
	ct_file(Energy,List,_Rest).	


seq_file(Folding) --> "pairingmask", space, folding(Folding), newline.

folding([]) --> [].
folding(['.'|Xs]) --> ".", folding(Xs).
folding(['<'|Xs]) --> "(", folding(Xs).
folding(['>'|Xs]) --> ")", folding(Xs).

% This is quite hacky because energy is reported with more digits than Prolog can really handle
% To handle this, we just take the two most significant digits of the fractional part
ct_file(Energy) -->
	spaces,
	integer(X), 
	spaces,
	"ENERGY = ",
	integer(NonFractionPart),
	".",
	digit(F1),
	digit(F2), % Just the two most significant digits
	digits(_Rest), % If there are more digits than we can handle
	{
		atom_codes(Atom,[F1,F2]),
		atom_integer(Atom,Fractional),
		move_after_comma(Fractional,FractionFloat),
		Energy is NonFractionPart + FractionFloat
	},
	spaces,
	newline.


newline --> "\n".

space --> " ".
space --> "\t".
spaces --> [].
spaces --> space, spaces.

integer(Integer) -->
	digits(Digits),
	{
		Digits \= [],
		atom_codes(Atom,Digits),
		atom_integer(Atom,Integer)
	}.
	
float(Float) -->
	integer(Part1),
	".",
	integer(Part2),
	{ writeln(Part1), writeln(Part2), move_after_comma(Part2,Part2Dec), Float is Part1 + Part2Dec }.
	
move_after_comma(X,Y) :-
    Y is X / 10,
    Y < 1.
move_after_comma(X,Y) :-
    Z is X / 10,
    Z >= 1,
    move_after_comma(Z,Y).

digits([]) --> [].	
digits([D|Ds]) -->
	digit(D),
	digits(Ds).

digit(D) --> [D], { atom_codes('0123456789',Digits), member(D,Digits) }.

test_p1 :-
	ppfold_parse_seq_file('/opt/ppfold/barkeri_mtmB.seq',Folding),
	writeln(Folding).

test_p2 :-
	ppfold_parse_ct_file('/opt/ppfold/barkeri_mtmB.ct',Energy),
	writeln('energy: '),
	writeln(Energy).
	
test_p3 :-
	Gene = gene(blah, 2,20,'+',2,[sequence([a,g,c,t,a,g,c,t])]),
	write_fasta_file(Gene,File),
	writeln(File).
	
test_p4 :-
	Gene = gene(blah, 2,20,'+',2,[sequence([a,g,c,t,a,g,c,t])]),
	write_fasta_file(Gene,File),
	ppfold_run(File,Folding,Energy),
	write('folding: '), writeln(Folding),
	write('energy: '), writeln(Energy).


