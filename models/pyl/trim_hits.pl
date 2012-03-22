trim_sequence_identifier(InputFile,OutputFile) :-
	open(InputFile,read,IS),
	open(OutputFile,write,OS),
	trim_sequence_identifier_rec(IS,OS),
	close(IS),
	close(OS).
	
trim_sequence_identifier_rec(IS,OS) :-
	read(IS,Term),
	((Term == end_of_file) ->
		true
		;
		Term =.. [ Functor, SeqId | Rest ],
		extract_organism_name(SeqId,SeqIdTrim),		
		NewTerm =.. [ Functor, SeqIdTrim | Rest ],
		writeq(OS,NewTerm),
		write(OS,'.\n'),
		trim_sequence_identifier_rec(IS,OS)).
		

extract_organism_name(SeqId,JustName) :-
	atom_codes(SeqId,Codes),
	parse_name_pos(NameCodes,Codes,[]),!,
	atom_codes(JustName,NameCodes).

parse_name_pos(Name) -->
	name(Name),
	position.

name([]) --> [].
name([95|Xs]) --> " ", name(Xs). % Convert spaces to underscores
name([X|Xs]) --> [X], name(Xs).

position -->
	"_",
   digits,
   "_",
   digits,
   "_",
   strand,
   "_",
   frame.

digits --> [].
digits -->
   digit,
	digits.
	
digit --> "0" ; "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8" ; "9".
strand --> "+" ; "-".
frame --> "1" ; "2" ; "3".

remove_spaces(SeqId,SeqIdTrim) :-
	atom_codes(SeqId,Codes),
	delete(Codes,32,Codes1),
	atom_codes(SeqIdTrim,Codes1).
