:- ['../../../../lost.pl'].

:- use(genedb).
:- use(fasta).
:- use(dnaseq).

go1 :-
        go('pylisall.pl','pylisall.fasta').

go(InputFile,OutputFile) :-
        terms_from_file(InputFile,Terms),
        length(Terms,LT),
        writeln(LT),
        findall([SeqId,PylSeq],(member(Term,Terms), check_protein_hit(Term,PylSeq,SeqId)),PylSeqs),
        eliminate_duplicate(PylSeqs,UniqPylSeqs),
        length(PylSeqs,L1),
        length(UniqPylSeqs,L2),
        writeln(L1),
        writeln(L2),
        findall(Fasta, (member([SeqId,PylSeq],UniqPylSeqs), to_fasta(SeqId,PylSeq,Fasta)), FastaSeqs),
 %       eliminate_duplicate(FastaSeqs,UniqFastaSeqs),
        flatten(FastaSeqs,Codes),
        open(OutputFile,write,OS),
        foreach(Code in Codes, put_code(OS,Code)),
        close(OS).


pylis_seq(Hit,PylisSeq,LongSeqId) :-
        gene_extra_field(Hit,match_to,Orf),
        gene_extra_field(Orf,pylis_sequence,PylisSeq),
        gene_sequence_id(Orf,SeqId),
        gene_left(Orf,Left),
        gene_right(Orf,Right),
        gene_strand(Orf,Strand),
        gene_frame(Orf,Frame),
        remove_spaces(SeqId,SeqIdTrim),
        atom_concat_list([SeqIdTrim,'_', Left, '-' , Right, '_', Frame, '_', Strand], LongSeqId).

check_protein_hit(Hit,ProtSeq,LongSeqId) :-
        (protein_seq(Hit,ProtSeq,LongSeqId)  -> true ; throw(error(Hit))).
        

protein_seq(Hit,ProtSeq,LongSeqId) :-
        gene_extra_field(Hit,match_to,Orf),
        %gene_extra_field(Hit,hseq,ProtSeq), % Not to be trusted!
        gene_extra_field(Orf,pylis_sequence,PylisSeq),
        translate(11,PylisSeq,ProtSeq),
	gene_extra_field(Orf,in_frame_stops,[AmberPosition|_]),
	((Strand == '+') ->
		RealLeft = AmberPosition,
		RealRight is AmberPosition + 100
		;
		RealLeft is AmberPosition - 100,
		RealRight = AmberPosition
	),

        PylisLen is RealRight - RealLeft,
        %gene_extra_field(Orf,pylis_sequence,PylisSeq),
        gene_sequence_id(Hit,SeqId),
        gene_left(Hit,Left),
        gene_right(Hit,Right),
        gene_strand(Hit,Strand),
        gene_frame(Hit,Frame),
%        (first_name(SeqId,SeqIdTrim) -> true ; throw(error(SeqId))),
        (extract_organism_name(SeqId,SeqIdTrim) -> true ; throw(error(SeqId))),
        atom_concat_list([SeqIdTrim,'_', RealLeft, '-' , RealRight, '_', Frame, '_', Strand], LongSeqId).

first_name(SeqId,SeqIdTrim) :-
        atom_codes(SeqId,Codes),
        append(FirstName,[32|_],Codes),
        not(member(32,FirstName)),
        atom_codes(SeqIdTrim,FirstName).

extract_organism_name(SeqId,JustName) :-
        atom_codes(SeqId,Codes),
        parse_name_pos(NameCodes,Codes,[]),
        atom_codes(JustName,NameCodes).


test :-
        parse_name_pos(NameC,"Methanosalsum_zhilinae_DSM_4017_uid40771_1234113_1234415_+_1",[]),
        atom_codes(Name,NameC),
        writeln(Name).

%test2 :-
%        parse_name_pos(NameC,"Methanosalsum_zhilinae_DSM_4017_uid40771_1234113_1234415_+_1",[]),
%%        append(Name,["Methanosalsum_zhilinae_DSM_4017_uid40771_1234113_1234415_+_1")
        
test3 :-
        position("_1234113_1234415_+_1",[]).


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

