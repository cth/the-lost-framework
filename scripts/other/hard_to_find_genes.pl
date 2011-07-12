% Produce a list of hard to find genes in the ecoli genome

:- ['../lost.pl'].
:- lost_include_api(interface).



minitest :-
	lost_sequence_file(gb_fragment,GBFile), % Genbank (reference genes)
	lost_sequence_file(parser_eg_U00096,EGFile), % Easygene predictions
	lost_sequence_file(parser_gm_U00096,GMFile), % Genemark predictions
	run_model(hard_to_find_genes,
	        annotate([GBFile,EGFile,GMFile], [], OutputFile)),
   	write('File written to: '), write(OutputFile), nl.


% This runs genemark and glimmer, but their options needs to be tweaked
% a bit still...
test1 :-
    lost_sequence_file('U00096_ptt',GenbankFile),
    run_model(parser_ptt, annotate([GenbankFile], [], GBFile)),
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(genemark,annotate([SeqFile],[parameters('Escherichia_coli_K12')],GMAllFile)),
	run_model(best_prediction_per_stop_codon,
  	        annotate([GMAllFile],
			 [prediction_functor(genemark_gene_prediction),score_functor(start_codon_probability)],
			 GMBestFile)),
	run_model(glimmer3,annotate([SeqFile],[mode(from-scratch)],GlimFile)),
        run_model(hard_to_find_genes,
			    annotate([GBFile,GMBestFile,GlimFile],
			    [],
			    Hard2FindGenesFile)),
	write('Find list of hard to find genes in: '), write(Hard2FindGenesFile),nl.




% script hard: Given a list of predictions files, a genome
% and a division, this script generates first a hard to find
% database and after that generates in separated files gene
% terms given the hard to find value. This separation is computed
% from the division variable.

script_hard([Golden_Standart|List_Predictions],Nucleotids_File,Division,Result_Files) :-
        map(lost_sequence_file,[Golden_Standart|List_Predictions],List_Files),
        lost_sequence_file(Nucleotids_File,Data_File),
	run_model(hard_to_find_genes, annotate(List_Files,[],OutputFile),
        consult(OutputFile),
        find_and_build_gene(Data_File,Division,Result_Files).

%----        
% Utils script hard          
%----

find_and_build_gene(_Data_File,[],[]) :-
        !.
        

find_and_build_gene(Data_File,[Range|Rest_Ranges],[Result_File|Rest_Files]) :-
        !,
        find_gene(Range,Data_File,Result_File),
        find_and_build_gene(Data_File,Rest_Ranges,Rest_Files).


% find gene        
find_gene((Range_Min,Range_Max),Data_File,File_Name) :-
        findall([Min,Max,Strand],(gene(Min,Max,Strand,_,Infos),
                                  member(gene_finding_difficulty_score(Value),Infos),
                                  in_range(Value,(Range_Min,Range_Max))
                                 ),
                Data_Infos),
        write(Data_Infos),nl,
        open(File_Name,write,Stream),
        build_data(Stream,Data_File,Data_Infos),
        close(Stream).


% in_range
in_range(V,(Min,Max)) :-
        Min =< V,
        V < Max,
        !.

in_range(_V,_Range) :-
        fail.


% build_data(++Stream,++Data)

build_data(_Stream,_Data_File,[]) :-
        !.
build_data(Stream,Data_File,[[Min,Max,Strand]|Rest_Data]) :-
        load_annotation_from_file(sequence,[range(Min,Max),data_position(4)],Data_File,Terms,Annotation),
        (Strand = + ->
            Annotation2 = Annotation
        ;
            reverse_strand(Annotation,Annotation2)
        ),
        Predicate =.. [hard_to_find,Min,Max,Strand,Annotation2],
        write([Min,Max,Strand]),nl,
        write(Stream,Predicate),write(Stream,'.'),
        nl(Stream),
        build_data_rec(Stream,Data_File,Rest_Data,Terms).


build_data_rec(_Stream,_Data_File,[],_) :-
        !.

build_data_rec(Stream,Data_File,[[Min,Max,Strand]|Rest_Data],Terms) :-
        load_annotation_from_file(sequence,[range(Min,Max),data_position(4)],Data_File,Terms,Annotation),
        (Strand = + ->
            Annotation2 = Annotation
        ;
            reverse_strand(Annotation,Annotation2)
        ),
        Predicate =.. [hard_to_find,Min,Max,Strand,Annotation2],
        write([Min,Max,Strand]),nl,
        write(Stream,Predicate),write(Stream,'.'),
        nl(Stream),
        build_data_rec(Stream,Data_File,Rest_Data,Terms).



% Reverse and complement a list of nucleotids.

reverse_strand([],[]) :-
        !.


reverse_strand(List,Result) :-
        reverse_strand_rec(List,[],Result).


reverse_strand_rec([],Result,Result) :-
        !.


reverse_strand_rec([Nuc|Rest_List],Partial_Result,Result) :-
        rev_nuc(Nuc,Nuc_Rev),
        reverse_strand_rec(Rest_List,[Nuc_Rev|Partial_Result],Result).


rev_nuc(a,t) :- !.
rev_nuc(t,a) :- !.
rev_nuc(c,g) :- !.
rev_nuc(g,c) :- !.
