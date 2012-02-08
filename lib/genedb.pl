:- module(genedb,[gene_sequence_id/2,gene_left/2,gene_right/2,gene_strand/2,gene_frame/2,gene_extra_field/3,genedb_distinct_predictions/3,genedb_distinct_stop_codons/2,genedb_predictions_for_stop_codon/3]).

/** <module> working with text(prolog(ranges(_))) files

This library contains utility predicates for working with the =|text(prolog(ranges(_)))|= format.
This format, which is descibed in detail in the manuals section on file formats contain
facts, 
==
gene(SequenceId,Left,Right,Strand,Frame,Extra)
==
Note that the functor 'gene' may vary. 
=|Left|= and =|Right|= are integers indicating the left and right position of the gene in the genome.
They are both inclusive. =|Strand|=, which is either '+' or '-' indicates whether the gene is located 
on the direct or reverse strand. =|Frame|= is an integer in [1,2,3,4,5,6] which indicates the reading
frame of the gene. =|Extra|= is a list of key/value pairs, e.g.
==
[product('hypothetical protein'), cid(12351)]
==
The format does not dictate which key/value pairs are required, so this may vary depending on the 
application and how the database was generated.

@author: Christian Theil Have

*/

%% gene_valid(+GeneRecord)
% True if GeneRecord is a proper gene record term
gene_valid(GeneRecord) :-
	GeneRecord =.. [ _PredFunctor, SequenceId,Left,Right,Strand,Frame,Extra],
	atom(SequenceId),
	integer(Left),
	integer(Right),
	member(Strand, ['+','-']),
	member(Frame, [1,2,3]), % FIXME: Should alsp check that this corresponds to Left/Right Position
	forall(member(ExtraField,Extra), ExtraField =.. [ _key, _value ]).
	
%% gene_create(+SeqId,+Left,+Right,+Strand,-GeneRecord)
% Create a new GeneRecord with specified data
gene_create(SeqId,Left,Right,Strand,GeneRecord) :-
	Frame is 1 + (Left mod 3),
	GeneRecord =.. [ seq, SeqId, Left, Right, Strand, Frame, []].

%% gene_sequence_id(+GeneRecord,-SequenceId) is det
% Extract the SequenceId field from a GeneRecord
gene_sequence_id(GeneRecord,SequenceId) :-
        GeneRecord =.. [ _PredFunctor, SequenceId,_Left,_Right,_Strand,_Frame,_Extra].

%% gene_left(+GeneRecord,-Left) is det
% Extract the Left position of GeneRecord
gene_left(GeneRecord,Left) :-
        GeneRecord =.. [ _PredFunctor, _SequenceId,Left,_Right,_Strand,_Frame,_Extra].

%% gene_right(+GeneRecord,-Right) is det
% Extract the Right position of GeneRecord
gene_right(GeneRecord,Right) :-
        GeneRecord =.. [ _PredFunctor, _SequenceId,_Left,Right,_Strand,_Frame,_Extra].

%% gene_start_codon(+GeneRecord,-Start) is det
% Extract the position of the first base of the start codon of the gene given by GeneRecord
gene_start_codon(GeneRecord,Start) :-
	(gene_strand(GeneRecord,'+') ->
		gene_left(GeneRecord,Start)
		;
		gene_right(GeneRecord,Start)
	).

%% gene_stop_codon(+GeneRecord,-Start) is det
% Extract the position of the first base of the stop codon of the gene given by GeneRecord
gene_stop_codon(GeneRecord,Stop) :-
	(gene_strand(GeneRecord,'+') ->
		gene_right(GeneRecord,Right),
		Stop is Right - 2
		;
		gene_left(GeneRecord,Left),
		Stop is Left + 2
	).

%% gene_strand(+GeneRecord,-Strand) is det
% Extract the Strand (+,-) from a GeneRecord
gene_strand(GeneRecord,Strand) :-
        GeneRecord =.. [ _PredFunctor, _SequenceId,_Left,_Right,Strand,_Frame,_Extra].

%% gene_frame(+GeneRecord,-Frame) is det
% Extract the Frame from a GeneRecord
gene_frame(GeneRecord,Frame) :-
        GeneRecord =.. [ _PredFunctor, _SequenceId,_Left,_Right,_Strand,Frame,_Extra].

%% gene_extra_field(+GeneRecord,+Key,-Value)
% Extract the Value of the extra field which has Key as its functor.
gene_extra_field(GeneRecord,Key,Value) :-
	GeneRecord =.. [_,_,_,_,_,_,Extra],
	Matcher =.. [ Key, Value ], 
	member(Matcher,Extra).

%% gene_add_extra_field(+GeneRecord,+Key,+Value,-UpdatedGeneRecord) is det
% Adds an additional extra field to a GeneRecord 
gene_add_extra_field(GeneRecord,Key,Value,UpdatedGeneRecord) :-
	GeneRecord =.. [ PredFunctor, SequenceId,Left,Right,Strand,Frame,Extra],
	NewExtraField =.. [ Key, Value ],
	UpdatedGeneRecord =.. [ PredFunctor, SequenceId,Left,Right,Strand,Frame,[NewExtraField|Extra]].

%% genedb_distinct_predictions(+GeneDBFunctor,GeneEnds,PredictionsForEnd)
% Find all distinct predictions in a _consulted_ genedb:
% GeneEnds is a list of stop codon positions for the predictions 
% and PredictionsForEnd is a list of lists. The list has the same length 
% as GeneEnds and each element corresponds to to an element in GeneEnds.
% The elements of the list are lists of predictions in 
% list predictions for the parstop codon 
genedb_distinct_predictions(_,[],[]).

genedb_distinct_predictions(PredFunctor,[DistinctEnd|RestEnds],[PredictionsForEnd|RestPred]) :-
	genedb_predictions_for_stop_codon(PredFunctor,DistinctEnd,PredictionsForEnd),
	genedb_distinct_predictions(PredFunctor,RestEnds,RestPred).

%% genedb_distinct_stop_codons(+GeneDBFunctor,-DistinctStops) is det
% DistinctStops is a list (a set) of all stop codons of predictions in the database.
genedb_distinct_stop_codons(PredFunctor,DistinctStops) :-
	ForwardStrand =.. [ PredFunctor, _id,_,StopCodonEnd,'+',Frame,_],
	ReverseStrand =.. [ PredFunctor, _id,StopCodonEnd,_,'-',Frame,_],
	findall([StopCodonEnd,'+',Frame], ForwardStrand, ForwardStops),
	findall([StopCodonEnd,'-',Frame], ReverseStrand, ReverseStops),
	append(ForwardStops,ReverseStops,AllStops),
	eliminate_duplicate(AllStops,DistinctStops).


%% genedb_predictions_for_stop_codon(+GeneDBFunctor,+StopMatchPattern,-Predictions)
% Finds all predictions that matches StopMatchPattern. StopMatchPattern is a list:
% ==
% [+StopCodonEnd,?Strand,?Frame]
% ==
% StopCodonEnd is position of the last nucleotide in the stop codon (i.e. right-most on direct strand and 
% left-most on reverse strand). Strand is '+' or '-' and frame is one of [1,2,3,4,5,6].
genedb_predictions_for_stop_codon(PredFunctor,[StopCodonEnd,'+',Frame],Predictions) :-
	FindGoal =.. [ PredFunctor, _,  Start,StopCodonEnd,'+',Frame,Extra],
	BuildGoal =.. [ PredFunctor, _, Start,StopCodonEnd,'+',Frame,Extra],
	findall(BuildGoal,FindGoal,Predictions).

genedb_predictions_for_stop_codon(PredFunctor,[StopCodonEnd,'-',Frame],Predictions) :-
	FindGoal =.. [ PredFunctor, _, StopCodonEnd,End,'-',Frame,Extra],
	BuildGoal =.. [ PredFunctor, _, StopCodonEnd,End,'-',Frame,Extra],
	findall(BuildGoal,FindGoal,Predictions).

