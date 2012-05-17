
extract :-
	%File = '25_best_ranked.pl',
	File = 'current_clusters',
	open(File,read,IS),
	open('avg_upstream_uag.dat',write,AvgUpStream),
	open('avg_downstream_uag.dat',write,AvgDownStream),
	open('codon_score.dat',write,ScoreStream),
	open('diversity.dat',write,DiversityStream),
	open('organisms.dat',write,OrgStream),
	open('orf_lengths.dat',write,OrfStream),
	open('diversity_ratio.dat',write,RatioStream),
	open('syn_codons.dat',write,SynCodonStream),
	open('pmcomp.dat',write,PMCOMPStream),
	extract_rec(IS,AvgDownStream,AvgUpStream,ScoreStream,DiversityStream,OrgStream,OrfStream,RatioStream,SynCodonStream,PMCOMPStream),
	close(AvgDownStream),
	close(ScoreStream),
	close(DiversityStream),
	close(OrgStream),
	close(OrfStream),
	close(RatioStream),
	close(SynCodonStream),
	close(PMCOMPStream).

extract_rec(IS,AvgDownStream,AvgUpStream,ScoreStream,DiversityStream,OrgStream,OrfStream,RatioStream,SynCodonStream,PMCOMPStream) :-
	read(IS,Term),
	((Term == end_of_file) ->
		true
		;
		Term = cluster(Features,_Members),
		member(avg_upstream_uag(AvgUp),Features),
		member(avg_downstream_uag(AvgDown),Features),
		member(orf_length(OrfLength),Features),
		member(organisms(Organisms),Features),
		member(diversity(Div),Features),
		member(codon_score(Score),Features),
		member(diversity_ratio(Ratio),Features),
		member(syn_codons(SynCodons),Features),
		member(pmcomp(PMCOMP),Features),
		writeln(AvgDownStream,AvgDown),
		writeln(AvgUpStream,AvgUp),
		writeln(OrfStream,OrfLength),
		writeln(OrgStream,Organisms),
		writeln(DiversityStream,Div),
		writeln(ScoreStream,Score),
		writeln(RatioStream,Ratio),
		writeln(SynCodonStream,SynCodons),
		writeln(PMCOMPStream,PMCOMP),
		extract_rec(IS,AvgDownStream,AvgUpStream,ScoreStream,DiversityStream,OrgStream,OrfStream,RatioStream,SynCodonStream,PMCOMPStream)).

