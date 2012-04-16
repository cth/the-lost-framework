%
% Pruning of clusters due to various rules
% 

prune_clusters(InFile,OutFile) :-
        open(InFile,read,InStream),
        open(OutFile,write,OutStream),
        prune_clusters_rec(InStream,OutStream),
        close(OutStream),
        close(InStream).


prune_clusters_rec(InStream,OutStream) :-
        read(InStream,Cluster),
        ((Cluster == end_of_file) ->
                true
                ;
                (is_feasible_cluster(Cluster) ->
                        writeq(OutStream,Cluster),
                        write(OutStream,'.\n')
                        ;
                        true),
                prune_clusters_rec(InStream,OutStream) 
                ).

is_feasible_cluster(Cluster) :-
        not(single_organism_zero_diversity(Cluster)).


single_organism_zero_diversity(cluster([Features,_Members])) :-
        member(diversity(D),Features),
        member(organisms(1),Features),
        D =< 0.

