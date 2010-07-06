% Test script for working with prism_parallel
%

:- ['../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(misc_utils).
:- lost_include_api(prism_parallel).

run :-
        lost_sequence_file('U00096_ptt',File),
        run_model(parser_ptt,
                  annotate([File],[],ParsedPTT)),
        lost_tmp_directory(Tmp),
        atom_concat(Tmp,'test_parallel',Prefix),
        split_file(ParsedPTT,1000,Prefix,'.pl',InFileParts),
        write(InFileParts),nl,
        map(out_file_name,InFileParts,OutFileParts),
        write(OutFileParts),nl,
        zip(InFileParts,OutFileParts,InOutFilePairs),
        write(InOutFilePairs),nl,
        map(create_prism_goals,InOutFilePairs,FilePartGoals),
        prism_parallel(FilePartGoals),
        run_model(merge_files,
                  annotate(OutFileParts,[],MergedFile)),
        write('Merged files after processing them in parallel: '),
        write(MergedFile),nl.

out_file_name(InFile,OutFile) :-
        atom_concat(InFile,'.processed',OutFile).

create_prism_goals([InFilePart,OutFilePart],Goal) :-
        Goal = (
                ['../lost.pl'],
                lost_include_api(io),
                terms_from_file(InFilePart,Terms),
                tell(OutFilePart),
                forall(member(Term,Terms),(sleep(1),writeq(wrapped(Term)),write('.'),nl)),
                told
        ).
