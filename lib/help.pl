:- module(help, []).


/** <module> Model information and help

@author: Matthieu Petit

This library extracts information about models such as,
- what are the valid task of the model
- what options does a particular task predicate take/require
- what is the  format of the file produced by the task
- what input files and their formats does each task assume

@bug: (cth) I think this help library has become slightly outdated by recent developments in the framework. It should be updated!

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Launching Help for a model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% help_model(+Model)
%
% Help predicate for defined model of the framework. If Model iss not defined, help_model
% print out a list of defined models

help_model(Model) :-
        nl,
        write('#####################################################################'),nl,
        write('# Lost Framework Help                                               #'),nl,
        write('#####################################################################'),nl,
        nl,
        % Information collections
        lost_model_interface_file(Model,ModelFile),
        file_directory_name(ModelFile,Dirname),
        catch(terms_from_file(ModelFile,Terms),_,Error ='yes'),
        (Error == 'yes' ->
            write('This Model does not exist'),nl,
            write('List of models available:'),nl,
            print_available_model
        ;
            findall(Term,(member(Term,Terms),Term =.. [lost_input_formats,_InterfacePredicate,_Formats]),Term_Input),
            findall(Term,(member(Term,Terms),Term =.. [lost_output_format,_InterfacePredicate,_Options,_Formats]),Term_Output1),
            findall(Head,(member(Rule,Terms),Rule =.. [:-,Head,_],Head =.. [lost_output_format,_InterfacePredicate,_Options,_Formats]),Term_Output2),
            append(Term_Output1,Term_Output2,Term_Output),
            findall(Term,(member(Term,Terms),Term =.. [lost_option|_ParamsOpts]),Term_Option),
            help_information(Term_Input,Term_Output,Term_Option,Predicates),
                                % Printing information
            write('Model name: '), write(Model),nl,
            write('Model directory: '), write(Dirname),nl,
            nl,
            write('Defined predicates:'),nl,
            print_predicates_help(Predicates)
        ).


%% help_information(+Term_Input,+Term_Output,+Term_Option,-Predicates)
%
% Compute a list of informations for each defined predicate of the models

help_information(Term_Input,Term_Output,Term_Option,Predicates) :-
        findall(InterfacePredicate,member(lost_input_formats(InterfacePredicate,_Formats),Term_Input),Predicate_Defined1),
        findall(InterfacePredicate,member(lost_output_format(InterfacePredicate,_Options,_Formats),Term_Output),Predicate_Defined2),
        append(Predicate_Defined1,Predicate_Defined2,Predicate_Defined3),
        remove_dups(Predicate_Defined3,Predicate_Defined),
        help_information_rec(Predicate_Defined,Term_Input,Term_Output,Term_Option,Predicates).


help_information_rec([],_,_,_,[]) :-
        !.

help_information_rec([PredName|Rest],Term_Input,Term_Output,Term_Option,[Predicate|Rest_Predicates]) :-
        findall(Format_In,member(lost_input_formats(PredName,Format_In),Term_Input),Formats_In),
        findall(Format_Out,member(lost_output_format(PredName,_Options,Format_Out),Term_Output),Formats_Out),
        findall([OptName,Default,Description],member(lost_option(PredName,OptName,Default,Description),Term_Option),List_Options),
        Predicate =.. [PredName,Formats_In,List_Options,Formats_Out],
        help_information_rec(Rest,Term_Input,Term_Output,Term_Option,Rest_Predicates).

%% print_predicates_help(+Predicates)
%
% Predicate that writes information of the defined predicates on the standart output

print_predicates_help([]) :-
        !.

print_predicates_help([Predicates|Rest]) :-
        Predicates =.. [PredName,In_Format,Options,Out_Format],
        Sign =..[PredName,'InputFiles','Options','OutputFile'],
        write(Sign),nl,
        write('----------'),nl,
        (In_Format = [T|_] ->      % Assumption: Arity of the InputFile remains the same
            length(T,Size_In),
            write('number of input files: '),write(Size_In),nl
        ;
            write('number of InputFiles: not specified'),nl
        ),
        length(Options,Size_Options),
        write('number of options: '),write(Size_Options),nl,
        write('number of output file: 1'),nl, % TO MODIDY when 1 and more output files will be available
        write('----------'),nl,
        write('Format of the input file(s):'),nl,
        print_inout_format(In_Format),
        (Size_Options =\= 0 ->
            write('----------'),nl,
            write('Option(s) available:'),nl,
            print_options(Options)
        ;
            true
        ),
        write('----------'),nl,
        write('Format of the output file(s):'),nl,
        print_inout_format(Out_Format),
        nl,
        print_predicates_help(Rest).

% Utils print_predicates

print_inout_format([]) :-
        !.


print_inout_format([Elt|Rest]) :-
        is_list(Elt),
        !,
        print_inout_format(Elt),
        nl,
        print_inout_format(Rest).

print_inout_format([Elt|Rest]) :-
        var(Elt),
        !,
        write('format not specified'),nl,
        print_inout_format(Rest).


print_inout_format([Elt|Rest]) :-
        write(Elt),nl,
        print_inout_format(Rest).



print_options([]) :-
        !.

print_options([[OptName,Default,Description]|Rest]) :-
        !,
        write('name: '),write(OptName),nl,
        write('default value: '),write(Default),nl,
        write('description: '),write(Description),nl,
        nl,
        print_options(Rest).

%% print_available_model
%
% This predicate prints on the default output a list of available models
print_available_model :-
        lost_models_directory(ModelsDir),
        getcwd(Current),
        cd(ModelsDir),
        directory_files('.',Directories), % B-Prolog build-in
        subtract(Directories,['.','..'],Models),
        cd(Current),
        print_available_model_rec(Models).


print_available_model_rec([]) :-
        !.

print_available_model_rec([Name|Rest]) :-
        write(Name),nl,
        print_available_model_rec(Rest).
