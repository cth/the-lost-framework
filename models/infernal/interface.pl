:- use(lists).
:- [config].

:- task(search([infernal(model),text(fasta)], [], infernal(results))).
:- task(calibrate([infernal(model)], [], infernal(model))).
:- task(build([infernal(alignment)],[],infernal(model))).

search([ModelFile,FastaFile],_Options,ResultsFile) :-
	infernal_cmsearch(CMSEARCH),
	atom_concat_list([CMSEARCH,' ', ModelFile, ' ', FastaFile, ' > ', ResultsFile],CMSearchCommand),
	write('running: '), writeln(CMSearchCommand),
	system(CMSearchCommand).
	
calibrate([ModelFile],_Options,CalibratedModelFile) :-
	infernal_cmcalibrate(CMCAL),
	copy_file(ModelFile,CalibratedModelFile),
	atom_concat_list([CMCAL, ' ', CalibratedModelFile],CalibrateCommand),
	system(CalibrateCommand).
	
build([AlignmentFile],_Options,ModelFile) :-
	infernal_cmbuild(CMBUILD),
	atom_concat_list([CMBUILD, ' ', ModelFile, ' ', AlignmentFile],BuildCommand),
	system(BuildCommand).
	
% TODO:
% parse([InfernalResultFile],_Options,OutputFile) :-
	
