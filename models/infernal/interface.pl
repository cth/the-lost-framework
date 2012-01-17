:- use(lists).

:- [config].

infernal_cmsearch(cmsearch).

search([ModelFile,FastaFile],_Options,ResultsFile) :-
	infernal_cmsearch(CMSEARCH),
	atom_concat_list([CMSEARCH,' ', ModelFile, ' ', FastaFile, ' > ', ResultsFile],CMSearchCommand),
	system(CMSearchCommand).
	
calibrate([ModelFile],_Options,CalibratedModelFile) :-
	infernal_cmcalibrate(CMCAL),
	copy_file(ModelFile,CalibratedModelFile),
	atom_concat_list([CMCAL, ' ', CalibratedModelFile],CalibrateCommand),
	system(CalibrateCommand).
	
build([AlignmentFile],Options,ModelFile) :-
	infernal_cmbuild(CMBUILD),
	atom_concat_list([CMBUILD, ' ', ModelFile, ' ', AlignmentFile],BuildCommand),
	system(BuildCommand).