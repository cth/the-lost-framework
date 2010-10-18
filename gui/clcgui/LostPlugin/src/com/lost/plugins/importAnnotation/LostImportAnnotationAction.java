package com.lost.plugins.importAnnotation;

import com.clcbio.api.base.algorithm.Algo;
import com.clcbio.api.base.algorithm.parameter.AlgoParameters;
import com.clcbio.api.free.actions.framework.ActionGroup;
import com.clcbio.api.free.algorithm.AlgoAction;
import com.clcbio.api.free.algorithm.wizard.AlgoWizardStepModel;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.NucleotideSequence;
import com.clcbio.api.free.gui.components.MultiSelectClassRestrictor;
import com.clcbio.api.free.gui.components.MultiSelectRestrictor;
import com.clcbio.api.free.gui.components.TypeRestrictor;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepModel;
import com.clcbio.api.free.workbench.WorkbenchManager;
import com.lost.plugins.runModel.LostModelView;

public class LostImportAnnotationAction extends AlgoAction {
    private static final long serialVersionUID = -8435791175828364350L;
    public static final String PLUGIN_GROUP = "free";

    public Algo createAlgo() {
        return new LostImportAnnotationAlgo(getManager());
    }
    
    @Override
    public void init(WorkbenchManager manager) {
        super.init(manager);
    }
    
    @Override
    protected void addToActionGroup() {
        ActionGroup g = manager.getActionManager().findActionGroup("actiongroup.lost");
        if (g != null) {
        	g.addAction(this);
        }  
    }
    
    @Override
    public String getName() {
    	return "Import LoSt annotation file";
    }
    
    @Override
    public String getClassKey() {
    	return "lost_import_annotation_action";
    }
    
    public ClcWizardStepModel getFirstStep(AlgoParameters defaultAlgoParameters, ClcWizardStepModel nextStep) {
        AlgoWizardStepModel step = new AlgoWizardStepModel(defaultAlgoParameters, nextStep) {
        	 public String getShortDescription() {
                 return "Select LoSt annotation to import";
             }

             public String getUniqueStepID() {
                 return "SelectLostAnnotation";
             }

         };
         step.setView(new LostImportAnnotationView(step, getManager()));
         return step;
    }

    
    @Override
    public MultiSelectRestrictor createRestrictor(WarningReceptor warningReceptor) {
        return new MultiSelectClassRestrictor(new Class[] { NucleotideSequence.class},
                "Select a sequence", TypeRestrictor.TEMP_RESTRICTORICON, MultiSelectClassRestrictor.SINGLE);
    }
    
    @Override
    protected String getHelpPropertyID() {
    	return "Imports a LoSt annotation";
    }    
}