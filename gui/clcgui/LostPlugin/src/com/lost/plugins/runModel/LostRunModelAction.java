package com.lost.plugins.runModel;

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

public class LostRunModelAction extends AlgoAction {
    private static final long serialVersionUID = -8435791176828364350L;
    public static final String PLUGIN_GROUP = "free";

    public Algo createAlgo() {
        return new LostAlgo(getManager());
    }
    
    @Override
    public void init(WorkbenchManager manager) {
        super.init(manager);
    }
    
    @Override
    protected void addToActionGroup() {
        //ActionGroup g = StaticActionGroupDefinitions.TOOLBOX_TOP_GROUP;
        ActionGroup g = manager.getActionManager().findActionGroup("actiongroup.lost");
//    	ActionGroup g = new LostGroup();
        if (g != null) {
        	g.addAction(this);
        }  
    }
    
    @Override
    public String getName() {
    	return "Run a LoSt model";
    }
    
    @Override
    public String getClassKey() {
    	return "run_lost_model_action";
    }
    
    public ClcWizardStepModel getFirstStep(AlgoParameters defaultAlgoParameters, ClcWizardStepModel nextStep) {
        AlgoWizardStepModel step = new AlgoWizardStepModel(defaultAlgoParameters, nextStep) {
        	 public String getShortDescription() {
                 return "Set LoSt model name and options";
             }

             public String getUniqueStepID() {
                 return "LostModelSelection";
             }

         };
         step.setView(new LostModelView(step, getManager()));
         return step;
    }

    
    @Override
    public MultiSelectRestrictor createRestrictor(WarningReceptor warningReceptor) {
        return new MultiSelectClassRestrictor(new Class[] { NucleotideSequence.class},
                "Select a sequence", TypeRestrictor.TEMP_RESTRICTORICON, MultiSelectClassRestrictor.SINGLE);
    }
    
    @Override
    protected String getHelpPropertyID() {
    	return "Runs a LoSt model";
    }    
}
