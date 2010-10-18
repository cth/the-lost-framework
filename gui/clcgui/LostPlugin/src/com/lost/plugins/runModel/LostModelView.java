package com.lost.plugins.runModel;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTextField;

import com.clcbio.api.free.algorithm.wizard.AlgoWizardStepModel;
import com.clcbio.api.free.gui.StandardLayout;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepLayout;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepModel;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepView;
import com.clcbio.api.free.workbench.WorkbenchManager;
import com.lost.plugins.LostFrameworkInterface;
import com.sun.org.apache.bcel.internal.generic.LSTORE;

public class LostModelView extends ClcWizardStepView implements ActionListener {
	    private JLabel modelNameLabel;
	    private JComboBox modelNamesListBox;
	    private final AlgoWizardStepModel model;
	    private final ClcWizardStepLayout panel;
	    private WorkbenchManager manager;
	    private ArrayList<String> lostModelNames;
	    private String selectedModel;
	    private StandardLayout selectModelPanel;
	    private StandardLayout optionPanel;
	    private StandardLayout inputFormatPanel;
	    private StandardLayout outputFormatPanel;
	    private LostFrameworkInterface lost;
	    private ArrayList<LostModelOption> selectedModelOptions;
	    private JButton updateOutputFormatButton;

	    //private ArrayList<JTextField> selectModelOptionValues;
	    private ArrayList<Object> selectModelOptionValues;
	    
	    public LostModelView (ClcWizardStepModel model, WorkbenchManager manager) {
	        super(model);
	        this.model = (AlgoWizardStepModel) model;
	        this.manager = manager;
	        
	        selectedModel = null;
	        
	        createModelSelectionPanel();
	        createModelOptionsPanel();
	        createInputFormatPanel();
	        createOutputFormatPanel();

	        panel = new ClcWizardStepLayout();
	        panel.addComponentGroup("LoSt model", selectModelPanel);
	        panel.addComponentGroup("Model input format", inputFormatPanel);
	        panel.addComponentGroup("LoSt model options", optionPanel);
	        panel.addComponentGroup("Model output format", outputFormatPanel);
	        
	        //final LostModelParameters p = new LostModelParameters(LostModelView.this.model.getParameters());
	        	        
	        updateUI();
	    }
	    
	    private void createModelSelectionPanel() {
	        modelNameLabel = new JLabel("Name of LoSt model:");
	        modelNameLabel.setToolTipText("What LoSt mode do you wish to use to annotate this sequence");
	        //modelNameTextField = new JTextField(20);
	        
	        // Extract model names from lost framework
	        lost = new LostFrameworkInterface();
	        lostModelNames = lost.lostModels();
	        
	        modelNamesListBox = new JComboBox();
	        for(String modelName : lostModelNames)
	        	modelNamesListBox.addItem(modelName);
	        
	        modelNamesListBox.addActionListener(this);
	        
	        selectModelPanel = new StandardLayout();
	        selectModelPanel.addComp(modelNameLabel);
	        selectModelPanel.addComp(modelNamesListBox);
	    }
	    
	    private void createModelOptionsPanel() {
	    	updateOutputFormatButton = new JButton("update output format");
	    	updateOutputFormatButton.addActionListener(this);
	    	optionPanel = new StandardLayout();
	        updateOptionsPanel();
	    }
	    
	    private void createInputFormatPanel() {
	    	inputFormatPanel = new StandardLayout();
	    	updateInputFormatPanel();
	    }
	    
	    private void createOutputFormatPanel() {
	    	outputFormatPanel = new StandardLayout();
	    	updateOutputFormatPanel();
	    }
	    
	    public boolean commit() {
	        if (panel != null) {
	            LostModelParameters p = new LostModelParameters(model.getParameters());
	            p.setModelName(modelNamesListBox.getSelectedItem().toString());
	            updateSelectedModelOptions();
	            p.setModelOptions(selectedModelOptions);
	        }
	        return true;
	    }
	    
	    public void actionPerformed(ActionEvent e) {
	    	if (e.getSource() == modelNamesListBox) {
	    		selectedModel = (String) modelNamesListBox.getSelectedItem();
	    		System.err.println("New selection: " + selectedModel);
	    		updateUI();
	    	}

	    	if (e.getSource() == updateOutputFormatButton) {
	    		System.out.println("outputFormatButton pressed");
	    		updateOutputFormatPanel();
	    	}
	    }
	    
	    @Override
	    public void updateUI() {
            updateOptionsPanel();
            updateInputFormatPanel();
            updateOutputFormatPanel();
	    }
	    
	    public void updateOptionsPanel() {
    		// Make "empty" options box
    		optionPanel.removeAll();
    		
	    	if (selectedModel == null) {
	    		optionPanel.addComp(new JLabel("Need to select model before specifying options."));
	    	} else {
	    		selectedModelOptions = lost.lostModelOptions(selectedModel);
	    		//selectModelOptionValues = new ArrayList<JTextField>();
	    		selectModelOptionValues = new ArrayList<Object>();
	    		
	    		if (selectedModelOptions.size() == 0)
	    			optionPanel.addComp(new JLabel("There are no declared options for this model."));
	    		else {
	    			for (LostModelOption opt : selectedModelOptions) {
	    				String optionName = opt.getOptionName();
    					JLabel optionNameLbl = new JLabel(opt.getOptionName());
    					optionNameLbl.setToolTipText(opt.getHelpText());
    					optionPanel.addComp(optionNameLbl);    					
	    				ArrayList<String> optionValues = lost.lostModelOptionValues(selectedModel, optionName);
	    				if (optionValues != null) {
	    					JComboBox optionValue = new JComboBox();
	    					for(String optionValueString : optionValues)
	    						optionValue.addItem(optionValueString);
	    					optionValue.setSelectedItem(opt.getDefaultValue());
	    					optionValue.setToolTipText(opt.getHelpText());
	    					optionPanel.addComp(optionValue);
	    					selectModelOptionValues.add(optionValue);
	    				} else {
	    					JTextField optionValue = new JTextField(opt.getDefaultValue());
	    					optionValue.setToolTipText(opt.getHelpText());
	    					optionPanel.addComp(optionValue);
	    					selectModelOptionValues.add(optionValue);
	    				}
	    			}
	    			optionPanel.addComp(updateOutputFormatButton);
	    		}
	    	}
	    	
	    	optionPanel.updateUI();
	    }
	    
	    public void updateInputFormatPanel() {
	    	// Make "empty" input format panel
    		inputFormatPanel.removeAll();
	    		
    		if (selectedModel == null) {
				JLabel errMsg = new JLabel("No model selected.");
				errMsg.setForeground(Color.RED);
    			inputFormatPanel.addComp(errMsg);
    		} else {
    			ArrayList<String> modelInputFormats = lost.lostModelInputFormats(selectedModel);
    			if (modelInputFormats == null || modelInputFormats.size() == 0) {
    				JLabel l = new JLabel("Can not determine input format.");
    				l.setForeground(Color.RED);
    				inputFormatPanel.addComp(l);    				
      			} else if (modelInputFormats.size() == 1) {
    				JLabel l = new JLabel(modelInputFormats.get(0));
    				inputFormatPanel.addComp(l);
    				if (lost.isSupportedInputFormat(modelInputFormats.get(0)))
    					l.setForeground(Color.GREEN);
    				else
    					l.setForeground(Color.RED);
    			} else {
    				JLabel errMsg = new JLabel("Model requires multiple inputs: ");
    				errMsg.setForeground(Color.RED);
    				inputFormatPanel.addComp(errMsg);
    				for(String inputType : modelInputFormats) {
    					inputFormatPanel.addComp(new JLabel("- " + inputType));
    				}
    			}
    		}
	    }
	    
	    public void updateOutputFormatPanel() {
	    	outputFormatPanel.removeAll();
	    	
	    	if(selectedModel == null) {
				JLabel errMsg = new JLabel("No model selected.");
				errMsg.setForeground(Color.RED);
				outputFormatPanel.addComp(errMsg);
	    	} else {
	    		updateSelectedModelOptions();
	    		String modelOutputFormat = lost.lostModelOutputFormat(selectedModel,selectedModelOptions);
	    		JLabel outputFormatLabel;
	    		if (modelOutputFormat == null) {
	    			outputFormatLabel = new JLabel("Cannot determine output format.");
	    			outputFormatLabel.setForeground(Color.RED);
	    		} else if (modelOutputFormat.equals("text(prolog(ranges(gene)))")) {
	    			outputFormatLabel = new JLabel(modelOutputFormat);
	    			outputFormatLabel.setForeground(Color.GREEN);
	    		} else {
	    			outputFormatLabel = new JLabel(modelOutputFormat);	    			
	    			outputFormatLabel.setForeground(Color.RED);
	    		}
	    		outputFormatPanel.addComp(outputFormatLabel);
	    	}
	    }
	    
	    public void updateSelectedModelOptions() {
            if (selectModelOptionValues != null) {
            	for (int i = 0; i < selectModelOptionValues.size(); i++) {
            		Object o = selectModelOptionValues.get(i);
            		String value;
            		if (o instanceof JComboBox) {
            			JComboBox comboBox = (JComboBox)o;
            			value = (String)comboBox.getSelectedItem();
            			selectedModelOptions.get(i).setValue(value);
            		} else if (o instanceof JTextField) {
            			JTextField textField = (JTextField)o;
            			value = textField.getText();
            			selectedModelOptions.get(i).setValue(value);
            		}
            	}
            }
	    }
	    
	    @Override
	    public JComponent getUI() {
	        return panel;
	    }
}
