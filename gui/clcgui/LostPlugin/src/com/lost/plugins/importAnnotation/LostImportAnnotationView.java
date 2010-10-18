package com.lost.plugins.importAnnotation;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JTextField;

import com.clcbio.api.free.algorithm.wizard.AlgoWizardStepModel;
import com.clcbio.api.free.gui.StandardLayout;
import com.clcbio.api.free.gui.components.FileBrowser;
import com.clcbio.api.free.gui.components.FileChooserFactory;
import com.clcbio.api.free.gui.dialog.ClcMessages;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepLayout;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepModel;
import com.clcbio.api.free.wizard.dynamic.ClcWizardStepView;
import com.clcbio.api.free.workbench.WorkbenchManager;

public class LostImportAnnotationView extends ClcWizardStepView implements ActionListener  {
	private final AlgoWizardStepModel model;
	private final ClcWizardStepLayout panel;
	private WorkbenchManager manager;
	
	private StandardLayout selectFilePanel;
	private JFileChooser fileChooser;
	private JButton selectFileButton;
	
	private File selectedFile;
	private JLabel selectedFileLabel;
	
	
    public LostImportAnnotationView (ClcWizardStepModel model, WorkbenchManager manager) {
        super(model);
        this.model = (AlgoWizardStepModel) model;
        this.manager = manager;
        
        selectFileButton = new JButton("Select file");
        selectFileButton.addActionListener(this);
        
        selectedFileLabel = new JLabel();
       
        //FileBrowser fb = new FileBrowser(FileBrowser.SelectionMode.FILES_ONLY);
        
        selectFilePanel = new StandardLayout();
        selectFilePanel.addComp(selectedFileLabel);
        selectFilePanel.addComp(selectFileButton);
        
        fileChooser = FileChooserFactory.createJFileChooser();
        
        panel = new ClcWizardStepLayout();
        panel.addComponentGroup("LoSt annotation file", selectFilePanel);
    }
    
    public void actionPerformed(ActionEvent e) {
    	if (e.getSource() == selectFileButton) {
    		int returnVal = fileChooser.showOpenDialog(manager.getCurrentMainFrame());
    		if (returnVal == JFileChooser.APPROVE_OPTION) {
    			System.out.println("File selected: " + fileChooser.getSelectedFile());
    			selectedFile = fileChooser.getSelectedFile();
    			updateUI();
    		}
    	}
    }
    
    
    public boolean commit() {
    	if (panel != null) {
    		if (selectedFile == null) {
    			ClcMessages.showInformation(manager.getMainFrame(), "Please select a LoSt annotation file", "Error");
    			return false;
    		} else {
    			LostImportAnnotationParams p = new LostImportAnnotationParams(model.getParameters());
    			p.setFileName(selectedFile.getAbsolutePath());
    		}
        	return true;
    	} else 
    		return false;
    }
    
    @Override
    public JComponent getUI() {
        return panel;
    }
    
    @Override
    public void updateUI() {
    	if (selectedFile != null) {
    		selectedFileLabel.setText(selectedFile.getAbsolutePath());
    	}
    }

}
