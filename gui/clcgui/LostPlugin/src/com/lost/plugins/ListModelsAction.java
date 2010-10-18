package com.lost.plugins;

import java.awt.event.ActionEvent;
import java.util.ArrayList;

import javax.swing.JComponent;

import com.clcbio.api.free.actions.framework.ClcAction;
import com.clcbio.api.free.actions.framework.StaticActionGroupDefinitions;
import com.clcbio.api.free.gui.dialog.ClcMessages;
import com.clcbio.api.free.gui.icon.ClcIcon;
import com.clcbio.api.free.workbench.WorkbenchManager;

import org.alexgruenstein.javaprolog.*;

public class ListModelsAction extends ClcAction {
	   private static final long serialVersionUID = 4222858950932875067L;
	   
	    public static String PLUGIN_GROUP = "free";

		@SuppressWarnings("unchecked")
		public boolean appliesTo(Class[] typeList)  {
			return true;
		}
		
		public void init(WorkbenchManager m) {
			super.init(m);
			StaticActionGroupDefinitions.TOOLBOX_TOP_GROUP.addAction(this);
		}
		
		public String getName() {
			return "List lost models";
		}
		
		public int getPreferredMenuLocation() {
			return 10;
		}
		
		public boolean isInToolBar() {
			return true;
		}
		
		public boolean isInMenu() {
			return true;
		}
		
		public double getVersion() {
			return 1.0;
		}
		
		public String getClassKey() {
			return "hello_world_action";
		}
		
		public void execute(ActionEvent e) {
			LostFrameworkInterface lfi = new LostFrameworkInterface();
			ArrayList<String> models = lfi.lostModels();
			for(String model : models) {
				System.err.println(model);
			}
			
		}
}
