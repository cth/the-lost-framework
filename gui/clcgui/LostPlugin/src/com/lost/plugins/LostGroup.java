package com.lost.plugins;

import com.clcbio.api.free.actions.framework.ActionGroupPlugin;
import com.clcbio.api.free.actions.framework.ActionGroup;
import com.clcbio.api.free.actions.framework.StaticActionGroupDefinitions;

public class LostGroup extends ActionGroupPlugin {
	public static final String PLUGIN_GROUP = "free";
	
	public String getClassKey() {
		return "actiongroup.lost";
	}
	
	public String getName() {
		return "LoSt";
	}
	
	public ActionGroup getParent() {
		return StaticActionGroupDefinitions.TOOLBOX_TOP_GROUP;
	}
	
	public ActionGroup.GroupType getType() {
		return ActionGroup.SUBMENUTYPE;
	}
	
	public double getVersion() {
		return 1.0;
	}
}