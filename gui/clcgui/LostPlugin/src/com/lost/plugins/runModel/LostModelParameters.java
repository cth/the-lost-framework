package com.lost.plugins.runModel;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.regex.Pattern;

import com.clcbio.api.base.algorithm.parameter.AlgoParameters;
import com.clcbio.api.base.algorithm.parameter.AlgoParametersInterpreter;

public class LostModelParameters extends AlgoParametersInterpreter {
	private static final String MODEL_NAME = "Name of the model to run";
	private static final String DUMMY_PARAM = "Just some random parameter";
	private static final String LOST_MODEL_OPTION_KEYS = "A ':' separated list of keys for options";
	private Set<String> KEYS;

	public LostModelParameters(AlgoParameters params)  {
		super(params);
	}
	
	public void setDummyParam(int dummyParam) {
		putInt(DUMMY_PARAM,dummyParam);
	}
	
	public void setModelName(String modelName) {
		putString(MODEL_NAME, modelName);
	}

	public void setModelOptions(ArrayList<LostModelOption> modelOptions) {
		StringBuffer keys = new StringBuffer();
		LostModelOption option = null;
		
//		for(LostModelOption option : modelOptions) {
		for (int i = 0; i < modelOptions.size(); i++) {
			option = modelOptions.get(i);
			
			// Add option to persistent AlgoParameters
			putString(option.getOptionName(), option.getValue());

			// Build a string with option keys, separated by ":"
			keys.append(option.getOptionName());
			if (i != modelOptions.size()-1)
				keys.append(":");
		}
		
		// Finally, put the keys
		putString(LOST_MODEL_OPTION_KEYS, keys.toString());
	}
	
	public ArrayList<LostModelOption> getModelOptions() {
		ArrayList<LostModelOption> lostModelOptions = new ArrayList<LostModelOption>();
		
		String lostModelOptionKeys;
		lostModelOptionKeys = getString(LOST_MODEL_OPTION_KEYS);
		
		if (lostModelOptionKeys != null) {
			Pattern p = Pattern.compile(":");
			String[] keys = p.split(lostModelOptionKeys);
			for(String key : keys) {
				String value = getString(key);
				lostModelOptions.add(new LostModelOption(key,value));
			}
		}
		
		return lostModelOptions;
	}
	
	public int getDummyParam() {
		return getInt(DUMMY_PARAM);
	}
	
	public String getModelName() {
		return getString(MODEL_NAME);
	}
	
	@Override
	public Set<String> getKeys() {
		if (KEYS == null) {
			KEYS = new LinkedHashSet<String>();
			KEYS.add(DUMMY_PARAM);
		}
		return KEYS;
	}
	
	@Override
	public void setToDefault() {
		setDummyParam(1);
	}
}
