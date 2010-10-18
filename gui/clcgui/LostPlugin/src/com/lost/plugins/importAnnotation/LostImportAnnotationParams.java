package com.lost.plugins.importAnnotation;

import java.util.LinkedHashSet;
import java.util.Set;

import com.clcbio.api.base.algorithm.parameter.AlgoParameters;
import com.clcbio.api.base.algorithm.parameter.AlgoParametersInterpreter;

public class LostImportAnnotationParams extends AlgoParametersInterpreter {
	private static final String FILE_NAME = "Name of file to import";
	private Set<String> KEYS;

	public LostImportAnnotationParams(AlgoParameters params)  {
		super(params);
	}
	
	public void setFileName(String fileName) {
		putString(FILE_NAME, fileName);
	}
		
	public String getFileName() {
		return getString(FILE_NAME);
	}
	
	@Override
	public Set<String> getKeys() {
		if (KEYS == null) {
			KEYS = new LinkedHashSet<String>();
			KEYS.add(FILE_NAME);
		}
		return KEYS;
	}
	
	@Override
	public void setToDefault() {
		setFileName(null);
	}
}
