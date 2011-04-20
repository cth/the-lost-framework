package com.lost.plugins.runModel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.regex.*;

import org.alexgruenstein.javaprolog.Parser;
import org.alexgruenstein.javaprolog.Term;

public class LostModelOption {
	private String optionGoal;
	private String optionName;
	private String defaultValue;
	private String value;
	private String helpText;
	
	public LostModelOption(String optionName, String value) {
		this.optionName = optionName;
		this.value = value;
	}

	public LostModelOption(String optionName, String defaultValue, String helpText) {
		this.optionName = optionName;
		this.defaultValue = defaultValue;
		this.helpText = helpText;
	}
	
	public LostModelOption(String prologTerm) {
		Parser prologParser = new Parser();
		Term t = prologParser.parseTerm(prologTerm);
		setOptionGoal(t.getTerm(1).toString());
		optionName = t.getTerm(2).toString();
		defaultValue = t.getTerm(3).toString();
		helpText = t.getTerm(4).toString();
	}
	
	public boolean valueIsNumeric() {
		Pattern p = Pattern.compile("^[0-9]\\.[0-9]$");
		Matcher m = p.matcher(getValue());
		return m.matches();
	}
	
	public boolean valueShouldBeQuoted() {
		Pattern number_pattern = Pattern.compile("^[0-9]\\.[0-9]$");
		Pattern atom_pattern = Pattern.compile("^[a-z]+.*$");
		Matcher number_matcher = number_pattern.matcher(getValue());
		Matcher atom_matcher = atom_pattern.matcher(getValue());
		return !(number_matcher.matches() || atom_matcher.matches());
	}
	
	public String toPrologString() {
		if (valueShouldBeQuoted())
			return optionName + "('" + getValue() + "')";
		else
			return optionName + "(" + getValue() + ")";
	}

	public String getOptionName() {
		return optionName;
	}

	public void setOptionName(String optionName) {
		this.optionName = optionName;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public String getValue() {
		if (value == null)
			return defaultValue;
		else
			return value;
	}

	public void setValue(String value) {
		if (value.equals(defaultValue)) 
			value = null;
		else
			this.value = value;
	}

	public String getHelpText() {
		return helpText;
	}

	public void setHelpText(String helpText) {
		this.helpText = helpText;
	}

	public void setOptionGoal(String optionGoal) {
		this.optionGoal = optionGoal;
	}

	public String getOptionGoal() {
		return optionGoal;
	}

}