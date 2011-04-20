package com.lost.plugins;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Map;

import com.clcbio.api.free.datatypes.bioinformatics.sequence.BasicSequence;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.region.Region;
import com.lost.plugins.runModel.LostModelOption;

public class LostAnnotationModel {
	private static String LOST_FRAMEWORK_PATH = "/Users/cth/code/lost/";
	private static String PRISM_BIN_PATH =  "/opt/prism/bin/prism";
	
	public ArrayList<LostAnnotation> run_lost_model(String modelName, ArrayList<LostModelOption> options, BasicSequence sequence) {
		File outputFile = null;
		File inputFile = null;
		
		try {
			inputFile = File.createTempFile("lost_plugin_input_", ".pl");
			outputFile = File.createTempFile("lost_plugin_output_", ".pl");
			
			// Write the sequence to the input file in prolog fact format
			String prologInputData = PrologDataHelper.sequenceToPrologData(sequence, 32);

			BufferedWriter ws = new BufferedWriter(new FileWriter(inputFile));
			ws.write(prologInputData);
			ws.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		// Build options
		StringBuffer optionsBuffer = new StringBuffer();
		if (options.size() == 0) {
			optionsBuffer.append("_");
		} else {
			for(int i = 0; i < options.size(); i++) {
				LostModelOption option = options.get(i);
				optionsBuffer.append(option.toPrologString());
				if (i != options.size() - 1)
					optionsBuffer.append(",");
			}
		}
		
		String prismGoals = "cl('" + modelInterfaceFile(modelName) + "'), " + 
							"lost_best_annotation(['" + inputFile.getAbsolutePath() + "']," +
							optionsBuffer.toString() + 
							", '" + 
							outputFile.getAbsolutePath() + "').";
			
		 ProcessBuilder pb = new ProcessBuilder("/usr/local/bin/prism-noio", prismGoals);
		 Map<String, String> env = pb.environment();
		 env.put("TERM", "xterm-color");
		 pb.directory(new File("/Users/cth/code/lost/models/test/"));
		 try {
			 String line;			 
			 Process p = pb.start();
			 BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
			 while((line = br.readLine()) != null) {
				 System.err.println(line);
			 }
			 br = new BufferedReader(new InputStreamReader(p.getErrorStream()));
			 while((line = br.readLine()) != null) {
				 System.err.println(line);
			 }
			 p.waitFor();
		 } catch (IOException e) {
			 e.printStackTrace();
		 } catch (InterruptedException e) {
			 e.printStackTrace();
		 }
		 
		return PrologDataHelper.parseLostAnntationFile(outputFile.getAbsolutePath());
	}
	
	private String modelWorkingDirectory(String modelName) {
		return LOST_FRAMEWORK_PATH + "models/" + modelName;
	}
	
	private String modelInterfaceFile(String modelName) {
		return modelWorkingDirectory(modelName) +  "/interface.pl";
	}
}
