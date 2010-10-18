package com.lost.plugins;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Map;
import java.util.regex.*;

import org.alexgruenstein.javaprolog.Parser;
import org.alexgruenstein.javaprolog.Term;

import com.clcbio.api.free.datatypes.bioinformatics.sequence.BasicSequence;
import com.lost.plugins.runModel.LostModelOption;

public class LostFrameworkInterface {
	private String lost_framework_path;
	private String prism_bin_path;
	private Pattern prolog_sequence_format_pattern = Pattern.compile("text\\(prolog\\(sequence.*");
	private Pattern fasta_sequence_format_pattern = Pattern.compile("text\\(fasta.*");
	
	public LostFrameworkInterface() {
		// FIXME: These should be configurable
		// Maybe implement as preference plugin if possible
		lost_framework_path = "/Users/cth/code/lost/";
		prism_bin_path = "/opt/prism2/bin/prism";
	}
	
	/**
	 * Check if an inputFormat is supported
	 * @param inputFormat
	 * @return true if models with input format of type inputFormat are supported
	 */
	public boolean isSupportedInputFormat(String inputFormat) {
		Matcher prolog_sequence_format_matcher = prolog_sequence_format_pattern.matcher(inputFormat);
		Matcher fasta_sequence_format_matcher = fasta_sequence_format_pattern.matcher(inputFormat);
		return (prolog_sequence_format_matcher.matches() || fasta_sequence_format_matcher.matches());
	}
	
	/**
	 * Runs a LoSt model. 
	 * @param The name of model to run.
	 * @param A list of options for running this model.
	 * @param The sequence to run the model on.
	 * @return A list of LostAnnotation objects.
	 */
	public ArrayList<LostAnnotation> run_lost_model(String modelName, ArrayList<LostModelOption> options, BasicSequence sequence) {
		File outputFile = null;
		File inputFile = null;
		
		try {
			inputFile = File.createTempFile("run_lost_model_input_", ".pl");
			outputFile = File.createTempFile("run_lost_model_output_", ".pl");
			
			String modelInputFormat = lostModelInputFormats(modelName).get(0);
			
			Matcher prolog_sequence_format_matcher = prolog_sequence_format_pattern.matcher(modelInputFormat);
			Matcher fasta_sequence_format_matcher = fasta_sequence_format_pattern.matcher(modelInputFormat);

			String inputData = null;			
			if (prolog_sequence_format_matcher.matches()) {
				inputData = PrologDataHelper.sequenceToPrologData(sequence, 32);
			} else if (fasta_sequence_format_matcher.matches()) {
				FastaExport fasta = new FastaExport();
				inputData = fasta.fastaFromSequence(sequence);
			}
			
			BufferedWriter ws = new BufferedWriter(new FileWriter(inputFile));
			ws.write(inputData);
			ws.close();

			// Build options
			StringBuffer optionsBuffer = new StringBuffer();
			optionsBuffer.append("[");
			for(int i = 0; i < options.size(); i++) {
				LostModelOption option = options.get(i);
				optionsBuffer.append(option.toPrologString());
				if (i != options.size() - 1)
					optionsBuffer.append(",");
			}
			optionsBuffer.append("]");

			String prismGoals = "cl('" + modelInterfaceFile(modelName) + "'), " + 
			"lost_best_annotation(['" + inputFile.getAbsolutePath() + "']," +
			optionsBuffer.toString() + 
			", '" + 
			outputFile.getAbsolutePath() + "').";

			runPrism2(modelWorkingDirectory(modelName), prismGoals);

			return PrologDataHelper.parseLostAnntationFile(outputFile.getAbsolutePath());		
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}			
	}
	
	
	/**
	 * Queries the lost framework for models.
	 * @return A list of strings with model names.
	 */
	public ArrayList<String> lostModels() {
		// Run prism to get lost models

		File outputFile;
		try {
			outputFile = File.createTempFile("lost_list_models", ".pl");

			String prismGoals = "cl('lost.pl'), " + 
			"lost_include_api(interface), " +
			"list_lost_models_to_file('" +
			outputFile.getAbsolutePath() + "').";
			
			System.err.println("goals: " + prismGoals);

			runPrism2(lost_framework_path, prismGoals);

			FileInputStream fstream = new FileInputStream(outputFile.getAbsolutePath());

			BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(fstream)));
			String prologOutput = br.readLine();

			Parser p = new Parser();
			Term t = p.parseTerm(prologOutput);
			ArrayList<Term> modelNameTerms = new ArrayList<Term>();
			PrologDataHelper.prologListTermsToArrayList(modelNameTerms,t.getTerm(1));
			ArrayList<String> modelNames = new ArrayList<String>();
			for (Term modelNameTerm : modelNameTerms)
				modelNames.add(modelNameTerm.toString());
				
			return modelNames;

		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public ArrayList<LostModelOption> lostModelOptions(String model) {
		try {
			ArrayList<LostModelOption> lostModelOptions = new ArrayList<LostModelOption>();
			
			File outputFile = File.createTempFile("lost_list_model_options", ".pl");

			String prismGoals = "cl('lost.pl'), " + 
			"lost_include_api(interface), " +
			"list_lost_model_options_to_file(" + 
			model + "," +
			"lost_best_annotation" + "," +
			"'" + outputFile.getAbsolutePath() + "').";

			runPrism2(lost_framework_path, prismGoals);

			FileInputStream fstream = new FileInputStream(outputFile.getAbsolutePath());
			BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(fstream)));

			String str;
			while(null != (str = br.readLine()))
				lostModelOptions.add(new LostModelOption(str));

			return lostModelOptions;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public ArrayList<String> lostModelOptionValues(String model, String option) {
		try {
			File outputFile = File.createTempFile("lost_list_model_option_values", ".pl");

			String prismGoals = "cl('lost.pl'), " + 
			"lost_include_api(interface), " +
			"lost_model_option_values_to_file(" + 
			model + "," +
			"lost_best_annotation," +
			option + "," +
			"'" + outputFile.getAbsolutePath() + "').";

			runPrism2(lost_framework_path, prismGoals);

			FileInputStream fstream = new FileInputStream(outputFile.getAbsolutePath());
			BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(fstream)));

			// If there is no lost_option_values for the given model/option combination,
			// then the file will not exist after this call
			String str;
			ArrayList<String> optionValues = new ArrayList<String>();
			
			while(null != (str = br.readLine())) {
				Parser p = new Parser();
				Term t = p.parseTerm(str);
				Term prologList = t.getTerm(4);
				ArrayList<Term> optionListTerms = new ArrayList<Term>();
				PrologDataHelper.prologListTermsToArrayList(optionListTerms, prologList);
				for(Term optionTerm : optionListTerms) 
					optionValues.add(optionTerm.toString());
			}
			
			System.out.println("Printing option values: ");
			for(String optionValue : optionValues)
				System.out.println("Option value: " + optionValue);
			
			if (optionValues.size() > 0)
				return optionValues;
			else
				return null;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public String lostModelOutputFormat(String model, ArrayList<LostModelOption> options) {
		try {
			File outputFile = File.createTempFile("lost_list_model_output_format", ".pl");

			// Build options
			StringBuffer optionsBuffer = new StringBuffer();
			optionsBuffer.append("[");
			for(int i = 0; i < options.size(); i++) {
				LostModelOption option = options.get(i);
				optionsBuffer.append(option.toPrologString());
				if (i != options.size() - 1)
					optionsBuffer.append(",");
			}
			optionsBuffer.append("]");
			
			String prismGoals = "cl('lost.pl'), " +
				"lost_interface_output_format_to_file(" +
				model + "," +
				"annotate," +
				optionsBuffer.toString() + "," + 
				"'" + outputFile.getAbsolutePath() + "').";

			runPrism2(lost_framework_path, prismGoals);

			System.out.println("File path: " + outputFile.getAbsolutePath());
			
			FileInputStream fstream = new FileInputStream(outputFile.getAbsolutePath());
			BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(fstream)));
			
			String prologString = br.readLine();
			if (prologString == null)
				System.out.println("could not read anything from output format file..");
			else
				System.out.println(prologString);
			
			Parser p = new Parser();
			Term prologTerm = p.parseTerm(prologString);
			
			return prologTerm.getTerm(4).toString();
			
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public ArrayList<String> lostModelInputFormats(String model) {
		try {
			File outputFile = File.createTempFile("lost_list_model_output_format", ".pl");

			String prismGoals = "cl('lost.pl'), " + 
//			"lost_include_api(interface), " +
			"lost_model_input_formats_to_file(" +
			model + "," +
			"annotate" + "," +
			"'" + outputFile.getAbsolutePath() + "').";

			runPrism2(lost_framework_path, prismGoals);

			FileInputStream fstream = new FileInputStream(outputFile.getAbsolutePath());
			BufferedReader br = new BufferedReader(new InputStreamReader(new DataInputStream(fstream)));
			
			String prologString = br.readLine();
			
			Parser p = new Parser();
			Term prologTerm = p.parseTerm(prologString);
			
			Term inputFormatPrologList = prologTerm.getTerm(3);
			ArrayList<Term> inputFormatList = new ArrayList<Term>();
			PrologDataHelper.prologListTermsToArrayList(inputFormatList, inputFormatPrologList);
			
			ArrayList<String> inputFormats = new ArrayList<String>();
			for (Term t : inputFormatList) 
				inputFormats.add(t.toString());
			
			return inputFormats;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Create a temporary file and write some PRISM goals to this file.
	 * @param goals
	 * @return A File object representing the file created
	 */
	private File writeGoalsToFile(String goals) {
		File inputFile;
		try {
			inputFile = File.createTempFile("prism_goals_file", ".psm");

			BufferedWriter ws = new BufferedWriter(new FileWriter(inputFile));
			ws.write(goals);
			ws.close();
			return inputFile;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}
 	
	private String modelWorkingDirectory(String modelName) {
		return lost_framework_path + "models/" + modelName;
	}
	
	private String modelInterfaceFile(String modelName) {
		return modelWorkingDirectory(modelName) +  "/interface.pl";
	}
	
	public void runPrism2( String directory, String goals) {
		StringBuffer stdOutBuf = new StringBuffer();
		StringBuffer stdErrBuf = new StringBuffer();
		runPrism2(directory,goals,stdOutBuf,stdErrBuf);
		System.out.println("runPrism2(" + directory + "," + goals + "): begin");
		System.err.println("std error buffer:");
		System.err.println(stdOutBuf);
		System.err.println("std out buffer:");
		System.err.println(stdErrBuf);
		System.err.println("runPrism2: done.");
	}
	
	/**
	 * Runs a PRISM process. 
	 * @param directory determines where the PRISM process is started, and hence the working directory of the PRISM model.
	 * @param goals A string containing the goals to be executed by the PRISM process.
	 * @param outputBuffer Collects the output sent to stdout.
	 * @param errorBuffer  Collects the output sent to stderr.
	 */
	public void runPrism2(	String directory, 
							String goals,
							StringBuffer outputBuffer,
							StringBuffer errorBuffer) {
		File goalsFile = writeGoalsToFile(goals);
	
		// -l runs PRISM in "batch mode"
		 ProcessBuilder pb = new ProcessBuilder(prism_bin_path, "-l");
		 Map<String, String> env = pb.environment();
		 pb.directory(new File(directory));
		 try {
			 String line;
			 Process p = pb.start();

			 BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
			 bw.write(goals + "\n");
			 bw.close();
			 
			 
			 StreamGobbler outGobbler = new StreamGobbler(p.getInputStream(), "OUT");
			 StreamGobbler errGobbler = new StreamGobbler(p.getErrorStream(), "ERR");
			 
			 int exitcode = p.waitFor();
			 System.err.println("PRISM exited with code: " + exitcode);
		 } catch (IOException e) {
			 e.printStackTrace();
		 } catch (InterruptedException e) {
			 e.printStackTrace();
		 }
	}
}
