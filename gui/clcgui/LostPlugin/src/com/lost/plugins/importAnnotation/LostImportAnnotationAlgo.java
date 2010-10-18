package com.lost.plugins.importAnnotation;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import com.clcbio.api.base.algorithm.Algo;
import com.clcbio.api.base.algorithm.AlgoException;
import com.clcbio.api.base.algorithm.AlgoHistoryTools;
import com.clcbio.api.base.algorithm.CallableExecutor;
import com.clcbio.api.base.algorithm.OutputHandler;
import com.clcbio.api.base.algorithm.parameter.AlgoParameters;
import com.clcbio.api.base.persistence.PersistenceException;
import com.clcbio.api.base.process.ProgressMeasurer;
import com.clcbio.api.base.session.ApplicationContext;
import com.clcbio.api.free.datatypes.ClcObject;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.NucleotideSequence;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.alphabet.Alphabet;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.alphabet.AlphabetTools;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.feature.Feature;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.region.Region;
import com.lost.plugins.LostAnnotation;
import com.lost.plugins.LostFrameworkInterface;
import com.lost.plugins.PrologDataHelper;
import com.lost.plugins.runModel.LostModelOption;

public class LostImportAnnotationAlgo extends Algo {
	public LostImportAnnotationAlgo(ApplicationContext ap) {
		super(ap);
	}
	
	public AlgoParameters createNewDefaultParameterObject() {
		AlgoParameters algoParameters = super.createNewDefaultParameterObject();
		return algoParameters;
	}
	
	public void calculate(OutputHandler handler, CallableExecutor objectModificationExecutor)  throws AlgoException, InterruptedException {
		// Expect first input object to be a nucleotide sequence
		final NucleotideSequence sequence = (NucleotideSequence) getInputObjectsIterator().next();
		
		// Get the current parameters
		final LostImportAnnotationParams params = new LostImportAnnotationParams(getParameters());
		
		getProgressMeasurer().setCurrentActivity("Reading LoSt annotation file");
		
		final ArrayList<LostAnnotation> annotations =  PrologDataHelper.parseLostAnntationFile(params.getFileName());
		
		if (!annotations.isEmpty()) {
			 List<ClcObject> out = new ArrayList<ClcObject>();
	         out.add(sequence);
	         handler.postOutputObjects(out, this);
	         
	         getProgressMeasurer().setCurrentActivity("Adding annotations");
	         
	         objectModificationExecutor.execute(new Callable<Object>() {
	        	 public Object call() throws Exception {
	        		 sequence.startUndoAndEventBlock("Add lost annotations");
	        		 
		        	 for(LostAnnotation annotation : annotations) {
		        		 sequence.addFeature(annotation.toFeature());
		        		 
		        		 sequence.addHistory(AlgoHistoryTools.getHistoryEntry(sequence, LostImportAnnotationAlgo.this));  	 
		        	 }
		        	 sequence.endUndoAndEventBlock();
		        	 return null;
	        	 }
	         });
		} else
			handler.postMessage("No annotations added to sequence" + sequence.getName(), this);
	}

	@Override
	public String getName() {
		return "Import LoSt annotation";
	}
}
