package com.lost.plugins.export;

import com.clcbio.api.base.io.ClcFileFilter;
import com.clcbio.api.base.io.ExportPlugin;
import com.clcbio.api.base.persistence.PersistenceException;
import com.clcbio.api.free.datatypes.ClcObject;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.*;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.alphabet.Alphabet;
import com.lost.plugins.PrologDataHelper;

import java.io.BufferedOutputStream;
import java.io.IOException;


public class SequenceAsPrologExport extends ExportPlugin {
	public final static String 	PLUGIN_GROUP = "free";
	
	public final static int FACT_SIZE = 32;
	
	@SuppressWarnings("unchecked")
	public boolean canExport(Class[] objectTypes) {
		return (objectTypes != null && objectTypes.length == 1 && BasicSequence.class.isAssignableFrom(objectTypes[0]));
	}
	
    public ClcFileFilter createFileFilter() {
        return new ClcFileFilter(new String[] { "pl",}, "Prolog facts (.pl)");
    }
    
    @Override
	@SuppressWarnings("unchecked")
    public boolean doExport(ClcObject[] objects, BufferedOutputStream stream, java.util.Map properties) throws IOException, PersistenceException {
    	if (objects != null && objects.length == 1 && objects[0] instanceof BasicSequence) {
        	System.out.println("doExport");
        	BasicSequence sequence = (BasicSequence) objects[0];
        	String prologData = PrologDataHelper.sequenceToPrologData(sequence,FACT_SIZE);
        	stream.write(prologData.getBytes());
            stream.close();
            return true;
        }
        return false;
    }
    

	public double getVersion() {
		return 1.0;
	}
	
	public String getName() {
		return "SequenceAsPrologExport";
	}
	
	public String getClassKey() {
		return "sequence_as_prolog_export";
	}
}
