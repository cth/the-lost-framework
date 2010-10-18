package com.lost.plugins;

import com.clcbio.api.free.datatypes.bioinformatics.sequence.BasicSequence;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.Sequence;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.alphabet.Alphabet;

public class FastaExport {
	private int lineSize = 70;
	
	public FastaExport() {

	}
	
	public String fastaFromSequence(BasicSequence sequence) {
		int nextStart,nextEnd;
		
		StringBuffer sb = new StringBuffer();
		
		sb.append("> fasta generated from CLC Bio LoSt plugin\n");
			
		for(nextStart = 0; nextStart < sequence.getLength(); nextStart += lineSize) {
			if (nextStart+lineSize > sequence.getLength())
				nextEnd = sequence.getLength();
			else	
				nextEnd = nextStart + lineSize;
				
			exportFastaLine(sb,nextStart,nextEnd,sequence);
		}
			
		return sb.toString();
	}
		
		public static void exportFastaLine(StringBuffer sb, int start, int end, BasicSequence seq) {
			Alphabet alpha = seq.getAlphabet();

			byte[] seqBytes = seq.getSymbols(start,end);
			for(int i = 0; i < seqBytes.length; i++) 
				sb.append(alpha.getSymbol(seqBytes[i]).getCharName());
		}
}
