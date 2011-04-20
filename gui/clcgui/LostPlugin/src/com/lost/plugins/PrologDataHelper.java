package com.lost.plugins;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.alexgruenstein.javaprolog.Term;

import com.clcbio.api.free.datatypes.bioinformatics.sequence.BasicSequence;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.alphabet.Alphabet;

public class PrologDataHelper {
	private static Pattern comment_pattern = Pattern.compile("\\s*\\%.*");
	private static Pattern fact_pattern = Pattern.compile(	"([a-z]+\\w*)" +// functor
															"\\(" + 
															"(\\d+)," + 	// LeftEnd 
															"(\\d+)," + 	// RightEnd
															"\'?(\\+|\\-)\'?," + 	// Strand
															"(\\d)," +   	// Frame
															".*");			// The rest

	
	public static String sequenceToPrologData(BasicSequence sequence, int factSize) {
		int nextStart,nextEnd;
		
		StringBuffer sb = new StringBuffer();
	
		for(nextStart = 0; nextStart < sequence.getLength(); nextStart += factSize) {
			if (nextStart+factSize > sequence.getLength())
				nextEnd = sequence.getLength();
			else
				nextEnd = nextStart + factSize;
			
			appendSequenceFact(sb,nextStart,nextEnd,sequence);
		}
		
		return sb.toString();
	}
	
	public static void appendSequenceFact(StringBuffer sb, int start, int end, BasicSequence seq) {
		Alphabet alpha = seq.getAlphabet();
		
		sb.append("data(" + (start+1) + "," + end + ", [");
		byte[] seqBytes = seq.getSymbols(start,end);
		for(int i = 0; i < seqBytes.length; i++) {
			sb.append(Character.toLowerCase(alpha.getSymbol(seqBytes[i]).getCharName()));
			if (i == (seqBytes.length-1))
				sb.append("]).\n");
			else
				sb.append(",");
		}
	}
	
	/**
	 * Parses a file with LoSt annotations.
	 * @param inputFile
	 * @return a list of LostAnnation objects
	 */
	public static ArrayList<LostAnnotation> parseLostAnntationFile(String inputFile) {
		ArrayList<LostAnnotation> annotations = new ArrayList<LostAnnotation>();
		
		try {
			FileInputStream fstream = new FileInputStream(inputFile);
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String line;

			while ((line = br.readLine()) != null) {
				System.err.println(line);
				if(!isComment(line)) {
					LostAnnotation annot = LostAnnotation.fromPrologTermString(line);
					if (annot != null)
						annotations.add(annot);
				}
			}
			in.close();
		} catch (Exception e) {
			System.err.println("Error: " + e.getMessage());
		}
		
		System.err.println("Number of annotations parsed: " + annotations.size());
		return annotations;
	}

	/**
	 * converts a nested list term representation to a flat arraylist
	 * @param terms
	 * @param prologList
	 */
	public static void prologListTermsToArrayList(ArrayList<Term> terms, Term prologList) {
		if (prologList.getFunctor().equals(".")) {
			terms.add(prologList.getTerm(1));
			prologListTermsToArrayList(terms,prologList.getTerm(2));
		}
	}

	/*
	public static Region parseFact(String fact) {
		Matcher m = fact_pattern.matcher(fact);
		if (m.matches()) {
			int firstPos = Integer.parseInt(m.group(2));
			int lastPos = Integer.parseInt(m.group(3));	
			//return new 
			Region r = new Region(firstPos,lastPos,(m.group(4).equals("+"))? Region.STRAND_PLUS : Region.STRAND_MINUS);
			
			return r;
		} else 
			return null;
	}
	*/
	
	public static boolean isComment(String line) {
		Matcher m = comment_pattern.matcher(line);
		return m.matches();
	}
}