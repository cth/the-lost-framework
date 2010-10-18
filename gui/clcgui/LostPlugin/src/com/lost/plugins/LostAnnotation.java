package com.lost.plugins;

import java.util.ArrayList;

import org.alexgruenstein.javaprolog.Parser;
import org.alexgruenstein.javaprolog.Term;

import com.clcbio.api.free.datatypes.bioinformatics.sequence.feature.Feature;
import com.clcbio.api.free.datatypes.bioinformatics.sequence.region.Region;

public class LostAnnotation {
	private String annotationType;
	private int firstPos;
	private int lastPos;
	private int frame;
	private String strand;
	ArrayList<Term> extra;

	public LostAnnotation(String annotationType, int firstPos, int lastPos,
			int frame, String strand, ArrayList<Term> extra) {
		super();
		this.annotationType = annotationType;
		this.firstPos = firstPos;
		this.lastPos = lastPos;
		this.frame = frame;
		this.strand = strand;
		this.extra = extra;
	}
	
	public static LostAnnotation fromPrologTermString(String termline) {
		try {
			Parser p = new Parser();
			Term t = p.parseTerm(termline);
			int firstPos, lastPos, frame, position;
			String strand;
			Term extra;
			
			if (t.getArity() == 5) // probably old format
				position = 1;
			else if (t.getArity() == 6) // probably new format
				position = 2;
			else
				position = -1; // provoke error
			
			// Parse leftPos
			String annotationName = t.getFunctor();
			firstPos = Integer.parseInt(t.getTerm(position).toString());
			
			// Parse rightPos
			++position;
			lastPos = Integer.parseInt(t.getTerm(position).toString());
			
			// Parse strand
			++position;
			strand = t.getTerm(position).toString();
				
			// Parse frame
			++position;
			frame = Integer.parseInt(t.getTerm(position).toString());

			// Parse extra list
			++position;			
			extra = t.getTerm(position);
			ArrayList<Term> extraList = new ArrayList<Term>();

			if (extra.getFunctor().equals("."))
				PrologDataHelper.prologListTermsToArrayList(extraList, extra);
			else
				extraList.add(extra);

			return new LostAnnotation(annotationName, firstPos, lastPos, frame, strand, extraList);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public Feature toFeature() {
		Region r = new Region(firstPos,lastPos,(getStrand().equals("+"))? Region.STRAND_PLUS : Region.STRAND_MINUS);
		Feature f = new Feature(getAnnotationType(), r);
		
		for(Term t : extra) {
			String key = t.getFunctor();
			
			StringBuffer value = new StringBuffer();
			
			for (int i = 1; i <= t.getArity(); i++) {
				value.append(t.getTerm(i).toString());
				if (i != t.getArity())
					value.append(", ");
			}
			
			f.addAnnotation(key, value.toString());
		}
		
		return f;
	}

	public String getAnnotationType() {
		return annotationType;
	}

	public void setAnnotationType(String annotationType) {
		this.annotationType = annotationType;
	}

	public int getFirstPos() {
		return firstPos;
	}

	public void setFirstPos(int firstPos) {
		this.firstPos = firstPos;
	}

	public int getLastPos() {
		return lastPos;
	}

	public void setLastPos(int lastPos) {
		this.lastPos = lastPos;
	}

	public int getFrame() {
		return frame;
	}

	public void setFrame(int frame) {
		this.frame = frame;
	}

	public String getStrand() {
		return strand;
	}

	public void setStrand(String strand) {
		this.strand = strand;
	}

	public ArrayList<Term> getExtra() {
		return extra;
	}

	public void setExtra(ArrayList<Term> extra) {
		this.extra = extra;
	}
}
