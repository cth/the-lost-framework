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

			String annotationName = t.getFunctor();
			int firstPos = Integer.parseInt(t.getTerm(1).toString());
			int lastPos = Integer.parseInt(t.getTerm(2).toString());
			String strand = t.getTerm(3).toString();
			int frame = Integer.parseInt(t.getTerm(4).toString());
			Term extra = t.getTerm(5);
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
