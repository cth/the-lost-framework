# gbk_to_frames.py
# S/ren M/rk
# 100623

# program for converting gbk to various usefull formats...
# mostly to be used for the genome_finder project...

# example command:
# python ~/code/gbk_to_frames.py f mv ~/data/Escherichia_coli_K12/NC_000913.gbk > ~/data/k12_frames_verified.dat

# input: outmode,typemode ,delimit, file.gbk 
# output: .seq, .dat, .inf .len .ptt

# outmode_x..y: from genome segment nts x to y, else from entire genome 

# outmode= d(at): sequence as .dat for PRISM analysis
# outmode= s(eq): just raw sequence as .seq
# outmode= l(enght): sorted lenghts
# outmode= i(nfo): statisics on sequence (some printed for .seq and .dat)
# outmode= f(rames): prints out frame sequence in .dat format
# outmode= ptt: prints "ptt" file with start..stop, frame and 7 colums
# outmode= pl: prints "ptt" file in .pl format

# typemode_x..y: only sequences in lenght x to y

# typemode= g_(enome): genome sequence
# typemode= g+: genes from plus strand
# typemode= g-: genes from minus strand
# typemode= gbs: all genes boht strands

# typemode= r_(NA): non-mRNAs
# typemode= r+: non-mRNAs from + strand
# typemode= r-: non-RNAs from - strand
# typemode= rr: rRNAs
# typemode= rt: tRNAs
# typemode= rs: sRNAs

# typemode= c: non-RNA chunks
# typemode= i: intergenic sequences
# typemode= virs: intergenic sequences no verified, rRNA with start...
# typemode= p: pseudogenes
# typemode= s: intergenic plus start

# typemode= m_: mRNAs
# typemode= m+: mRNAs from + strand
# typemode= m-: mRNAs from - strand

# typemode= mb: non-cannonical mRNAs 
# typemode= ms: spurious and putative mRNAs 
# typemode= mc: canonical mRNAs with correct start and stop + modulo 3 length
# typemode= mpn: cannonical mRNAs predicted no function
# typemode= mpf: cannonical mRNAs predicted function
# typemode= mcn: cannonical mRNAs conserved no function
# typemode= mcf: canonical mRNAs conserved function
# typemode= mnp: canonical mRNAs (not predicted)
# typemode= mv: verified canonical mRNAs (not spurious,predicted or conserved)


import sys
import re
import operator

outmode=sys.argv[1]

typemode=sys.argv[2]

input=open(sys.argv[3]) 
gbk_file=input.read()
input.close()

######### Sections:
number_p=re.compile('[0123456789]+')

feature_split=gbk_file.split('FEATURE')

gbk_file='deleted'

origin_split=feature_split[1].split('ORIGIN')

intro=feature_split[0]

features=origin_split[0]

genes=features.split('     gene            ')

info_p=re.compile('\".+\"')

info_list_2=info_p.findall(genes[0])

refs=intro.split('REFERENCE')

intro=refs[0].split('\n')

####### PRINT:
print '%s from gbk_to_dat.py %s %s'%('%',outmode,typemode)
print '%s organism:%s'%('%',info_list_2[0])
print '%s strain:%s'%('%',info_list_2[2])
print '%s sub_strain:%s'%('%',info_list_2[3])
print '%s db_xref:%s'%('%',info_list_2[4])
print '%s version: %s'%('%',intro[3][12:])

intro='deleted'
features='deleted'
info_list_2='deleted'
refs='deleted'
intro='deleted'

######## Genome str:
genome_str=origin_split[1]
genome_str=genome_str.replace('\n','')
genome_str=genome_str.replace(' ','')
genome_str=genome_str.replace('1','')
genome_str=genome_str.replace('2','')
genome_str=genome_str.replace('3','')
genome_str=genome_str.replace('4','')
genome_str=genome_str.replace('5','')
genome_str=genome_str.replace('6','')
genome_str=genome_str.replace('7','')
genome_str=genome_str.replace('8','')
genome_str=genome_str.replace('9','')
genome_str=genome_str.replace('0','')
genome_str=genome_str.replace('//','')

origin_split='deleted'

############ Delimit:

fragment_begin=0
fragment_end=len(genome_str)

if len(outmode)>6:
	fragment_begin=int(number_p.findall(outmode)[0])
	fragment_end=int(number_p.findall(outmode)[1])

size_begin=0
size_end=10000

if len(typemode)>6:
	size_begin=int(number_p.findall(typemode)[0])	
	size_end=int(number_p.findall(typemode)[1])

print '%s fragment:%s..%s'%('%',fragment_begin,fragment_end)

######## annotation stuff 

delim_p=re.compile('[0123456789]+\.\.[0123456789]+')

gene_list=[]
for e in genes[1:]:
	gene_list.append(e.split('\n                     '))

gene_p=re.compile('/gene=".+"')
product_p=re.compile('/product="[^"]+')
codon_p=re.compile('/codon_start=\d+')
transl_p=re.compile('/transl_table=\d+')

annot_unlim=[]
for e in gene_list:
	start=delim_p.findall(e[0])[0].split('..')[0]
	stop=delim_p.findall(e[0])[0].split('..')[1]
	name=gene_p.findall(str(e))[0].split('"')[1]	
	if 'complement' in e[0]:
		strand='-'
	else:
		strand='+'
	if strand=='+':
		frame=((int(start)-1)%3)+1
	else:
		frame=((int(start)-1)%3)+4
	length=int(stop)-(int(start)-1)
	mod=length%3
	product=product_p.findall(str(e))
	codon=codon_p.findall(str(e))
	transl=transl_p.findall(str(e))
	if 'CDS' in str(e):
		annot_unlim.append((int(start)-1,int(stop),name,strand,frame,length,mod,'mRNA',product,codon,transl,genome_str[int(start)-1:int(start)+2],genome_str[int(stop)-3:int(stop)]))
	if 'RNA   ' in str(e):
		annot_unlim.append((int(start)-1,int(stop),name,strand,frame,length,mod,'RNA',product))
	if 'pseudo' in str(e):
		annot_unlim.append((int(start)-1,int(stop),name,strand,frame,length,mod,'pseudo',product))

############# Annot limited:

annot=[]
for e in annot_unlim:
	if int(e[0])>int(fragment_begin):
		if int(e[1])<int(fragment_end):
			if int(e[5])>int(size_begin):
				if int(e[5])<int(size_end):
					annot.append(e)


################# getting Genes::

plus=[]
minus=[]
RNAs=[]
mRNAs=[]
pseudo=[]
unproduct=[]

for e in annot:
	if e[7]=='mRNA':
		mRNAs.append(e)
	if e[7]=='RNA':
		RNAs.append(e)
	if e[3]=='+':
		plus.append(e)
	if e[3]=='-':
		minus.append(e)
	if e[7]=='pseudo':
		pseudo.append(e)
	if e[8]==[]:
		unproduct.append(e)

########### Getting overlaps:

overlaps=[]
back_to_back=[]
double_annot=[]
for i in range(1,len(annot)):
        if annot[i-1][1]>annot[i][0]:
                if annot[i-1][2]==annot[i][2]:
                         double_annot.append((annot[i-1],annot[i],int(annot[i-1][1])-int(annot[i][0])))
                else:
                        overlaps.append((annot[i-1],annot[i],int(annot[i-1][1])-int(annot[i][0])))
        if annot[i][0]==annot[i-1][1]:
                back_to_back.append((annot[i-1],annot[i],int(annot[i-1][1])-int(annot[i][0])))
        
############Getting RNAs:

tRNAs=[]
rRNAs=[]
sRNAs=[]
RNAs_plus=[]
RNAs_minus=[]

for e in RNAs:
	if e[3]=='+':
		RNAs_plus.append(e)
	else:
		RNAs_minus.append(e)
	if 'ribosomal' in str(e[8]):
		rRNAs.append(e)
	if 'tRNA' in str(e[8]):
		tRNAs.append(e)
	else:
		sRNAs.append(e)

##########Getting proteins:

mRNAs_plus=[]
mRNAs_minus=[]
predicted=[]
predicted_protein=[]
predicted_named=[]
conserved=[]
conserved_protein=[]
conserved_named=[]	
spurious=[]
non_predicted=[]
verified=[]
checked=[]
bad=[]

for e in mRNAs:
	if e[3]=='+':
		mRNAs_plus.append(e)
	if e[3]=='-':
		mRNAs_minus.append(e)

######### checked::
#print mRNAs_plus[0]
#print genome_str[mRNAs_plus[0][0]:mRNAs_plus[0][1]]

start_plus=[]
start_minus=[]
stop=[]
mod3=[]
codon_1=[]

for e in mRNAs_plus:
        if e[11]=='atg':
                start_plus.append(e)
        elif e[11]=='ctg':
                start_plus.append(e)
        elif e[11]=='gtg':
                start_plus.append(e)
        elif e[11]=='ttg':
                start_plus.append(e)
        else:
                bad.append(e)
for e in mRNAs_minus:
        if e[11]=='tta':
                start_minus.append(e)
        elif e[11]=='cta':
                start_minus.append(e)
        elif e[11]=='tca':
                start_minus.append(e)
        else:
                bad.append(e)
for e in start_plus:
        if e[12]=='taa':
                stop.append(e)     
        elif e[12]=='tag':
                stop.append(e)     
        elif e[12]=='tga':
                stop.append(e)     
        else:
                bad.append(e)
for e in start_minus:
        if e[12]=='caa':
                stop.append(e)
        elif e[12]=='cac':
                stop.append(e)
        elif e[12]=='cag':
                stop.append(e)
        elif e[12]=='cat':
                stop.append(e)
        else:bad.append(e)

for e in stop:
        if int(e[6])==0:
                mod3.append(e)
        else:
                bad.append(e)
for e in mod3:
        if '=1' in str(e[9]):
                codon_1.append(e)
        else: 
                bad.append(e)
for e in codon_1:
        if '=11' in str(e[10]):
                checked.append(e)
        else:
                bad.append(e)

for e in checked:
	if 'predicted' in str(e[8]):
		predicted.append(e)
	elif 'conserved' in str(e[8]):
		conserved.append(e)
	elif 'putative' in str(e[8]):
		spurious.append(e)
	elif 'spurious' in str(e[8]):
		spurious.append(e)
	else:
		verified.append(e)

for e in predicted:
	if 'predicted protein' in str(e[8]):
		predicted_protein.append(e)
	else:
		predicted_named.append(e)
for e in conserved:
	if 'conserved protein' in str(e[8]):
		conserved_protein.append(e)
	else:
		conserved_named.append(e)
mRNAs_p=[]
mRNAs_m=[]

for e in checked:
	if e[3]=='+':
		mRNAs_p.append(e)
	if e[3]=='-':
		mRNAs_m.append(e)


########### cntiguous verified mRNAs:

#print annot[0]

contiguous_verified=[]

i=0
while i < len(annot):
        if annot[i] in verified:
                k=[]
                #k.append((annot[i][0],annot[i][1],str(annot[i][4])))
                #i=i+1
                while annot[i] in verified:
                        k.append((annot[i][0],annot[i][1],str(annot[i][4])))
                        i=i+1
                if len(k)>1:
                        contiguous_verified.append(k)
        else:
                i=i+1

#print '%s len contiguous verified:%s'%('%',len(contiguous_verified))
#print contiguous_verified


################ frames:
if outmode=='f':
        if typemode=='m':
                print '%s %s mRNAs'%('%',len(mRNAs))
                frame_seq=str(mRNAs[0][4])
                for e in mRNAs[1:]:
                        frame_seq+=','+str(e[4])
                print 'model([%s]).'%frame_seq
        if typemode=='mv':
                print '%s %s verified mRNAs'%('%',len(verified))
                frame_seq=str(verified[0][4])
                for e in verified[1:]:
                        frame_seq+=','+str(e[4])
                print 'model([%s]).'%frame_seq
        if typemode=='mvc':
                y=0
                for j in contiguous_verified:
                        for g in j:
                                y=y+1
                print '%s number of contiguous verified mRNAs:%s'%('%',y)
                print '%s %s contiguous regions'%('%',len(contiguous_verified))
                for e in contiguous_verified:
                        print '%s pos %s..%s'%('%',e[0][0],e[-1][1])
                        frame_seq=e[0][2]
                        for j in e[1:]:
                                frame_seq+=','+j[2]
                        print 'model([%s]).'%frame_seq


##### print ptt:

if outmode=='ptt':
        if typemode=='mv':
                print '%s number of verified mRNAs:%s'%('%',len(verified))
                for e in verified:
                        print '%s..%s\t%s\t x \t x \t x \t x \t x \t x \t x '%(e[0],e[1],e[3])

        if typemode=='mvc':
                y=0
                for j in contiguous_verified:
                        for g in j:
                                y=y+1
                print '%s %s contiguous regions'%('%',len(contiguous_verified))
                print '%s number of contiguous verified mRNAs:%s'%('%',y)
                for i in contiguous_verified:
                        for e in i:
                                print '%s..%s\t%s\t x \t x \t x \t x \t x \t x \t x '%(e[0],e[1],e[2])

if outmode=='pl':
        if typemode=='mv':
                print '%s number of verified mRNAs:%s'%('%',len(verified))
                for e in verified:
                        if e[3]<4:
                                print 'gb(%s,%s,%s,%s,%s).'%(e[0],e[1],'\'+\'',e[3],'[]')
                        else:
                                print 'gb(%s,%s,%s,%s,%s).'%(e[0],e[1],'\'-\'',e[3],'[]')

        if typemode=='mvc':
                y=0
                for j in contiguous_verified:
                        for g in j:
                                y=y+1
                print '%s number of contiguous verified mRNAs:%s'%('%',y)
                for i in contiguous_verified:
                        for e in i:
                                if e[2]<4:
                                        print 'gb(%s,%s,%s,%s,%s).'%(e[0],e[1],'\'+\'',e[2],'[]')
                                else:
                                        print 'gb(%s,%s,%s,%s,%s).'%(e[0],e[1],'\'-\'',e[2],'[]')








