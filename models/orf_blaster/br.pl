xml( [],
	[
	element( 'BlastOutput',
		[],
		[
		element( 'BlastOutput_program',
			[],
			[
			pcdata("tblastn")
			] ),
		element( 'BlastOutput_version',
			[],
			[
			pcdata("TBLASTN 2.2.22+")
			] ),
		element( 'BlastOutput_reference',
			[],
			[
			pcdata("Stephen F. Altschul, Thomas L. Madden, Alejandro A. Sch&auml;ffer, Jinghui Zhang, Zheng Zhang, Webb Miller, and David J. Lipman (1997), ""Gapped BLAST and PSI-BLAST: a new generation of protein database search programs"", Nucleic Acids Res. 25:3389-3402.")
			] ),
		element( 'BlastOutput_db',
			[],
			[
			pcdata("NC_000913/NC_000913.fna")
			] ),
		element( 'BlastOutput_query-ID',
			[],
			[
			pcdata("1")
			] ),
		element( 'BlastOutput_query-def',
			[],
			[
			pcdata("U00096 1 48 +1")
			] ),
		element( 'BlastOutput_query-len',
			[],
			[
			pcdata("16")
			] ),
		element( 'BlastOutput_param',
			[],
			[
			element( 'Parameters',
				[],
				[
				element( 'Parameters_matrix',
					[],
					[
					pcdata("BLOSUM62")
					] ),
				element( 'Parameters_expect',
					[],
					[
					pcdata("0.0001")
					] ),
				element( 'Parameters_gap-open',
					[],
					[
					pcdata("11")
					] ),
				element( 'Parameters_gap-extend',
					[],
					[
					pcdata("1")
					] ),
				element( 'Parameters_filter',
					[],
					[
					pcdata("L;")
					] )
				] )
			] ),
		element( 'BlastOutput_iterations',
			[],
			[
			element( 'Iteration',
				[],
				[
				element( 'Iteration_iter-num',
					[],
					[
					pcdata("1")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("1")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 1 48 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("16")
					] ),
				element( 'Iteration_hits',
					[],
					[] ),
				element( 'Iteration_stat',
					[],
					[
					element( 'Statistics',
						[],
						[
						element( 'Statistics_db-num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Statistics_db-len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Statistics_hsp-len',
							[],
							[
							pcdata("0")
							] ),
						element( 'Statistics_eff-space',
							[],
							[
							pcdata("24744928")
							] ),
						element( 'Statistics_kappa',
							[],
							[
							pcdata("0.041")
							] ),
						element( 'Statistics_lambda',
							[],
							[
							pcdata("0.267")
							] ),
						element( 'Statistics_entropy',
							[],
							[
							pcdata("0.14")
							] )
						] )
					] ),
				element( 'Iteration_message',
					[],
					[
					pcdata("No hits found")
					] )
				] ),
			element( 'Iteration',
				[],
				[
				element( 'Iteration_iter-num',
					[],
					[
					pcdata("2")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("2")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 49 114 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("22")
					] ),
				element( 'Iteration_hits',
					[],
					[
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("gi|49175990|ref|NC_000913.2|")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Escherichia coli str. K-12 substr. MG1655 chromosome, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_000913")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Hit_hsps',
							[],
							[
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("42.3577907199446")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("98")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.60213355752534e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("114")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KKSVXXQLLNWLPAVSKLKFYX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KKSV**QLLNWLPAVSKLKFY*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KKSV  QLLNWLPAVSKLKFY ")
									] )
								] )
							] )
						] )
					] ),
				element( 'Iteration_stat',
					[],
					[
					element( 'Statistics',
						[],
						[
						element( 'Statistics_db-num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Statistics_db-len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Statistics_hsp-len',
							[],
							[
							pcdata("0")
							] ),
						element( 'Statistics_eff-space',
							[],
							[
							pcdata("34024276")
							] ),
						element( 'Statistics_kappa',
							[],
							[
							pcdata("0.041")
							] ),
						element( 'Statistics_lambda',
							[],
							[
							pcdata("0.267")
							] ),
						element( 'Statistics_entropy',
							[],
							[
							pcdata("0.14")
							] )
						] )
					] )
				] ),
			element( 'Iteration',
				[],
				[
				element( 'Iteration_iter-num',
					[],
					[
					pcdata("3")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("3")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 115 255 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("47")
					] ),
				element( 'Iteration_hits',
					[],
					[
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("gi|49175990|ref|NC_000913.2|")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Escherichia coli str. K-12 substr. MG1655 chromosome, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_000913")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Hit_hsps',
							[],
							[
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("66.6253640027379")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("161")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.20660637686881e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("115")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("255")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRSLNTLTNIGIAHRQIKITEYTTSMKRISXXXXXXXXXXXGNGAGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRSLNTLTNIGIAHRQIKITEYTTSMKRISTTITTTITITTGNGAG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRSLNTLTNIGIAHRQIKITEYTTSMKRISTTITTTITITTGNGAG ")
									] )
								] )
							] )
						] )
					] ),
				element( 'Iteration_stat',
					[],
					[
					element( 'Statistics',
						[],
						[
						element( 'Statistics_db-num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Statistics_db-len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Statistics_hsp-len',
							[],
							[
							pcdata("0")
							] ),
						element( 'Statistics_eff-space',
							[],
							[
							pcdata("38663400")
							] ),
						element( 'Statistics_kappa',
							[],
							[
							pcdata("0.041")
							] ),
						element( 'Statistics_lambda',
							[],
							[
							pcdata("0.267")
							] ),
						element( 'Statistics_entropy',
							[],
							[
							pcdata("0.14")
							] )
						] )
					] )
				] ),
			element( 'Iteration',
				[],
				[
				element( 'Iteration_iter-num',
					[],
					[
					pcdata("4")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("4")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 256 2799 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("848")
					] ),
				element( 'Iteration_hits',
					[],
					[
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("gi|49175990|ref|NC_000913.2|")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Escherichia coli str. K-12 substr. MG1655 chromosome, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_000913")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Hit_hsps',
							[],
							[
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("1710.27195444208")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("4428")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("848")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("256")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2799")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("846")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("846")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("848")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RVQETQKKARTXQCGLFFSTKGNEVTTMRVLKFGGTSVANAERFLRVADILESNARQGQVATVLSAPAKITNHLVAMIEKTISGQDALPNISDAERIFAELLTGLAAAQPGFPLAQLKTFVDQEFAQIKHVLHGISLLGQCPDSINAALICRGEKMSIAIMAGVLEARGHNVTVIDPVEKLLAVGHYLESTVDIAESTRRIAASRIPADHMVLMAGFTAGNEKGELVVLGRNGSDYSAAVLAACLRADCCEIWTDVDGVYTCDPRQVPDARLLKSMSYQEAMELSYFGAKVLHPRTITPIAQFQIPCLIKNTGNPQAPGTLIGASRDEDELPVKGISNLNNMAMFSVSGPGMKGMVGMAARVFAAMSRARISVVLITQSSSEYSISFCVPQSDCVRAERAMQEEFYLELKEGLLEPLAVTERLAIISVVGDGMRTLRGISAKFFAALARANINIVAIAQGSSERSISVVVNNDDATTGVRVTHQMLFNTDQXXXXXXXXXXXXXXALLEQLKRQQSWLKNKHIDLRVCGVANSKALLTNVHGLNLENWQEELAQAKEPFNLGRLIRLVKEYHLLNPVIVDCTSSQAVADQYADFLREGFHVVTPNKKANTSSMDYYHQLRYAAEKSRRKFLYDTNVGAGLPVIENLQNLLNAGDELMKFSGILSGSLSYIFGKLDEGMSFSEATTLAREMGYTEPDPRDDLSGMDVARKLLILARETGRELELADIEIEPVLPAEFNAEGDVAAFMANLSQLDDLFAARVAKARDEGKVLRYVGNIDEDGVCRVKIAEVDGNDPLFKVKNGENALAFYSHYYQPLPLVLRGYGAGNDVTAAGVFADLLRTLSWKLGVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RVQETQKKART*QCGLFFSTKGNEVTTMRVLKFGGTSVANAERFLRVADILESNARQGQVATVLSAPAKITNHLVAMIEKTISGQDALPNISDAERIFAELLTGLAAAQPGFPLAQLKTFVDQEFAQIKHVLHGISLLGQCPDSINAALICRGEKMSIAIMAGVLEARGHNVTVIDPVEKLLAVGHYLESTVDIAESTRRIAASRIPADHMVLMAGFTAGNEKGELVVLGRNGSDYSAAVLAACLRADCCEIWTDVDGVYTCDPRQVPDARLLKSMSYQEAMELSYFGAKVLHPRTITPIAQFQIPCLIKNTGNPQAPGTLIGASRDEDELPVKGISNLNNMAMFSVSGPGMKGMVGMAARVFAAMSRARISVVLITQSSSEYSISFCVPQSDCVRAERAMQEEFYLELKEGLLEPLAVTERLAIISVVGDGMRTLRGISAKFFAALARANINIVAIAQGSSERSISVVVNNDDATTGVRVTHQMLFNTDQVIEVFVIGVGGVGGALLEQLKRQQSWLKNKHIDLRVCGVANSKALLTNVHGLNLENWQEELAQAKEPFNLGRLIRLVKEYHLLNPVIVDCTSSQAVADQYADFLREGFHVVTPNKKANTSSMDYYHQLRYAAEKSRRKFLYDTNVGAGLPVIENLQNLLNAGDELMKFSGILSGSLSYIFGKLDEGMSFSEATTLAREMGYTEPDPRDDLSGMDVARKLLILARETGRELELADIEIEPVLPAEFNAEGDVAAFMANLSQLDDLFAARVAKARDEGKVLRYVGNIDEDGVCRVKIAEVDGNDPLFKVKNGENALAFYSHYYQPLPLVLRGYGAGNDVTAAGVFADLLRTLSWKLGV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RVQETQKKART QCGLFFSTKGNEVTTMRVLKFGGTSVANAERFLRVADILESNARQGQVATVLSAPAKITNHLVAMIEKTISGQDALPNISDAERIFAELLTGLAAAQPGFPLAQLKTFVDQEFAQIKHVLHGISLLGQCPDSINAALICRGEKMSIAIMAGVLEARGHNVTVIDPVEKLLAVGHYLESTVDIAESTRRIAASRIPADHMVLMAGFTAGNEKGELVVLGRNGSDYSAAVLAACLRADCCEIWTDVDGVYTCDPRQVPDARLLKSMSYQEAMELSYFGAKVLHPRTITPIAQFQIPCLIKNTGNPQAPGTLIGASRDEDELPVKGISNLNNMAMFSVSGPGMKGMVGMAARVFAAMSRARISVVLITQSSSEYSISFCVPQSDCVRAERAMQEEFYLELKEGLLEPLAVTERLAIISVVGDGMRTLRGISAKFFAALARANINIVAIAQGSSERSISVVVNNDDATTGVRVTHQMLFNTDQVIEVFVIGVGGVGGALLEQLKRQQSWLKNKHIDLRVCGVANSKALLTNVHGLNLENWQEELAQAKEPFNLGRLIRLVKEYHLLNPVIVDCTSSQAVADQYADFLREGFHVVTPNKKANTSSMDYYHQLRYAAEKSRRKFLYDTNVGAGLPVIENLQNLLNAGDELMKFSGILSGSLSYIFGKLDEGMSFSEATTLAREMGYTEPDPRDDLSGMDVARKLLILARETGRELELADIEIEPVLPAEFNAEGDVAAFMANLSQLDDLFAARVAKARDEGKVLRYVGNIDEDGVCRVKIAEVDGNDPLFKVKNGENALAFYSHYYQPLPLVLRGYGAGNDVTAAGVFADLLRTLSWKLGV ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("333.183470537546")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("853")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.78877682983322e-92")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("839")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4127903")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4130272")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("251")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("413")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("821")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KFGGTSVANAERFLRVADILESNARQGQVATVLSAPAKITNHLVAMIEKTISGQDALPNISDAERIF-AELLTGLAAAQPGFPLAQLKTFVDQEFAQIKHVL-HGISLLGQCPDSINAALICRGEKMSIAIMAGVLEARGHNVTVIDPVEKLLAVGHYLESTVDIAESTRRIAASRI--PADHMVLMAGFTAGNEKGELVVLGRNGSDYSAAVLAACLRADCCEIWTDVDGVYTCDPRQVPDARLLKSMSYQEAMELSYFGAKVLHPRTITPIAQFQIPCLIKNTGNPQAPGTLIGASRDEDELP----VKGISNLNNMAMFSVSGPGMKGMVGMAARVFAAMSRARISVVLITQSSSEYSISFCVPQSDCVRAERAMQEEFYLELKEGLLEPLAVTERLAIISVVGDGMRTLRGISAKFFAALARANINIVAIAQGSSERSISVVVNNDDATTGVRVTHQMLFNTDQXXXXXXXXXXXXXXALLEQLKRQQSWLKNKH-IDLRVCGVANSKALLTNVHGLN----LENWQEELAQAKEPFNLGRLIRLVKEYHLLNPVIVDCTSSQAVADQYADFLREGFHVVTPNKKANTSSMDYYHQLRYAAEKSRRKFLYDTNVGAGLPVIENLQNLLNAGDELMKFSGILSGSLSYIFGKLDEGMSFSEATTLAREMGYTEPDPRDDLSGMDVARKLLILARETGRELELADIEIEPVLPAEFNAEGDVAAFMANLSQLDDLFAARVAKARDEGKVLRYVGNIDEDGVCRVKIAEVDGNDPLFKVKNGENALAFYSHYYQPLPLVLRGYGAGNDVTAAGVFADLLR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KFGGSSLADVKCYLRVAGIM-AEYSQPDDMMVVSAAGSTTNQLINWLKLSQTDRLSAHQVQQTLRRYQCDLISGLLPAEEADSL--ISAFV-SDLERLAALLDSGIN------DAVYAEVVGHGEVWSARLMSAVLNQQGLPAAWLDARE-FLRAERAAQPQVDEGLSYPLLQQLLVQHPGKRLV-VTGFISRNNAGETVLLGRNGSDYSATQIGALAGVSRVTIWSDVAGVYSADPRKVKDACLLPLLRLDEASELARLAAPVLHARTLQPVSGSEIDLQLRCSYTPDQ-----GSTRIERVLASGTGARIVTSHDDVCLIEFQVPASQDFKLAHKEIDQILKRAQVRPLAVGVHNDRQLLQFCYTSEVADSALKILDE-------AGLPGELRLRQGLALVAMVGAGVTRNPLHCHRFWQQLKGQPVEFT--WQSDDGISLVAVLRTGPTESLIQGLHQSVFRAEKRIGLVLFGKGNIGSRWLELFAREQSTLSARTGFEFVLAGVVDSRRSLLSYDGLDASRALAFFNDEAVEQDEE----SLFLWMRAHPYDDLVVLDVTASQQLADQYLDFASHGFHVISANKLAGASDSNKYRQIHDAFEKTGRHWLYNATVGAGLPINHTVRDLIDSGDTILSISGIFSGTLSWLFLQFDGSVPFTELVDQAWQQGLTEPDPRDDLSGKDVMRKLVILAREAGYNIEPDQVRVESLVPAHCEG-GSIDHFFENGDELNEQMVQRLEAAREMGLVLRYVARFDANGKARVGVEAVREDHPLASLLPCDNVFAIESRWYRDNPLVIRGPGAGRDVTAGAIQSDINR")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KFGG+S+A+ + +LRVA I+ +   Q     V+SA    TN L+  ++ + + + +   +    R +  +L++GL  A+    L  +  FV  +  ++  +L  GI+      D++ A ++  GE  S  +M+ VL  +G     +D  E  L      +  VD   S   +    +  P   +V + GF + N  GE V+LGRNGSDYSA  + A        IW+DV GVY+ DPR+V DA LL  +   EA EL+   A VLH RT+ P++  +I   ++ +  P       G++R E  L      + +++ +++ +     P  +        +   + RA++  + +   +    + FC        A + + E        GL   L + + LA++++VG G+        +F+  L    +      Q     S+  V+      + ++  HQ +F  ++ I + + G G +G   LE   R+QS L  +   +  + GV +S+  L +  GL+    L  + +E  +  E      L   ++ +   + V++D T+SQ +ADQY DF   GFHV++ NK A  S  + Y Q+  A EK+ R +LY+  VGAGLP+   +++L+++GD ++  SGI SG+LS++F + D  + F+E    A + G TEPDPRDDLSG DV RKL+ILARE G  +E   + +E ++PA     G +  F  N  +L++    R+  AR+ G VLRYV   D +G  RV +  V  + PL  +   +N  A  S +Y+  PLV+RG GAG DVTA  + +D+ R")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("161.769659254324")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("408")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.29096000704818e-40")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("487")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4229913")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4231241")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("142")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("227")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("471")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VLKFGGTSVANAERFLRVADILESNARQGQVATVLSAPAKITNHLVAMIEKTISGQ-----DALPNISDAERIFAELLTGLAAAQPGFPLAQLKTFVDQEFAQIKHVLHGISLLGQC-----PDSINAALICRGEKMSIAIMAGVLEARGHNVTVIDPVEKLLAVGHYLESTVDIAESTRRIAASRIPA--DHMVLMAGFTAGNEKGELVVLGRNGSDYSAAVLAACLRADCCEIWTDVDGVYTCDPRQVPDARLLKSMSYQEAMELSYFGAKVLHPRTITPIAQFQIPCLIKNTGNPQAPGTLIGASRDEDELPVKGISNLNNMAMFSVSGPGMKGMVGMAARVFAAMSRARISVVLITQSSSEYSISFCVPQSDCVRAERA-MQEEFYLELKEGLLEPLAVTERLAIISVVGDGMRTLRGISAKFFAALARANINIVAIAQGSSERSISVVVNNDDATTGVRVTHQMLF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("VSKFGGTSVADFDAMNRSADIVLSDANVRLV--VLSASAGITNLLVALAEGLEPGERFEKLDAIRNIQ-------------------FAILERLRYPNVIREEIERLLENITVLAEAAALATSPALTDELVSHGELMSTLLFVEILRERDVQAQWFDVRKVMRTNDRFGRAEPDIAALAELAALQLLPRLNEGLVITQGFIGSENKGRTTTLGRGGSDYTAALLAEALHASRVDIWTDVPGIYTTDPRVVSAAKRIDEIAFAEAAEMATFGAKVLHPATLLPAVRSDIPVFVGSSKDPRAGGTLV-CNKTENPPLFRALALRRNQTLLTLHSLNMLHSRGFLAEVFGILARHNISVDLIT--TSEVSVALTLDTTGSTSTGDTLLTQSLLMELSA--LCRVEVEEGLALVALIGNDLSKACGVGKEVFGVLEPFNIRM--ICYGASSHNLCFLVPGEDAEQVVQKLHSNLF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("V KFGGTSVA+ +   R ADI+ S+A    V  VLSA A ITN LVA+ E    G+     DA+ NI                    F + +   + +    +I+ +L  I++L +        ++   L+  GE MS  +   +L  R       D  + +     +  +  DIA      A   +P   + +V+  GF     KG    LGR GSDY+AA+LA  L A   +IWTDV G+YT DPR V  A+ +  +++ EA E++ FGAKVLHP T+ P  +  IP  + ++ +P+A GTL+  ++ E+    + ++   N  + ++    M    G  A VF  ++R  ISV LIT  +SE S++  +  +         + +   +EL    L  + V E LA+++++G+ +    G+  + F  L   NI +  I  G+S  ++  +V  +DA   V+  H  LF")
									] )
								] )
							] )
						] )
					] ),
				element( 'Iteration_stat',
					[],
					[
					element( 'Statistics',
						[],
						[
						element( 'Statistics_db-num',
							[],
							[
							pcdata("1")
							] ),
						element( 'Statistics_db-len',
							[],
							[
							pcdata("4639675")
							] ),
						element( 'Statistics_hsp-len',
							[],
							[
							pcdata("0")
							] ),
						element( 'Statistics_eff-space',
							[],
							[
							pcdata("1164486639")
							] ),
						element( 'Statistics_kappa',
							[],
							[
							pcdata("0.041")
							] ),
						element( 'Statistics_lambda',
							[],
							[
							pcdata("0.267")
							] ),
						element( 'Statistics_entropy',
							[],
							[
							pcdata("0.14")
							] )
						] )
					] )
				] )
			] )
		] )
	] ).
