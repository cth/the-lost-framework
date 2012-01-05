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
				] ),
			element( 'Iteration',
				[],
				[
				element( 'Iteration_iter-num',
					[],
					[
					pcdata("5")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("5")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 2800 2916 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("39")
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
									pcdata("75.0997546729196")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("183")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.76212828067101e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2800")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2916")
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
									pcdata("35")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HGXSLCPGFQCQYERRVXCARGGGDTCXWCIARRCSHGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("HG*SLCPGFQCQYERRV*CARGGGDTC*WCIARRCSHG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("HG SLCPGFQCQYERRV CARGGGDTC WCIARRCSHG ")
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
							pcdata("38663600")
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
					pcdata("6")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("6")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 2917 2964 +1")
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
					pcdata("7")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("7")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 2965 3033 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("23")
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
									pcdata("45.8245869032008")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("107")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.24860868340336e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2965")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3033")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AAVRTTGKYRLSVLGAFLPGTGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AAVRTTGKYRLSVLGAFLPGTG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AAVRTTGKYRLSVLGAFLPGTG ")
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
							pcdata("35570834")
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
					pcdata("8")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("8")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3034 3135 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("34")
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
									pcdata("67.7809627304899")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("164")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.62495015623459e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3034")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3135")
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
									pcdata("33")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ANSSGDDPGKEYADRFGLRLQCLFGGRGADGDEX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ANSSGDDPGKEYADRFGLRLQCLFGGRGADGDE*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ANSSGDDPGKEYADRFGLRLQCLFGGRGADGDE ")
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
							pcdata("38663725")
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
					pcdata("9")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("9")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3136 3156 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("7")
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
							pcdata("10825906")
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
					pcdata("10")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("10")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3157 3306 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("98.9821283797956")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("245")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.64846693037409e-23")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3157")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3306")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XHSFAGFDGRAGRPYLRQHSLRQRGTVFSRWYAVDDRRKRHHQPASARVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*HSFAGFDGRAGRPYLRQHSLRQRGTVFSRWYAVDDRRKRHHQPASARV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" HSFAGFDGRAGRPYLRQHSLRQRGTVFSRWYAVDDRRKRHHQPASARV ")
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
							pcdata("38663325")
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
					pcdata("11")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("11")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3307 3342 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("12")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("12")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3343 3459 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("39")
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
									pcdata("78.1813512802585")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("191")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.38551642225744e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3343")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3459")
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
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SLDGRSQGYFTGAVSPPGLHCARATSGRLHSRLLFPSAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SLDGRSQGYFTGAVSPPGLHCARATSGRLHSRLLFPSA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SLDGRSQGYFTGAVSPPGLHCARATSGRLHSRLLFPSA ")
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
							pcdata("38663600")
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
					pcdata("13")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("13")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3460 3612 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("51")
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
									pcdata("61.6177695158123")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("148")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.37598654890845e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("51")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3460")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3612")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("51")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ACREADERCYRXTLPXTVTXXXXXXXXXXXXXXXXSERYLRLRPDLVRSVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ACREADERCYR*TLP*TVTARLPAGAAGGRGNRRGSERYLRLRPDLVRSV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ACREADERCYR TLP TVTARLPAGAAGGRGNRRGSERYLRLRPDLVRSV ")
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
							pcdata("38663300")
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
					pcdata("14")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("14")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3613 3654 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("14")
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
							pcdata("21651812")
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
					pcdata("15")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("15")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3655 3858 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("68")
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
									pcdata("135.961287667861")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("341")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.46057781143094e-34")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3655")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3858")
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
									pcdata("67")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ELPAKSGRFCSYLPAGYGGRTSTGKLNETLQSERSQRAGQLCASRNPGVGQKSGAVFSARPAGIQPDX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ELPAKSGRFCSYLPAGYGGRTSTGKLNETLQSERSQRAGQLCASRNPGVGQKSGAVFSARPAGIQPD*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ELPAKSGRFCSYLPAGYGGRTSTGKLNETLQSERSQRAGQLCASRNPGVGQKSGAVFSARPAGIQPD ")
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
							pcdata("38662875")
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
					pcdata("16")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("16")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 3859 4053 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("65")
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
									pcdata("124.020100814424")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("310")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.84786086021665e-30")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3859")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4053")
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
									pcdata("59")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("59")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NXXDAEAGFCHPQCEDPLGVYWXXNPTGNPGRARARGVCLPGSGRQCXKRCRLSGIVPRANAGIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("N**DAEAGFCHPQCEDPLGVYW**NPTGNPGRARARGVCLPGSGRQC*KRCRLSGIVPRANAGI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("N  DAEAGFCHPQCEDPLGVYW  NPTGNPGRARARGVCLPGSGRQC KRCRLSGIVPRANAGI ")
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
							pcdata("38662950")
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
					pcdata("17")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("17")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 4054 4326 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("91")
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
									pcdata("177.562841866936")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("449")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.63650170054997e-46")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("91")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4054")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4326")
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
									pcdata("87")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("87")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("91")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RFRRSLYGTNADPYCGXXASDHSDRDLRXYRSGSGSCFLRFTECESGYPLSTRQNQSTARKTVLYIGRQYRNCCHRRRFRCLSGAGEAGVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RFRRSLYGTNADPYCG**ASDHSDRDLR*YRSGSGSCFLRFTECESGYPLSTRQNQSTARKTVLYIGRQYRNCCHRRRFRCLSGAGEAGV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RFRRSLYGTNADPYCG  ASDHSDRDLR YRSGSGSCFLRFTECESGYPLSTRQNQSTARKTVLYIGRQYRNCCHRRRFRCLSGAGEAGV ")
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
							pcdata("38662300")
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
					pcdata("18")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("18")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 4327 4416 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("30")
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
									pcdata("55.0693767252173")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("131")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.25606410612129e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4327")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4416")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XXRTESGARVKLGXLDXHQPFAGADLLLLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("**RTESGARVKLG*LD*HQPFAGADLLLL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("  RTESGARVKLG LD HQPFAGADLLLL ")
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
							pcdata("38663825")
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
					pcdata("19")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("19")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 4417 4731 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("105")
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
									pcdata("178.718440594688")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("452")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.40274340826957e-47")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("16")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("105")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4462")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4731")
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
									pcdata("89")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("89")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("90")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LGAKRKLRRFDGGSAGEVTRSAGETFYCCDQRERYRATFPARRSVVTQSDSGDVIQRDGRESAEQLAACGRVVPPQNLATERAGLCSRGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LGAKRKLRRFDGGSAGEVTRSAGETFYCCDQRERYRATFPARRSVVTQSDSGDVIQRDGRESAEQLAACGRVVPPQNLATERAGLCSRG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LGAKRKLRRFDGGSAGEVTRSAGETFYCCDQRERYRATFPARRSVVTQSDSGDVIQRDGRESAEQLAACGRVVPPQNLATERAGLCSRG ")
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
							pcdata("49487520")
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
					pcdata("20")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("20")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 4732 4881 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("69.7069606100767")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("169")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.34810183262111e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4732")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4881")
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
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XNHATDNAXVKRTGLHFGXXXXXXXXXXXXXVESRRIWLVPRHRASGEIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*NHATDNA*VKRTGLHFGAARCRSLSCAA*SVESRRIWLVPRHRASGEI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" NHATDNA VKRTGLHFGAARCRSLSCAA SVESRRIWLVPRHRASGEI ")
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
							pcdata("38663325")
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
					pcdata("21")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("21")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 4882 5142 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("87")
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
									pcdata("172.55524738001")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("436")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.13431839565058e-45")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("87")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4882")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("5142")
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
									pcdata("82")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("82")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("87")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RERGSDSRXNVGSAKRAGRTCXFTLAFTXSARRFCCVAXIDDESSVKSIHYLNQAGFAFMQPGFFMKKLWRKMTGKKEKFSINAVTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RERGSDSR*NVGSAKRAGRTC*FTLAFT*SARRFCCVA*IDDESSVKSIHYLNQAGFAFMQPGFFMKKLWRKMTGKKEKFSINAVT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RERGSDSR NVGSAKRAGRTC FTLAFT SARRFCCVA IDDESSVKSIHYLNQAGFAFMQPGFFMKKLWRKMTGKKEKFSINAVT ")
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
							pcdata("38662400")
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
					pcdata("22")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("22")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5143 5187 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("15")
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
							pcdata("23198370")
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
					pcdata("23")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("23")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5188 5208 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("7")
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
							pcdata("10825906")
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
					pcdata("24")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("24")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5209 5352 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("48")
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
									pcdata("97.4413300761261")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("241")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.79137579773774e-22")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("5209")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("5352")
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
									pcdata("47")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RAMIKGVTCEKDAIYRTRTFPGSGRSHGSTGCGNYVSPVSKITDRRSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RAMIKGVTCEKDAIYRTRTFPGSGRSHGSTGCGNYVSPVSKITDRRS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RAMIKGVTCEKDAIYRTRTFPGSGRSHGSTGCGNYVSPVSKITDRRS ")
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
							pcdata("38663375")
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
					pcdata("25")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("25")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5353 5418 +1")
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
									pcdata("40.8169924162752")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.95167425431713e-05")
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
									pcdata("5353")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("5418")
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
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("20")
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
									pcdata("XSWLLLGWRSLARPRLVETTLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*SWLLLGWRSLARPRLVETTL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" SWLLLGWRSLARPRLVETTL ")
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
					pcdata("26")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("26")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5419 5475 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("19")
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
							pcdata("29384602")
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
					pcdata("27")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("27")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5476 5946 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("157")
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
									pcdata("292.352315490307")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("747")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.30844722738905e-80")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("14")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("157")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("5515")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("5946")
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
									pcdata("143")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("144")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QTSPLNDKCRVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKEVTFAIKTSQLLWFGQPIFNNETTHQTACFLSVEADDLKFAVLLIEEHRLDNLGIQFLRLHRFKILIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QTSPLNDKCRVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKEVTFAIKTSQLLWFGQPIFNNETTHQTACFLSVEADDLKFAVLLIEEHRLDNLGIQFLRLHRFKILI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("QTSPLNDKCRVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKEVTFAIKTSQLLWFGQPIFNNETTHQTACFLSVEADDLKFAVLLIEEHRLDNLGIQFLRLHRFKILI ")
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
									pcdata("58.1509733325561")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("139")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.75752164074725e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("111")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4135685")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4135900")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKEVTFAIKTSQLLWFGQPIFNNE")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTQSLQYIEFACFCRPDKAFTPHPALCASNLKLPRCSGIFYPLLDIFSTEQSLARKVESVTL----IFTDE")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE ACFCR DKAFTPHPAL A+   L      F     IF  E + A K   +      IF +E")
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
									pcdata("56.6101750288867")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("135")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.35813442128203e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3267741")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3267842")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NA*CATLIRPTPSLQYIEFACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+A  A LIRPT  LQYIE ACFCRQDKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("54.2989775733826")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("129")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.6569974126052e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("72")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("698629")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("698736")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTQLLQYIEFSCFCRPDKAFTPHPA*T-NRTL")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT +LQYIE +CFCR DKAFTPHPA T N TL")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("53.5285784215479")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("127")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.89396038624547e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("831521")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("831613")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPT*TLQYIEFACFCRPDKAFTPHPAF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE ACFCR DKAFTPHPA ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("53.1433788456305")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("126")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.30301806992713e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("707069")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("707158")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPT*SLQYIEFACFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE ACFCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("52.3729796937958")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("124")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.27894910075342e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3068024")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3068113")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTRILQYIEFAGFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT ILQYIE A FCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("8")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.9877801178785")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("123")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.20851484826366e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("83")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2682148")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2682282")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTRRLQYIEFAKFCRPDKAFTPHPA*T---TSTLTANHRFAYA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE A FCR DKAFTPHPA T   T  +   H+FA A")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("9")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.9877801178785")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("123")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.37322974254361e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("714496")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("714597")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTAN")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTRMVQYIEFKRFCRPDKAFTPHPAKTTN")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT ++QYIE   FCR DKAFTPHPA T N")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("10")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.6025805419611")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("122")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.75970093617656e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("430201")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("430317")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTLFQQYIEFTCFCWPDKAFTPHPA*TKHTLLTI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPTL  QYIE  CFC  DKAFTPHPA T +  L +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("11")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.6025805419611")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("122")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.22556050530494e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1067559")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1067675")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("8")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NDKCRVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DDKCKL--------A*CAMLIRPTRRLQYIEFALFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+DKC++        A  A LIRPT  LQYIE A FCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("12")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.6025805419611")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("122")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.47970934506077e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("84")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1952395")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1952541")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAG")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AMLIRPT*SLQYIELARFCRPDKAFTPHPAVIPDLYGLRAISTARLGAG")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE A FCR DKAFTPHPA+  +L  L A       AG")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("13")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.2173809660437")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("121")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.70968330063582e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("70")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("856821")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("856952")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("8")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("52")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NDKCRVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTANL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DDKCKI--------A*CATLIRPTQYLQYIESAGLCRPDKALTPHQALTKRI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+DKC++        A  A LIRPT  LQYIESA  CR DKA TPH ALT  +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("14")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.2173809660437")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("121")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.29325392777104e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2536529")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2536645")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTRFLQYIELARFCRPDKAFTPHPA*TKRTFLTV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE A FCR DKAFTPHPA T    L V")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("15")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("50.8321813901264")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("120")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.14782536713205e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("80")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3096380")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3096532")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("10")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL---TANLTLLVAFKHQF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IRQSTSDATLARLIRPT-------TAATCRLDKAFTPHPAIQVYSARKSSSSEFKNSF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IR S  DATLARLIRPT       +A  CR DKAFTPHPA+   +A  +    FK+ F")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("16")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("50.446981814209")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("119")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.02892367513568e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1814202")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1814297")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALT")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ASLIRPTCFLQYIEFAQFCRPDKAFTPHPA*T")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE A FCR DKAFTPHPA T")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("17")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("50.446981814209")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("119")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.81459600582795e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3637231")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3637320")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPT*SLQSIEFACFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQ IE ACFCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("50.0617822382917")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("118")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.17921395065597e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4106591")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4106698")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DATLARLIRPTLILQYIESACFCRQDKAFTPHPALT")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NA*CASLIRPT*ILQFIEFANFCRPDKAFAPHPAWT")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+A  A LIRPT ILQ+IE A FCR DKAF PHPA T")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("49.6765826623743")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("117")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.67411651600526e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("14")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2712242")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2712403")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QTSPLNDKCRVTIRHSAPDAT--LARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QKSNLNARFKPGIKSAGKRKTA*CATLIRPTWLMQYIEFARSCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("Q S LN + +  I+ +    T   A LIRPT ++QYIE A  CR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("48.9061835105396")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("115")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.29870994706022e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4631148")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4631255")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("APDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("APDATLPRLIMPT---KHIEITGICRTDKAFTPHPAFGA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("APDATL RLI PT   ++IE    CR DKAFTPHPA  A")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("48.9061835105396")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("115")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.62701461217673e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3328481")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3328591")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NAKSA*CATLIRPTRELQYVEFARFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+A  A  A LIRPT  LQY+E A FCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("48.5209839346223")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("114")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.76079105782755e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("105")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3875515")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3875697")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("9")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("70")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKEVTFAIKTSQLLWFGQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTKILQCIEFS*FCRPDKAFTPHPARS--------YKKRAEIISALFISAIR-NYHPANLYFLGQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT ILQ IE + FCR DKAFTPHPA +        +K +  I   +FI  +      + L + GQ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("48.1357843587049")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("113")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.71104886875482e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("740176")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("740289")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("HRCLMRRLTRLIRPTYFLQFIEFAQICRLDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("H      L RLIRPT  LQ+IE A  CR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("47.7505847827876")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("112")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.66027837005099e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4458424")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4458525")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IR*SMPDATLSRLIRPGAIAK-------CRPDKAFTPHPAF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IR S PDATL+RLIRP  I +       CR DKAFTPHPA ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("47.7505847827876")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("112")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.00071916568918e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("98")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2234465")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2234668")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("72")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKEVTFAIKTS")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RSQMPDATLARFIRPTISSTY-*FVWFCRPDKAFKPHPATGTKCRMRLWF---YNVTGCSFSPFLNFTSPSS")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R   PDATLAR IRPT+   Y     FCR DKAF PHPA      + + F   + + G  F   + F   +S")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("46.5949860550355")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("109")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.29294069172143e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4482351")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4482449")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AAPDATLLCLIRPT-------SSVLCRSDKAFTPHPAITA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+APDATL  LIRPT       S+  CR DKAFTPHPA+TA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("46.2097864791182")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("108")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.48998042159135e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2289232")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2289321")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTWIDQFIEFTQFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT I Q+IE   FCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("46.2097864791182")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("108")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.77532184922884e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("138708")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("138797")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTLAVQSIDFAQFCRLDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPTL +Q I+ A FCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.8245869032008")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("107")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.43767381483909e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3703985")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3704086")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NA*CAPLIRPTKILQCIEFARFCRPDKALTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+A  A LIRPT ILQ IE A FCR DKA TPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.4393873272834")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("106")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.76267794707332e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("90")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1112610")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1112792")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIFIKE")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RMRLRNKNVVA*CATLIRPTSFLQFIDFARLCGSDKAFTPHPAKTTS-------Q*KYRRYAGIFLRK")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R+ +R+    A  A LIRPT  LQ+I+ A  C  DKAFTPHPA T +       + ++    GIF+++")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.4393873272834")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("106")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.05361300088412e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("898918")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("899007")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTWPMQCIEFARFCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  +Q IE A FCR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.4393873272834")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("106")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.21037572956049e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3010451")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3010567")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTSALQSIEFAMFCRTDKALAPHPASTKRTYLTI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQ IE A FCR DKA  PHPA T    L +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.0541877513661")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("105")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.7306238810316e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("87")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("983529")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("983672")
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
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("52")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIAGGIF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTRPLQYFEFARFCRPDKALVPHPA*TKRTLPEI----QASIKSGLF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQY E A FCR DKA  PHPA T      +    Q +I  G+F")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.0541877513661")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("105")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.76188149555829e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4247386")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4247469")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PDATLARLIRPT-------TAVKCRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PDATLARLIRPT       +A  CR DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("44.6689881754487")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("104")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.4823074964578e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("769891")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("770019")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("45")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RVTIRHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPALT")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RVKINAKLPDAL--RLPVSTSSLQYIEFACFCRPNKAFMPYPA*T")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RV I    PDA   RL   T  LQYIE ACFCR +KAF P+PA T")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("44.6689881754487")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("104")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.29632392049334e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4323970")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4324059")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PDATLARLIRPT------NSRTLRRPDKAFTPHPAM")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PDATLARLIRPT       S    R DKAFTPHPA+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.61487446016127e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("87")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3148632")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3148796")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("11")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ILQYIESACFCRQDKAFTPHPALTANLTLLVAFK-----------HQFAIAGGIF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LLQHVESSCVCRPDKAFTTHPALGAQCLMRYAYQAYKIYCNMLNLHAFVGG*GVF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+LQ++ES+C CR DKAFT HPAL A   +  A++           H F    G+F")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.90312416347839e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4078036")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4078131")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALT")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AALIRPTNKLQYIEFIRLCRPDKAFTPHPA*T")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE    CR DKAFTPHPA T")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.57944555743798e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2547497")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2547592")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALT")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTWSPQYIELARFRRPDKAFTPHPACT")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT   QYIE A F R DKAFTPHPA T")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.898589023614")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.6456585733925e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("70")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4407160")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4407258")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LIRPTLILQYIESACFCRQDKAFTPHPALTANL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FIRPT*SLQYIEFA*LCRPDKAFTPHPA*TKRI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" IRPT  LQYIE A  CR DKAFTPHPA T  +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.898589023614")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.77431600062704e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3561614")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3561703")
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
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTWFMQYIEFAWSCRPDKTFTSHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  +QYIE A  CR DK FT HPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.898589023614")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.1734245286225e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("83")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("489134")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("489271")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPHPALTANLTLLVAFKHQFAIA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPTPLLQYIEFLRFRRPDKAVTPHPA*TTR--TLLAISGQLNVA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT +LQYIE   F R DKA TPHPA T     L+A   Q  +A")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("43")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.898589023614")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.18618274898239e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4324273")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4324359")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PDATLARLIRPT------NSRTLRRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PDATLARLIRPT       S    R DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("32.7278013220108")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("73")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.18751655564654e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("52")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("111539")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("111592")
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
									pcdata("12")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("13")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CFCRQDKAFTPHPALTAN")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CFCRSDKAFTPHPTFAQD")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CFCR DKAFTPHP    +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("45")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("29.2610051387546")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("64")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.18751655564654e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("111454")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("111519")
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
									pcdata("14")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("16")
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
									pcdata("RVTIRHSAPDATLARLIRPTLI")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RIRHSHTMPDATLARLIRPKGI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R+   H+ PDATLARLIRP  I")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.1281898717793")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("100")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.27191245390812e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("374150")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("374251")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RAQMPDATLTRLIRPT------NSHAIRRPDKAFTPHPAV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R   PDATL RLIRPT       S    R DKAFTPHPA+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("42.742990295862")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.82084902033859e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("374453")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("374554")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RVQMPDATLTRLIRPT------NSRAIRRPDKAFTPHPAV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R   PDATL RLIRPT       S    R DKAFTPHPA+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("48")
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
									pcdata("2.6727699978414e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4324173")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4324259")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PDATLARLIRPT------NSRTLRRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PDATLARLIRPT       S    R DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("49")
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
									pcdata("2.6727699978414e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4323873")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4323959")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PDATLARLIRPT------NSRTLRRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PDATLARLIRPT       S    R DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("41.9725911440273")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("97")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.4329772479704e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("374251")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("374352")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RAQMPDATLTRLIRPT------NSRAIRRPDKAFTPHPAV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R   PDATL RLIRPT       S    R DKAFTPHPA+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("51")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("41.5873915681099")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("96")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.05642842025089e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4324073")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4324159")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PDATLARLIRPT------NSRTIRRPDKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PDATLARLIRPT       S    R DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("52")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("41.2021919921926")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.12396158536703e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3201165")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3201257")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QSTPDATLARLIRPTTVVDM*P-------DKAFTPHPA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" S PDATLARLIRPT ++           DKAFTPHPA")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("53")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("41.2021919921926")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.2101851210499e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("63")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3738996")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3739079")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARLIRPTLILQYIESACFCRQDKAFTPH")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATLIRPT*FLQYIEFGRICRPDKAFTPH")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A LIRPT  LQYIE    CR DKAFTPH")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("40.8169924162752")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.10523522677952e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("374352")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("374453")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RAQMPDATLTRLIRPT------NSRATRRPDKAFTPHPAV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R   PDATL RLIRPT       S    R DKAFTPHPA+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("40.4317928403579")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.04049289777701e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4092452")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4092658")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("71")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CRVTIRHSAPDATLARLIRPTLIL--------------QYIESAC----------FCRQDKAFTPHPALTA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CR--IRQSMPDATLSRLIRPTTVADL*AG*GTCAASGNQCLMRRCRVLSGLQLLPTCRPDKALVPHPAINA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CR  IR S PDATL+RLIRPT +               Q +   C           CR DKA  PHPA+ A")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("56")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("40.4317928403579")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.26583645664482e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("507941")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("508045")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("8")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IRHSAPDATLARLIRPTLILQYIESACFCRQ-DKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IRAPLPDATLTRLIRPT-------NLCLNRRPDKAFTPHPAV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IR   PDATL RLIRPT       + C  R+ DKAFTPHPA+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("40.4317928403579")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.74151487627169e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4216496")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4216594")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSAPDATLARLIRPTLILQYIESACFCRQDKAFTPHPAL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RCTMPDATLARLIMPTAVAER-------RLDKAFTPHPAI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R + PDATLARLI PT + +        R DKAFTPHPA+")
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
							pcdata("120625362")
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
					pcdata("28")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("28")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 5947 6093 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("49")
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
									pcdata("102.063724987134")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("253")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.10614337932439e-24")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("5947")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6093")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RQINHHIIALCCERLVQLVGDDISPELIQIFPSGILKTDPHFQTIRLHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RQINHHIIALCCERLVQLVGDDISPELIQIFPSGILKTDPHFQTIRLH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RQINHHIIALCCERLVQLVGDDISPELIQIFPSGILKTDPHFQTIRLH ")
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
							pcdata("38663350")
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
					pcdata("29")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("29")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6094 6378 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("95")
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
									pcdata("190.659627448126")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("483")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.93145506696926e-50")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("6094")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6378")
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
									pcdata("94")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IERAEYAIQAGKHSQMLLGKIEIVFAEGFGLQAGVDITFKRQNRLAGIRRREIWLPVMKASGVDTRQFVADAHQRANLRRRQFPRLMDQLLGIVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IERAEYAIQAGKHSQMLLGKIEIVFAEGFGLQAGVDITFKRQNRLAGIRRREIWLPVMKASGVDTRQFVADAHQRANLRRRQFPRLMDQLLGIV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IERAEYAIQAGKHSQMLLGKIEIVFAEGFGLQAGVDITFKRQNRLAGIRRREIWLPVMKASGVDTRQFVADAHQRANLRRRQFPRLMDQLLGIV ")
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
							pcdata("38662200")
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
					pcdata("30")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("30")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6379 6444 +1")
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
									pcdata("39.6613936885231")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("91")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.93362973434557e-05")
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
									pcdata("6379")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6444")
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
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
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
									pcdata("QLRQRIARGGQRALVIKRFRRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QLRQRIARGGQRALVIKRFRR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("QLRQRIARGGQRALVIKRFRR ")
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
					pcdata("31")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("31")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6445 6504 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("20")
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
									pcdata("41.2021919921926")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.53961697252409e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("6445")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6504")
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
									pcdata("20")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NKNQHIQSLQEIYADFSKKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NKNQHIQSLQEIYADFSKK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NKNQHIQSLQEIYADFSKK ")
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
							pcdata("30931160")
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
					pcdata("32")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("32")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6505 6528 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("8")
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
							pcdata("12372464")
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
					pcdata("33")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("33")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6529 6636 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("36")
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
									pcdata("70.8625593378288")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("172")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.08652174346046e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("6529")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6636")
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
									pcdata("35")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LLLRNIIPRVRRKLATDIRITQWIKHRAHAEFTLAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LLLRNIIPRVRRKLATDIRITQWIKHRAHAEFTLA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LLLRNIIPRVRRKLATDIRITQWIKHRAHAEFTLA ")
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
							pcdata("38663675")
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
					pcdata("34")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("34")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6637 6678 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("14")
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
							pcdata("21651812")
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
					pcdata("35")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("35")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6679 6753 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("25")
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
									pcdata("53.1433788456305")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("126")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.0058972570174e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("6679")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6753")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NGGQIGNSHAGHYDICQLPHQRKAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NGGQIGNSHAGHYDICQLPHQRKA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NGGQIGNSHAGHYDICQLPHQRKA ")
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
							pcdata("38663950")
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
					pcdata("36")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("36")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6754 6933 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("60")
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
									pcdata("120.938504207085")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("302")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.67229480577656e-29")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("60")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("6754")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("6933")
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
									pcdata("58")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("60")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QGAADDRCEGADPQTPDRFRVVQAXKEEIVFGINVVGNDGAEGKQNNHKGNKLSTPGTHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QGAADDRCEGADPQTPDRFRVVQA*KEEIVFGINVVGNDGAEGKQNNHKGNKLSTPGTH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("QGAADDRCEGADPQTPDRFRVVQA KEEIVFGINVVGNDGAEGKQNNHKGNKLSTPGTH ")
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
							pcdata("38663075")
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
					pcdata("37")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("37")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 6934 7005 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("50.8321813901264")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("120")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.97335571480543e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("6934")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("7005")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HPHRLLDKLNTFQRHVGCAVTRQX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("HPHRLLDKLNTFQRHVGCAVTRQ*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("HPHRLLDKLNTFQRHVGCAVTRQ ")
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
							pcdata("37117392")
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
					pcdata("38")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("38")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 7006 7194 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("63")
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
									pcdata("77.0257525525064")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("188")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.50543825067642e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("63")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("7006")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("7194")
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
									pcdata("62")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("62")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("63")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("YQHGACRTDDQGVDKNANHLDNPLXXXXXXXXXXXXXXXXXXXXXXLIGKHTALKTVSNRLAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("YQHGACRTDDQGVDKNANHLDNPLRCRMRRPGRRCRCRVWRRTHSRLIGKHTALKTVSNRLA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("YQHGACRTDDQGVDKNANHLDNPLRCRMRRPGRRCRCRVWRRTHSRLIGKHTALKTVSNRLA ")
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
							pcdata("38663000")
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
					pcdata("39")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("39")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 7195 7773 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("193")
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
									pcdata("397.126600139827")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1019")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.50366796177332e-112")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("193")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("7195")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("7773")
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
									pcdata("192")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("192")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("193")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GISRRAACRFLPAKSTLKNRPNDVGKLPDIHYANYQAGQYPDYRHQRDKALHEPGDAMKTASDCQQSKDSENNACHQRGKIKRKTQGAGNGVRLNSAENYAIGDEQKDGEQNAHPAHPQPARHIPCRSATKLPIDVTFFIKLCQRTFGKRGRHADKRGNPHPKDGSRSTGGNSQRNAGQVAATHARRKTGTQX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GISRRAACRFLPAKSTLKNRPNDVGKLPDIHYANYQAGQYPDYRHQRDKALHEPGDAMKTASDCQQSKDSENNACHQRGKIKRKTQGAGNGVRLNSAENYAIGDEQKDGEQNAHPAHPQPARHIPCRSATKLPIDVTFFIKLCQRTFGKRGRHADKRGNPHPKDGSRSTGGNSQRNAGQVAATHARRKTGTQ*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("GISRRAACRFLPAKSTLKNRPNDVGKLPDIHYANYQAGQYPDYRHQRDKALHEPGDAMKTASDCQQSKDSENNACHQRGKIKRKTQGAGNGVRLNSAENYAIGDEQKDGEQNAHPAHPQPARHIPCRSATKLPIDVTFFIKLCQRTFGKRGRHADKRGNPHPKDGSRSTGGNSQRNAGQVAATHARRKTGTQ ")
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
							pcdata("171658836")
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
					pcdata("40")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("40")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 7774 7782 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("3")
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
							pcdata("4639674")
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
					pcdata("41")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("41")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 7783 7989 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("69")
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
									pcdata("144.050478762126")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("362")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.78409565286944e-36")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("7783")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("7989")
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
									pcdata("67")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GXTAWLWMNAIFKTFAKLADVAKLHKSGAKSEPTTCAEEQVNHYRSPKDAVNEGEKIWHAYPSYCRSRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("G*TAWLWMNAIFKTFAKLADVAKLHKSGAKSEPTTCAEEQVNHYRSPKDAVNEGEKIWHAYPSYCRSR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("G TAWLWMNAIFKTFAKLADVAKLHKSGAKSEPTTCAEEQVNHYRSPKDAVNEGEKIWHAYPSYCRSR ")
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
							pcdata("38662850")
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
					pcdata("42")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("42")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 7990 8061 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("46.9801856309529")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("110")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.39522203388096e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("7990")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8061")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LSCVNVTNCLRSIYATRYLITSAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LSCVNVTNCLRSIYATRYLITSA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LSCVNVTNCLRSIYATRYLITSA ")
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
							pcdata("37117392")
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
					pcdata("43")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("43")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8062 8178 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("39")
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
									pcdata("78.5665508561758")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("192")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.0877422178059e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8062")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8178")
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
									pcdata("36")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HTXGFVXKWRAISTQCQKSETVSPGDNRLVGGCADVASX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("HT*GFV*KWRAISTQCQKSETVSPGDNRLVGGCADVAS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("HT GFV KWRAISTQCQKSETVSPGDNRLVGGCADVAS ")
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
							pcdata("38663600")
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
					pcdata("44")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("44")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8179 8241 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("21")
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
									pcdata("40.8169924162752")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.74266853728993e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8179")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8241")
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
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("YHQGRPVTSPXQAVXREILSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("YHQGRPVTSP*QAV*REILS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("YHQGRPVTSP QAV REILS ")
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
							pcdata("32477718")
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
					pcdata("45")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("45")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8242 8310 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("23")
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
							pcdata("35570834")
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
					pcdata("46")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("46")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8311 8391 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("27")
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
									pcdata("54.2989775733826")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("129")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.81323047354856e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8311")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8391")
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
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SCINRRMPQPTLLSFLTQRRFRNTVSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SCINRRMPQPTLLSFLTQRRFRNTVS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SCINRRMPQPTLLSFLTQRRFRNTVS ")
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
							pcdata("38663900")
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
					pcdata("47")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("47")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8392 8475 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("28")
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
									pcdata("55.839775877052")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("133")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.28419809018113e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8392")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8475")
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
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMMLSPGRNSRATIARSRSWTRPTNWQX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMMLSPGRNSRATIARSRSWTRPTNWQ*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LMMLSPGRNSRATIARSRSWTRPTNWQ ")
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
							pcdata("38663875")
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
					pcdata("48")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("48")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8476 8496 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("7")
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
							pcdata("10825906")
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
					pcdata("49")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("49")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8497 8583 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("29")
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
									pcdata("59.3065720603082")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("142")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.77230457731374e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8497")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8583")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NWFRAVSQLKLMRVFPMTPKRQLRKQNAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NWFRAVSQLKLMRVFPMTPKRQLRKQNA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NWFRAVSQLKLMRVFPMTPKRQLRKQNA ")
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
							pcdata("38663850")
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
					pcdata("50")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("50")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8584 8628 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("15")
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
							pcdata("23198370")
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
					pcdata("51")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("51")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8629 8703 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("25")
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
									pcdata("49.6765826623743")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("117")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.32144560468719e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8629")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8703")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SNWLLPGRVSVLQNSWKKKASTVTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SNWLLPGRVSVLQNSWKKKASTVT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SNWLLPGRVSVLQNSWKKKASTVT ")
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
							pcdata("38663950")
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
					pcdata("52")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("52")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8704 8760 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("19")
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
									pcdata("38.5057949607711")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("88")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.89064493210975e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8704")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8760")
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
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PCCSPSLRLVLVRKRACSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PCCSPSLRLVLVRKRACS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PCCSPSLRLVLVRKRACS ")
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
							pcdata("29384602")
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
					pcdata("53")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("53")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8761 8964 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("68")
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
									pcdata("114.004911840572")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("284")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.2975284529239e-27")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8761")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("8964")
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
									pcdata("67")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SRRLLAVFLTGTKRIPIRKSTLRQKIRAWFLYLXXXXXXXXXVMKPWLWAQASVTSAKFWNWQAATVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SRRLLAVFLTGTKRIPIRKSTLRQKIRAWFLYLKSTSTTKSTVMKPWLWAQASVTSAKFWNWQAATV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SRRLLAVFLTGTKRIPIRKSTLRQKIRAWFLYLKSTSTTKSTVMKPWLWAQASVTSAKFWNWQAATV ")
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
							pcdata("38662875")
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
					pcdata("54")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("54")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 8965 9114 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("96.2857313483741")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("238")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.95760436354031e-22")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("8965")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("9114")
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
									pcdata("47")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PSHRHCXKSWRRAKGLSNVNCLTPAKXKRVRRVSLSPSSCGSTTRIQWQX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PSHRHC*KSWRRAKGLSNVNCLTPAK*KRVRRVSLSPSSCGSTTRIQWQ*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PSHRHC KSWRRAKGLSNVNCLTPAK KRVRRVSLSPSSCGSTTRIQWQ ")
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
							pcdata("38663325")
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
					pcdata("55")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("55")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9115 9174 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("20")
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
							pcdata("30931160")
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
					pcdata("56")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("56")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9175 9201 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("9")
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
							pcdata("13919022")
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
					pcdata("57")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("57")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9202 9291 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("30")
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
									pcdata("62.388168667647")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("150")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.7372852689427e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("9202")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("9291")
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
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RDREVGHATSSEACLSLPSQCIILFNETVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RDREVGHATSSEACLSLPSQCIILFNETV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RDREVGHATSSEACLSLPSQCIILFNETV ")
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
							pcdata("38663825")
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
					pcdata("58")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("58")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9292 9330 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("13")
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
							pcdata("20105254")
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
					pcdata("59")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("59")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9331 9402 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("49.291383086457")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("116")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.93644519449675e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("9331")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("9402")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FPSLIAHPAAFIRIKASLRWKNGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FPSLIAHPAAFIRIKASLRWKNG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FPSLIAHPAAFIRIKASLRWKNG ")
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
							pcdata("37117392")
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
					pcdata("60")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("60")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9403 9444 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("14")
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
							pcdata("21651812")
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
					pcdata("61")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("61")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9445 9501 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("19")
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
							pcdata("29384602")
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
					pcdata("62")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("62")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9502 9555 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("18")
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
							pcdata("27838044")
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
					pcdata("63")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("63")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9556 9579 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("8")
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
							pcdata("12372464")
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
					pcdata("64")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("64")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9580 9672 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("9580")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("9672")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RTARCLALVNRCARSACILYQLRSFRVRWAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RTARCLALVNRCARSACILYQLRSFRVRWA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RTARCLALVNRCARSACILYQLRSFRVRWA ")
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
							pcdata("38663800")
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
					pcdata("65")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("65")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9673 9744 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("47.7505847827876")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("112")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.7272411232444e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("9673")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("9744")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FANRRXSLTYPVSRSLLKRRWKVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FANRR*SLTYPVSRSLLKRRWKV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FANRR SLTYPVSRSLLKRRWKV ")
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
							pcdata("37117392")
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
					pcdata("66")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("66")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 9745 10299 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("185")
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
									pcdata("356.680644668505")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("914")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.91916879846777e-100")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("185")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("9745")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("10299")
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
									pcdata("184")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("185")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RTLRVTLWYTVFLPAYRTAFSCWKGHTLKRHRKWLQHSDRRVQDATLANKKIPPSGGISKQLVGFTNRQNGATKLLVQYFTHRQIDCAGTADQPNPAGKVDDCGVTGNVTDRQQGKQHGQAKENELQNACAFQRAEEHKQRKYAPQTQVDTKELCIWRIGXXXXXXXXXXXXXXPERTVRGECGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RTLRVTLWYTVFLPAYRTAFSCWKGHTLKRHRKWLQHSDRRVQDATLANKKIPPSGGISKQLVGFTNRQNGATKLLVQYFTHRQIDCAGTADQPNPAGKVDDCGVTGNVTDRQQGKQHGQAKENELQNACAFQRAEEHKQRKYAPQTQVDTKELCIWRIGQTQFRHQQNRNQRQPERTVRGECG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RTLRVTLWYTVFLPAYRTAFSCWKGHTLKRHRKWLQHSDRRVQDATLANKKIPPSGGISKQLVGFTNRQNGATKLLVQYFTHRQIDCAGTADQPNPAGKVDDCGVTGNVTDRQQGKQHGQAKENELQNACAFQRAEEHKQRKYAPQTQVDTKELCIWRIGQTQFRHQQNRNQRQPERTVRGECG ")
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
							pcdata("160833608")
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
					pcdata("67")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("67")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 10300 10521 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("74")
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
									pcdata("146.36167621763")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("368")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.97338553033001e-37")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("10300")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("10521")
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
									pcdata("73")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("73")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TESVAFFVLQQTSKNLRDAAVENAHGKNNTVQSEITHVVQVKQNGGHAEAHQAQRCRISQLSVAHNSSKIIIEX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TESVAFFVLQQTSKNLRDAAVENAHGKNNTVQSEITHVVQVKQNGGHAEAHQAQRCRISQLSVAHNSSKIIIE*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TESVAFFVLQQTSKNLRDAAVENAHGKNNTVQSEITHVVQVKQNGGHAEAHQAQRCRISQLSVAHNSSKIIIE ")
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
							pcdata("38662725")
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
					pcdata("68")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("68")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 10522 10533 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("4")
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
							pcdata("6186232")
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
					pcdata("69")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("69")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 10534 10620 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("29")
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
									pcdata("56.6101750288867")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("135")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.17533187533629e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("10534")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("10620")
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
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FPXITVVFSGRGIIISQWGSVYDLLRGKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FP*ITVVFSGRGIIISQWGSVYDLLRGK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FP ITVVFSGRGIIISQWGSVYDLLRGK ")
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
							pcdata("38663850")
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
					pcdata("70")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("70")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 10621 10662 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("14")
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
							pcdata("21651812")
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
					pcdata("71")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("71")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 10663 10728 +1")
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
									pcdata("42.742990295862")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.86951900436556e-06")
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
									pcdata("10663")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("10728")
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
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
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
									pcdata("PSGAGRRFAVRLESSRDRRCRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PSGAGRRFAVRLESSRDRRCR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PSGAGRRFAVRLESSRDRRCR ")
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
					pcdata("72")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("72")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 10729 11031 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("101")
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
									pcdata("154.836066887812")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("390")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.21220841023136e-39")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("101")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("10729")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("11031")
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
									pcdata("99")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("101")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PLXPHLVPHLQGLPAPRRAANHAQVRSLLRGCVKCGSVGWKANATHXXXXXXXXXXXXXXXXXHSPWHRPRAETPAYVPRPFSSRYYAEIVPVTVAQFQTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PL*PHLVPHLQGLPAPRRAANHAQVRSLLRGCVKCGSVGWKANATHLLPIICLSSAAAAALPAHSPWHRPRAETPAYVPRPFSSRYYAEIVPVTVAQFQT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PL PHLVPHLQGLPAPRRAANHAQVRSLLRGCVKCGSVGWKANATHLLPIICLSSAAAAALPAHSPWHRPRAETPAYVPRPFSSRYYAEIVPVTVAQFQT ")
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
							pcdata("44848094")
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
					pcdata("73")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("73")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11032 11211 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("60")
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
									pcdata("120.553304631167")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("301")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.09484186487329e-29")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("60")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("11032")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("11211")
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
									pcdata("59")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("59")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("60")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TSLCRLSASIALKHRAKWPDTIYRVRAVCWRYYRHQNAVILRQSAASCGDVARDALPSIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TSLCRLSASIALKHRAKWPDTIYRVRAVCWRYYRHQNAVILRQSAASCGDVARDALPSI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TSLCRLSASIALKHRAKWPDTIYRVRAVCWRYYRHQNAVILRQSAASCGDVARDALPSI ")
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
							pcdata("38663075")
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
					pcdata("74")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("74")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11212 11349 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("46")
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
									pcdata("96.6709309242914")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("239")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.70206758351028e-22")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("11212")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("11349")
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
									pcdata("44")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TVRCASVRWRGEFCLFHYGXAIGRNLPTVPHYNAEENPDLNHSGNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TVRCASVRWRGEFCLFHYG*AIGRNLPTVPHYNAEENPDLNHSGN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TVRCASVRWRGEFCLFHYG AIGRNLPTVPHYNAEENPDLNHSGN ")
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
							pcdata("38663425")
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
					pcdata("75")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("75")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11350 11385 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("76")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("76")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11386 11436 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("17")
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
							pcdata("26291486")
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
					pcdata("77")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("77")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11437 11547 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("37")
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
									pcdata("47.3653852068702")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("111")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.09075897179644e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("11437")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("11538")
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
									pcdata("34")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PEKVIFETQRXXXXXXXXXIQSVVCPSFISEGIF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PEKVIFETQRLLSLSLLMLIQSVVCPSFISEGIF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PEKVIFETQRLLSLSLLMLIQSVVCPSFISEGIF")
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
							pcdata("38663650")
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
					pcdata("78")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("78")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11548 11574 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("9")
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
							pcdata("13919022")
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
					pcdata("79")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("79")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11575 11832 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("86")
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
									pcdata("176.407243139184")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("446")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.52608816252563e-46")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("86")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("11575")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("11832")
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
									pcdata("85")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("85")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("86")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TDAPLNCKSPWSAESCMRFTMRQTGSFSARSLVSLRGIAMAPRSLWSFACAVQHSIRLIARLAEIVKTDFIRILLVGRGRGMNTHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TDAPLNCKSPWSAESCMRFTMRQTGSFSARSLVSLRGIAMAPRSLWSFACAVQHSIRLIARLAEIVKTDFIRILLVGRGRGMNTH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TDAPLNCKSPWSAESCMRFTMRQTGSFSARSLVSLRGIAMAPRSLWSFACAVQHSIRLIARLAEIVKTDFIRILLVGRGRGMNTH ")
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
							pcdata("38662425")
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
					pcdata("80")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("80")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11833 11928 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("32")
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
									pcdata("64.6993661231511")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("156")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.29756196537476e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("11833")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("11928")
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
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FTADKEKIQARNLFFLLQFFDECLGCDSFFIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FTADKEKIQARNLFFLLQFFDECLGCDSFFI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FTADKEKIQARNLFFLLQFFDECLGCDSFFI ")
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
							pcdata("38663775")
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
					pcdata("81")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("81")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 11929 12042 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("38")
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
									pcdata("73.1737567933329")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("178")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.90004656109229e-15")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("11929")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("12042")
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
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IKLLSILRLVLPYREISAQKHKKFLHLPLDDVVYDPIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IKLLSILRLVLPYREISAQKHKKFLHLPLDDVVYDPI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IKLLSILRLVLPYREISAQKHKKFLHLPLDDVVYDPI ")
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
							pcdata("38663625")
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
					pcdata("82")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("82")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 12043 12078 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("83")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("83")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 12079 12162 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("28")
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
									pcdata("62.388168667647")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("150")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.43974973817146e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("12079")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("12162")
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
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NWAVETRRFAPITDSQPHDDRIYSGDVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NWAVETRRFAPITDSQPHDDRIYSGDV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NWAVETRRFAPITDSQPHDDRIYSGDV ")
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
							pcdata("38663875")
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
					pcdata("84")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("84")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 12163 14079 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("639")
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
									pcdata("1190.25252695366")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("3078")
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
									pcdata("601")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("12163")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("13965")
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
									pcdata("601")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("601")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("601")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("MGKIIGIDLGTTNSCVAIMDGTTPRVLENAEGDRTTPSIIAYTQDGETLVGQPAKRQAVTNPQNTLFAIKRLIGRRFQDEEVQRDVSIMPFKIIAADNGDAWVEVKGQKMAPPQISAEVLKKMKKTAEDYLGEPVTEAVITVPAYFNDAQRQATKDAGRIAGLEVKRIINEPTAAALAYGLDKGTGNRTIAVYDLGGGTFDISIIEIDEVDGEKTFEVLATNGDTHLGGEDFDSRLINYLVEEFKKDQGIDLRNDPLAMQRLKEAAEKAKIELSSAQQTDVNLPYITADATGPKHMNIKVTRAKLESLVEDLVNRSIEPLKVALQDAGLSVSDIDDVILVGGQTRMPMVQKKVAEFFGKEPRKDVNPDEXXXXXXXXXXXXLTGDVKDVLLLDVTPLSLGIETMGGVMTTLIAKNTTIPTKHSQVFSTAEDNQSAVTIHVLQGERKRAADNKSLGQFNLDGINPAPRGMPQIEVTFDIDADGILHVSAKDKNSGKEQKITIKASSGLNEDEIQKMVRDAEANAEADRKFEELVQTRNQGDHLLHSTRKQVEEAGDKLPADDKTAIESALTALETALKGEDKAAIEAKMQELAQVSQKLMEI")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("MGKIIGIDLGTTNSCVAIMDGTTPRVLENAEGDRTTPSIIAYTQDGETLVGQPAKRQAVTNPQNTLFAIKRLIGRRFQDEEVQRDVSIMPFKIIAADNGDAWVEVKGQKMAPPQISAEVLKKMKKTAEDYLGEPVTEAVITVPAYFNDAQRQATKDAGRIAGLEVKRIINEPTAAALAYGLDKGTGNRTIAVYDLGGGTFDISIIEIDEVDGEKTFEVLATNGDTHLGGEDFDSRLINYLVEEFKKDQGIDLRNDPLAMQRLKEAAEKAKIELSSAQQTDVNLPYITADATGPKHMNIKVTRAKLESLVEDLVNRSIEPLKVALQDAGLSVSDIDDVILVGGQTRMPMVQKKVAEFFGKEPRKDVNPDEAVAIGAAVQGGVLTGDVKDVLLLDVTPLSLGIETMGGVMTTLIAKNTTIPTKHSQVFSTAEDNQSAVTIHVLQGERKRAADNKSLGQFNLDGINPAPRGMPQIEVTFDIDADGILHVSAKDKNSGKEQKITIKASSGLNEDEIQKMVRDAEANAEADRKFEELVQTRNQGDHLLHSTRKQVEEAGDKLPADDKTAIESALTALETALKGEDKAAIEAKMQELAQVSQKLMEI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("MGKIIGIDLGTTNSCVAIMDGTTPRVLENAEGDRTTPSIIAYTQDGETLVGQPAKRQAVTNPQNTLFAIKRLIGRRFQDEEVQRDVSIMPFKIIAADNGDAWVEVKGQKMAPPQISAEVLKKMKKTAEDYLGEPVTEAVITVPAYFNDAQRQATKDAGRIAGLEVKRIINEPTAAALAYGLDKGTGNRTIAVYDLGGGTFDISIIEIDEVDGEKTFEVLATNGDTHLGGEDFDSRLINYLVEEFKKDQGIDLRNDPLAMQRLKEAAEKAKIELSSAQQTDVNLPYITADATGPKHMNIKVTRAKLESLVEDLVNRSIEPLKVALQDAGLSVSDIDDVILVGGQTRMPMVQKKVAEFFGKEPRKDVNPDEAVAIGAAVQGGVLTGDVKDVLLLDVTPLSLGIETMGGVMTTLIAKNTTIPTKHSQVFSTAEDNQSAVTIHVLQGERKRAADNKSLGQFNLDGINPAPRGMPQIEVTFDIDADGILHVSAKDKNSGKEQKITIKASSGLNEDEIQKMVRDAEANAEADRKFEELVQTRNQGDHLLHSTRKQVEEAGDKLPADDKTAIESALTALETALKGEDKAAIEAKMQELAQVSQKLMEI")
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
									pcdata("415.230980207943")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1066")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.47622892395242e-117")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("596")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2655179")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2656891")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("249")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("362")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("593")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GIDLGTTNSCVAIMDGTTPRVLENAEGDRTTPSIIAYTQDGETLVGQPAKRQAVTNPQNTLFAIKRLIGRRFQDEEVQRDVSIMPFKIIAADNGDAWVEVKGQKMAPPQISAEVLKKMKKTAEDYLGEPVTEAVITVPAYFNDAQRQATKDAGRIAGLEVKRIINEPTAAALAYGLDKGTGNRTIAVYDLGGGTFDISIIEIDEVDGEKTFEVLATNGDTHLGGEDFDSRLINYLVEEFKKDQGIDLRNDPLAMQRLKEAAEKAKIELSSAQQTDVNLPYITADATGPKHMNIKVTRAKLESLVEDLVNRSIEPLKVALQDAGLSVSDIDDVILVGGQTRMPMVQKKVAEFFGKEPRKDVNPDEXXXXXXXXXXXXLTGDVKD--VLLLDVTPLSLGIETMGGVMTTLIAKNTTIPTKHSQVFSTAEDNQSAVTIHVLQGERKRAADNKSLGQFNLDGINPAPRGMPQIEVTFDIDADGILHVSAKDKNSGKEQKITIKASSGLNEDEIQKMVRDAEANAEADRKFEELVQTRNQGDHLLHSTRKQVEEAGDKLPADDKTAIESALTALETALKGEDKAAIEAKMQELAQVSQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GIDLGTTNSLVATVRSGQAETLADHEGRHLLPSVVHYQQQGHS-VGYDARTNAALDTANTISSVKRLMGRSLAD--IQQRYPHLPYQFQASENGLPMIETAAGLLNPVRVSADILKALAARATEALAGELDGVVITVPAYFDDAQRQGTKDAARLAGLHVLRLLNEPTAAAIAYGLDSGQ-EGVIAVYDLGGGTFDISILRLS----RGVFEVLATGGDSALGGDDFDHLLADYIREQ----AGIPDRSDNRVQRELLDAAIAAKIALSDADSVTVNV----AGWQG------EISREQFNELIAPLVKRTLLACRRALKDAGVEADEVLEVVMVGGSTRVPLVRERVGEFFGRPPLTSIDPDKVVAIGAAIQADILVGNKPDSEMLLLDVIPLSLGLETMGGLVEKVIPRNTTIPVARAQDFTTFKDGQTAMSIHVMQGERELVQDCRSLARFALRGIPALPAGGAHIRVTFQVDADGLLSVTAMEKSTGVEASIQVKPSYGLTDSEIASMIKDSMSYAEQDVKARMLAEQKVEAARVLESLHGALAADAALLSAAERQVIDDAAAHLSEVAQGDDVDAIEQAIKNVDKQTQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("GIDLGTTNS VA +       L + EG    PS++ Y Q G + VG  A+  A  +  NT+ ++KRL+GR   D  +Q+    +P++  A++NG   +E     + P ++SA++LK +   A + L   +   VITVPAYF+DAQRQ TKDA R+AGL V R++NEPTAAA+AYGLD G     IAVYDLGGGTFDISI+ +        FEVLAT GD+ LGG+DFD  L +Y+ E+     GI  R+D    + L +AA  AKI LS A    VN+    A   G      +++R +   L+  LV R++   + AL+DAG+   ++ +V++VGG TR+P+V+++V EFFG+ P   ++PD+ VAIGAA+Q  +L G+  D  +LLLDV PLSLG+ETMGG++  +I +NTTIP   +Q F+T +D Q+A++IHV+QGER+   D +SL +F L GI   P G   I VTF +DADG+L V+A +K++G E  I +K S GL + EI  M++D+ + AE D K   L + + +   +L S    +      L A ++  I+ A   L    +G+D  AIE  ++ + + +Q")
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
									pcdata("254.602757050406")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("649")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.18823677029984e-68")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("585")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("680994")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("682595")
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
									pcdata("177")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("301")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("591")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IGIDLGTTNSCVAIMDGTTPRVLENAEGDRTTPSIIAYTQDGETLVGQPAKRQAVTNPQNTLFAIKRLIGRRFQDEEVQRDVSIMPFKIIAADNGDAWVEVKGQKMAPPQISAEVLKKMKKTAEDYLGEPVTEAVITVPAYFNDAQRQATKDAGRIAGLEVKRIINEPTAAALAYGLDKGTGNRTIAVYDLGGGTFDISIIEIDEVDGEKTFEVLATNGDTHLGGEDFDSRLINYLVEEFKKDQGIDLRNDPLAMQRLKEAAEKAKIELSSAQQTDVNLPYITADATGPKHMNIKVTRAKLESLVEDLVNRSIEPLKVALQDAGLSVSDIDDVILVGGQTRMPMVQKKVAEFFGKEPRKDVNPDEXXX--XXXXXXXXXLTGDVKDVLLLDVTPLSLGIET----MGGVMTTLIAKNTTIPTKHSQVFSTAEDNQSAVTIHVLQGERKRAADNKSLGQFNLDGINPAPR--GMPQIEVTFDIDADGILHVSAKDKNSGKEQKITIKASSGLNEDEIQKMVRDAEANAEADRKFEEL-VQTRNQGDHLLHSTRK-QVEEAGDKLPADDKTAIESALTALETALKGEDKAAIE")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IGIDLGTTNSLIAVWKDGAAQLIPNKFGEYLTPSIISMDENNHILVGKPAVSRRTSHPDKTAALFKRAMG-----------------------SNTNW-RLGSDTFNAPELSSLVLRSLKEDAEEFLQRPIKDVVISVPAYFSDEQRKHTRLAAELAGLNAVRLINEPTAAAMAYGLHTQQNTRSL-VFDLGGGTFDVTVLEY----ATPVIEVHASAGDNFLGGEDFTHMLVDEVLK--RADVARTTLNES-ELAALYACVEAAK----CSNQSPLHIRWQYQEET----RECEFYENELEDLWLPLLNRLRVPIEQALRDARLKPSQIDSLVLVGGASQMPLVQRIAVRLFGKLPYQSYDPSTIVALGAAIQAACRLRSEDIEEVILTDICPYSLGVEVNRQGVSGIFSPIIERNTTVPVSRVETYSTMHPEQDSITVNVYQGENHKVKNNILVESFDV----PLKKTGAYQSIDIRFSYDINGLLEVDVLLEDGSVKSRVINHSPVTLSAQQIE----------ESRTRLSALKIYPR---DMLINRTFKAKLEELWARALGDEREEIGRVITDFDAALQSNDMARVD")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IGIDLGTTNS +A+      +++ N  G+  TPSII+  ++   LVG+PA  +  ++P  T    KR +G                       +   W  +       P++S+ VL+ +K+ AE++L  P+ + VI+VPAYF+D QR+ T+ A  +AGL   R+INEPTAAA+AYGL      R++ V+DLGGGTFD++++E          EV A+ GD  LGGEDF   L++ +++  + D      N+   +  L    E AK     + Q+ +++ +   + T       +    +LE L   L+NR   P++ AL+DA L  S ID ++LVGG ++MP+VQ+     FGK P +  +P   VA          + + D+++V+L D+ P SLG+E     + G+ + +I +NTT+P    + +ST    Q ++T++V QGE  +  +N  +  F++    P  +      I++ F  D +G+L V    ++   + ++   +   L+  +I+          E+  +   L +  R   D L++ T K ++EE   +   D++  I   +T  + AL+  D A ++")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
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
									pcdata("8.88525698056308e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("355")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2145701")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2146960")
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
									pcdata("102")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("174")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("92")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("432")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IIGIDLGTTNSCVAIMDGTTPRVLENAEGDRTTPSIIAYTQDGETLVGQPAKRQAVTNPQNTLFAIKRLIGRRFQDEEVQRDVSIMPFKIIAA-----DNGDAWVEVKGQK-------MAPPQIS------AEVLKKMKKTAEDYLGEPVTEAVITVPAYF-----NDAQRQA---TKDAGRIAGLEVKRIINEPTAAALAYGLDKGTGNRTIAVYDLGGGTFDISIIEI------------------------DEVDGEKTFE----VLATNGDTHLG-------------------GEDFDS----RLINYLVEEFKKDQGIDLRNDPLAMQ---RLKEAAEKAKIELSSAQQTDVNLPYITADATGPKHMNIKVTRAKLESLVEDLVNRSIEPLKVALQDAGLSVSDIDDVILVGGQTRMPMVQKKVAE")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FIGFDYGTANCSVAVMRDGKPHLLKMENDSTLLPSMLC-APTREAVSEWLYRHHDVPADDDETQALLRRAIRYNREEDIDVTAKSVQFGLSSLAQYIDDPEEVWF-VKSPKSFLGASGLKPQQVALFEDLVCAMMLHIRQQAQAQLPEAITQAVIGRPINFQGLGGDEANTQAQGILERAAKRAGFRDVVFQYEPVAAGLDYEATLQEEKRVLVV-DIGGGTTDCSLLLMGPQWRSRLDREASLLGHSGCRIGGNDLDIALAFKNLMPLLGMGGETEKGIALPILPWWNAVAINDVPAQSDFYSSANGRLLNDLVRDAREPEKVALLQKVWRQRLSYRLVRSAEECKIALSSVAETRASLPFIS------NELATLISQRGLESALSQPLTRILEQVQLALDNAQ---EKPDVIYLTGGSARSPLIKKALAE")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" IG D GT N  VA+M    P +L+        PS++      E +     +   V    +   A+ R   R  ++E++      + F + +      D  + W  VK  K       + P Q++        ++  +++ A+  L E +T+AVI  P  F     ++A  QA    + A + AG        EP AA L Y        R + V D+GGGT D S++ +                        +++D    F+    +L   G+T  G                     DF S    RL+N LV + ++ + + L       +   RL  +AE+ KI LSS  +T  +LP+I+        +   +++  LES +   + R +E +++AL +A       D + L GG  R P+++K +AE")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("54.2989775733826")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("129")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.89433044028253e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("123")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("280")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3398423")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3398827")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("72")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("160")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("MKKTAEDYLGEPVTEAVITVPAYFNDAQRQATKDAGRIAGLEVKRIINEPTAAALAYGL--DKGTGNRTIAVYDLGGGTFDISIIEIDEVDGEKTFEVLATNGDTHLGGEDFDSRLINYLVEEFKKDQGIDLRNDPLAMQRLKEAAEKAKIELSSAQQTD")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IKQVHSNSFMRPSPRVLVCVPVGATQVERRAIRESAQGAGAREVFLIEEPMAAAIGAGLPVSEATGS---MVVDIGGGTTEVAVISLNGV---------VYSSSVRIGGDRFDEAIINYVRRNYGSLIG-------------EATAERIKHEIGSAYPGD")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+K+   +    P    ++ VP      +R+A +++ + AG     +I EP AAA+  GL   + TG+    V D+GGGT ++++I ++ V           +    +GG+ FD  +INY+   +    G             +  AE+ K E+ SA   D")
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
							pcdata("844369890")
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
					pcdata("85")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("85")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14080 14151 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("48.1357843587049")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("113")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.41379193341312e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("14080")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("14151")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SPYKRVIILTRAKGNFLSARAFIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SPYKRVIILTRAKGNFLSARAFI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SPYKRVIILTRAKGNFLSARAFI ")
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
							pcdata("37117392")
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
					pcdata("86")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("86")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14152 14277 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("42")
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
									pcdata("81.6481474635147")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("200")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.22216617962935e-17")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("14152")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("14277")
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
									pcdata("39")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GQFKKDGXARLLRDFRRFQNSGRAXNQKGLQTPGHEIPPGPX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GQFKKDG*ARLLRDFRRFQNSGRA*NQKGLQTPGHEIPPGP*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("GQFKKDG ARLLRDFRRFQNSGRA NQKGLQTPGHEIPPGP ")
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
							pcdata("38663525")
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
					pcdata("87")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("87")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14278 14310 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("11")
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
							pcdata("17012138")
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
					pcdata("88")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("88")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14311 14391 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("27")
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
									pcdata("52.3729796937958")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("124")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.66410090425511e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("14311")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("14391")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RDQGSLXSSDRLAKTCGIRSVWSCCVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RDQGSL*SSDRLAKTCGIRSVWSCCV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RDQGSL SSDRLAKTCGIRSVWSCCV ")
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
							pcdata("38663900")
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
					pcdata("89")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("89")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14392 14601 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("70")
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
									pcdata("79.3369500080105")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("194")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.16750411295767e-17")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("70")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("14482")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("14601")
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
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TWSSTCGARCXFTLXHGAHPRRSCTWRDQRDPHSDSGRVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TWSSTCGARC*FTL*HGAHPRRSCTWRDQRDPHSDSGRV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TWSSTCGARC FTL HGAHPRRSCTWRDQRDPHSDSGRV ")
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
							pcdata("38662825")
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
					pcdata("90")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("90")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14602 14616 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("5")
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
							pcdata("7732790")
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
					pcdata("91")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("91")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14617 14787 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("57")
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
									pcdata("114.775310992407")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("286")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.27053087153262e-27")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("14617")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("14787")
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
									pcdata("56")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("56")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RCKTRYTAADLSDLSWFWSGADAPGILRCTADLSTLSGPRYADQRSVQQMSWSWSCX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RCKTRYTAADLSDLSWFWSGADAPGILRCTADLSTLSGPRYADQRSVQQMSWSWSC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RCKTRYTAADLSDLSWFWSGADAPGILRCTADLSTLSGPRYADQRSVQQMSWSWSC ")
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
							pcdata("38663150")
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
					pcdata("92")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("92")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 14788 15024 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("79")
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
									pcdata("125.560899118093")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("314")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.63060564274774e-31")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("79")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("14788")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("15024")
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
									pcdata("74")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("79")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AQQNAVRXNPGRGGHWRPHPSCGRRXSGRAWRTXXXXXXXXXXXTAPDFRAXRQQPVLRSPDQLRYGGAGWRNRSTDPX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AQQNAVR*NPGRGGHWRPHPSCGRR*SGRAWRTGRRSVRSGSG*TAPDFRA*RQQPVLRSPDQLRYGGAGWRNRSTDP*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AQQNAVR NPGRGGHWRPHPSCGRR SGRAWRTGRRSVRSGSG TAPDFRA RQQPVLRSPDQLRYGGAGWRNRSTDP ")
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
							pcdata("38662600")
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
					pcdata("93")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("93")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 15025 15090 +1")
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
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.84041564318821e-06")
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
									pcdata("15025")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("15090")
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
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("20")
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
									pcdata("WSRQTESAWRNPDRXAIPYARX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("WSRQTESAWRNPDR*AIPYAR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("WSRQTESAWRNPDR AIPYAR ")
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
					pcdata("94")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("94")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 15091 15261 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("57")
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
									pcdata("69.3217610341594")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("168")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.54419641447729e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("15091")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("15261")
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
									pcdata("55")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RRQVCPRWRTGXFAVPRCRRNTGXXXXXXXXXXXXXXXXXXWPNRRAQQPALKELLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRQVCPRWRTG*FAVPRCRRNTGRPERKAETAAARAARKLRWPNRRAQQPALKELL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RRQVCPRWRTG FAVPRCRRNTGRPERKAETAAARAARKLRWPNRRAQQPALKELL ")
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
							pcdata("38663150")
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
					pcdata("95")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("95")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 15262 15351 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("30")
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
									pcdata("62.388168667647")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("150")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.49369778893867e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("15262")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("15351")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("WCEEVFXRPDPLTSPKACPWAGLGKNRVRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("WCEEVF*RPDPLTSPKACPWAGLGKNRVR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("WCEEVF RPDPLTSPKACPWAGLGKNRVR ")
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
							pcdata("38663825")
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
					pcdata("96")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("96")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 15352 15432 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("27")
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
									pcdata("53.5285784215479")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("127")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.77862446949091e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("15352")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("15432")
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
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RYASTCKVAGITPISANLRVVVLRLIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RYASTCKVAGITPISANLRVVVLRLI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RYASTCKVAGITPISANLRVVVLRLI ")
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
							pcdata("38663900")
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
					pcdata("97")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("97")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 15433 16557 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("375")
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
									pcdata("763.066197261313")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1969")
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
									pcdata("375")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("15433")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("16557")
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
									pcdata("374")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("374")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("375")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKN ")
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
									pcdata("763.066197261313")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1969")
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
									pcdata("375")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("607276")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("608400")
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
									pcdata("374")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("374")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("375")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKN ")
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
									pcdata("763.066197261313")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1969")
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
									pcdata("375")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2512341")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2513465")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("374")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("374")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("375")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FNVPMNYSHDNWSAILAHIGKPEELDTSARNAGALTRRREIRDAATLLRLGLAYGPGGMSLREVTAWAQLHDVATLSDVALLKRLRNAADWFGILAAQTLAVRAAVTGCTSGKRLRLVDGTAISAPGGGSAEWRLHMGYDPHTCQFTDFELTDSRDAERLDRFAQTADEIRIADRGFGSRPECIRSLAFGEADYIVRVHWRGLRWLTAEGMRFDMMGFLRGLDCGKNGETTVMIGNSGNKKAGAPFPARLIAVSLPPEKALISKTRLLSENRRKGRVVQAETLEAAGHVLLLTSLPEDEYSAEQVADCYRLRWQIELAFKRLKSLLHLDALRAKEPELAKAWIFANLLAAFLIDDIIQPSLDFPPRSAGSEKKN ")
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
							pcdata("443836890")
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
					pcdata("98")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("98")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 16558 16671 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("38")
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
									pcdata("77.4109521284238")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("189")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.44354189922946e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("16558")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("16671")
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
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASS ")
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
									pcdata("77.0257525525064")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("188")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.59051017427941e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("608401")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("608514")
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
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASS ")
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
									pcdata("76.6405529765891")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("187")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.8025326622794e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2513466")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2513579")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LVVENNKNGHLELTGGHSWDSIPDSLQNAIEEREASS ")
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
							pcdata("38663625")
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
					pcdata("99")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("99")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 16672 16773 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("34")
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
									pcdata("66.2401644268205")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("160")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.42259727472267e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("16672")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("16773")
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
									pcdata("33")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMGSLPAVALTRIRKPX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMGSLPAVALTRIRKP*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMGSLPAVALTRIRKP ")
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
									pcdata("48.1357843587049")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("113")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.28864539371664e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("608515")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("608601")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMGSLPAVALTR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMGVIPAYPVVR")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMG +PA  + R")
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
									pcdata("43.898589023614")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.66389074783981e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2513580")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2513654")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMGSLPAV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMG*FPGF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RGTEASHSSDGSTLKLALMG  P  ")
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
							pcdata("38663725")
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
					pcdata("100")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("100")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 16774 16920 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("49")
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
									pcdata("40.4317928403579")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.84857237923486e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("16774")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("16827")
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
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KQQPPSGQFGCEPHRGLF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KQQPPSGQFGCEPHRGLF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KQQPPSGQFGCEPHRGLF")
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
							pcdata("38663350")
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
					pcdata("101")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("101")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 16921 17025 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("35")
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
									pcdata("71.2477589137461")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("173")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.48201898712608e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("16921")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17025")
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
									pcdata("33")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PYGQXEALYMCSAYRRPRVDGKISLGAFLYLPFSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PYGQ*EALYMCSAYRRPRVDGKISLGAFLYLPFS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PYGQ EALYMCSAYRRPRVDGKISLGAFLYLPFS ")
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
							pcdata("38663700")
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
					pcdata("102")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("102")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17026 17142 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("39")
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
									pcdata("82.033347039432")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("201")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.35781547835399e-18")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17026")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17142")
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
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CLRQTASSTRRYYIALFNPFCFIDSNPEDASHFCGVIFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CLRQTASSTRRYYIALFNPFCFIDSNPEDASHFCGVIF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CLRQTASSTRRYYIALFNPFCFIDSNPEDASHFCGVIF ")
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
							pcdata("38663600")
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
					pcdata("103")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("103")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17143 17211 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("23")
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
									pcdata("41.5873915681099")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("96")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.13777756893825e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17143")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17211")
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
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XFNYLTLIYLFIAIIDDKLDYFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*FNYLTLIYLFIAIIDDKLDYF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" FNYLTLIYLFIAIIDDKLDYF ")
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
							pcdata("35570834")
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
					pcdata("104")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("104")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17212 17241 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("10")
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
							pcdata("15465580")
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
					pcdata("105")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("105")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17242 17268 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("9")
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
							pcdata("13919022")
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
					pcdata("106")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("106")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17269 17295 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("9")
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
							pcdata("13919022")
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
					pcdata("107")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("107")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17296 17310 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("5")
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
							pcdata("7732790")
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
					pcdata("108")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("108")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17311 17421 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("37")
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
									pcdata("67.7809627304899")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("164")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.52002259886231e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17311")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17421")
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
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AXGHSXALTAVKNASHRXWRKFFNSSXKTNYSYTIIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("A*GHS*ALTAVKNASHR*WRKFFNSS*KTNYSYTII*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("A GHS ALTAVKNASHR WRKFFNSS KTNYSYTII ")
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
							pcdata("38663650")
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
					pcdata("109")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("109")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17422 17514 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("57.3805741807214")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("137")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.59503188347067e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17422")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17514")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FXRXFVRGKIVKTIYSPEREIKSETSASILX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("F*R*FVRGKIVKTIYSPEREIKSETSASIL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("F R FVRGKIVKTIYSPEREIKSETSASIL ")
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
							pcdata("38663800")
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
					pcdata("110")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("110")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17515 17682 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("56")
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
									pcdata("117.086508447911")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("292")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.27779543747118e-28")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("56")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17515")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17682")
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
									pcdata("54")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("56")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QXCLGRHYSYHCRYPGDDYGQQRRNQWMVSRLSGDAGSAPGWFTRNQQKHAVMDKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("Q*CLGRHYSYHCRYPGDDYGQQRRNQWMVSRLSGDAGSAPGWFTRNQQKHAVMDK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("Q CLGRHYSYHCRYPGDDYGQQRRNQWMVSRLSGDAGSAPGWFTRNQQKHAVMDK ")
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
							pcdata("38663175")
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
					pcdata("111")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("111")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17683 17727 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("15")
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
							pcdata("23198370")
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
					pcdata("112")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("112")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17728 17835 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("36")
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
									pcdata("72.4033576414982")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("176")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.87825361717124e-15")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17728")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("17835")
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
									pcdata("34")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TXTDARIASQLTPGRISSYRRYWWDDCAGITLSGFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("T*TDARIASQLTPGRISSYRRYWWDDCAGITLSGF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("T TDARIASQLTPGRISSYRRYWWDDCAGITLSGF ")
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
							pcdata("38663675")
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
					pcdata("113")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("113")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17836 17886 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("17")
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
							pcdata("26291486")
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
					pcdata("114")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("114")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 17887 18018 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("44")
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
									pcdata("91.2781368614485")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("225")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.59480875647767e-20")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("17887")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("18018")
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
									pcdata("43")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("43")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HCFCTWCTGAVGKSCSVSAEDLFDGSGYYRRSWGHHYHRIVLHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("HCFCTWCTGAVGKSCSVSAEDLFDGSGYYRRSWGHHYHRIVLH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("HCFCTWCTGAVGKSCSVSAEDLFDGSGYYRRSWGHHYHRIVLH ")
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
							pcdata("38663475")
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
					pcdata("115")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("115")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 18019 18300 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("94")
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
									pcdata("155.606466039646")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("392")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.82832639524363e-40")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("18019")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("18300")
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
									pcdata("92")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("92")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XLIDGLSWRRGCSNCGTRGIESVWCTPHGRLYSCWRGVVDCGVEIXXXXXXXXXXXXLLYSFEREAWAFSSEATGACVAPVGGVSDFAAVCICX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*LIDGLSWRRGCSNCGTRGIESVWCTPHGRLYSCWRGVVDCGVEIGGSRNSGGGNCRLLYSFEREAWAFSSEATGACVAPVGGVSDFAAVCIC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" LIDGLSWRRGCSNCGTRGIESVWCTPHGRLYSCWRGVVDCGVEIGGSRNSGGGNCRLLYSFEREAWAFSSEATGACVAPVGGVSDFAAVCIC ")
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
							pcdata("38662225")
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
					pcdata("116")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("116")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 18301 18402 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("34")
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
									pcdata("69.3217610341594")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("168")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.27689062977369e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("18301")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("18402")
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
									pcdata("33")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CWRFTARRHAGWLDLHSAIGDHRWLADWQTAGDX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CWRFTARRHAGWLDLHSAIGDHRWLADWQTAGD*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CWRFTARRHAGWLDLHSAIGDHRWLADWQTAGD ")
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
							pcdata("38663725")
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
					pcdata("117")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("117")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 18403 18450 +1")
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
					pcdata("118")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("118")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 18451 18543 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("64.3141665472338")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("155")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.05318857875295e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("18451")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("18543")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GNDLSANYGGGDPVRYRFYYVYLYCQPGLWX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GNDLSANYGGGDPVRYRFYYVYLYCQPGLW*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("GNDLSANYGGGDPVRYRFYYVYLYCQPGLW ")
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
							pcdata("38663800")
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
					pcdata("119")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("119")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 18544 18681 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("46")
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
									pcdata("93.97453389287")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("232")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.48104880727395e-21")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("18544")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("18681")
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
									pcdata("44")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RRSRTDXLGETRYPGRFYLFGGNWIQLVTRSFASISLTGRFTGEPX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRSRTD*LGETRYPGRFYLFGGNWIQLVTRSFASISLTGRFTGEP*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RRSRTD LGETRYPGRFYLFGGNWIQLVTRSFASISLTGRFTGEP ")
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
							pcdata("38663425")
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
					pcdata("120")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("120")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 18682 19620 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("313")
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
									pcdata("652.513918973032")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1682")
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
									pcdata("313")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("18682")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("19620")
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
									pcdata("312")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("312")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("313")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TAPFSLLSGREMSMSHINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMFTLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRCFESTHEMLLEQLSQHKLDMIISDCPIDSTQQEGLFSVRIGECGVSFWCTNPPPEKPFPACLEERRLLIPGRRSMLGRKLLNWFNSQGLNVEILGEFDDAALMKAFGAMHNAIFVAPTLYAYDFYADKTVVEIGRVENVMEEYHAIFAERMIQHPAVQRICNTDYSALFSPAVRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TAPFSLLSGREMSMSHINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMFTLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRCFESTHEMLLEQLSQHKLDMIISDCPIDSTQQEGLFSVRIGECGVSFWCTNPPPEKPFPACLEERRLLIPGRRSMLGRKLLNWFNSQGLNVEILGEFDDAALMKAFGAMHNAIFVAPTLYAYDFYADKTVVEIGRVENVMEEYHAIFAERMIQHPAVQRICNTDYSALFSPAVR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TAPFSLLSGREMSMSHINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMFTLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRCFESTHEMLLEQLSQHKLDMIISDCPIDSTQQEGLFSVRIGECGVSFWCTNPPPEKPFPACLEERRLLIPGRRSMLGRKLLNWFNSQGLNVEILGEFDDAALMKAFGAMHNAIFVAPTLYAYDFYADKTVVEIGRVENVMEEYHAIFAERMIQHPAVQRICNTDYSALFSPAVR ")
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
									pcdata("55.0693767252173")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("131")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.93525217839418e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("173")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("357441")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("357905")
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
									pcdata("40")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("158")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMF----TLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRCFESTHEMLLEQLSQHKLDMIISDCPIDSTQQEGL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RHINYFLAVAEHGSFTRAASALHVSQPALSQQIRQLEESLGVPLFDRSGRTIRLTDAGEVWRQYASRALQELGAGKRAIHDVADLTRGS---LRIAVTPTFTSYFIGPLMADFYARYPSITLQLQEMSQEKIEDMLCRDELDVGIAFAPVHSPELEAI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" H+ YF  V + GS   AA AL+++   ++ QIR LEE L   LF R GR +  ++ GE+  +YA +         + + D+ +  + S     + V    +   +  ++         I L+  E + E + + L + +LD+ I+  P+ S + E +")
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
									pcdata("53.9137779974652")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("128")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.17671457757575e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("16")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("161")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1402774")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1403214")
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
									pcdata("34")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("75")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("147")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMF-TLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRCFESTHEMLLEQLSQHKLDMIIS")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QVKIHQIRAFVEVARQGSIRGASRMLNMSQPALSKSIQELEEGLAAQLFFRRSKGVTLTDAGESFYQHASLILEELRAAQEDIRQRQGQLAGQINIGMGASISRSLMPAVISRFHQQHPQVKVRIMEGQLVSMINELRQGELDFTIN")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" +  + +  F  V ++GS+ GA+  L ++   ++  I+ LEE L  +LF R+ +G+  ++ GE  Y++A  +   L     DI   + +     ++G+  ++S+ L+ +V++    +   + +R  E     ++ +L Q +LD  I+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("53.1433788456305")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("126")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.65146758737541e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("192")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("636296")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("636811")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("84")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("185")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMFTLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVV----EGEPIHLRCFESTHEMLL------EQLSQHKLDMIISDCPIDSTQQEGLFSVRIGECGVSFWCTNPPP")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DLNLLVIFECIYQHLSISKAAESLYITPSAVSQSLQRLRAQFNDPLFIRSGKGIAPTTTGLNLHHHLEKNLRGLEQTINIVNKSELKKNFIIYG------PQLISCSNNSMLIRCLRQDSSVEIEC----HDILMSAENAEELLVHRKADLVITQMPVIS---RSVICMPLHTIRNTLICSNRHP")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+ N L  F  +Y+  S+  AAE+LY+TP  ++  ++ L  +    LF R G+G+ P+  G  ++ + +K     ++ ++IVN  +        G       +L+S   N+ ++    +   + + C    H++L+      E L   K D++I+  P+ S     +  + +     +  C+N  P")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("49.291383086457")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("116")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.45165412893325e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("13")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("175")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("660044")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("660529")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("47")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("176")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SMSHINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMFTLSQEMLDIV-NYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRCFESTHEMLL----------EQLSQHKLDMIISD--CPIDSTQQEGLFS")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TLRNIDLNLLTIFEAVYVHKGIVNAAKVLNLTPSAISQSIQKLRVIFPDPLFIRKGQGVTPTAFAMHLHEYISQGLESILGALDIEGSYDKQRTITI------ATTPSVGALVL--------PVIYRAIKTHYPQLLLRNPPISDAENQLSQFQTDLIIDNMFCTNRTVQHHVLFT")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("++ +I+ N L  F  VY    +V AA+ L LTP  I+  I+ L       LF RKG+G+ P+     ++ Y  +        LDI  +Y K+  +        A +  + + VL        P+  R  ++ +  LL           QLSQ + D+II +  C   + Q   LF+")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("45.4393873272834")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("106")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.74189492142594e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2475358")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2475621")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("8")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LLSGREMSMSHINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVY-RYADKMFTLSQEMLDIVN")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LLNGWQLSKMHT-------FEVAARHQSFALAAEELSLSPSAVSHRINQLEEELGIQLFVRSHRKVELTHEGKRVYWALKSSLDTLNQEILDIKN")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LL+G ++S  H        F    +  S   AAE L L+P  ++ +I  LEE L  +LF R  R +E +  G+ VY      + TL+QE+LDI N")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.1281898717793")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("100")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.71614331246492e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("161")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3945550")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3946023")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("77")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("160")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LLSGREMSMSHINYNHLYYFWHVYKEGSVVGAAEALYLTPQTITGQIRALEERLQGKLFKRKGRGLEPSELGELVYRYADKMFTLSQEMLDIVNYRKESNLLFDVGVADALSKRLVSSVLNAAVVEGEPIHLRC-FE---STHEMLLEQLSQHKLDMIIS")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IITTNNFAESTVDTELLKTFLEVSRTRHFGRAAESLYLTQSAVSFRIRQLENQLGVNLFTRHRNNIRLTAAGEKLLPYAETLMSTWQAARKEVAHTSRHN-EFSIGASASLWECMLNQWL-GRLYQNQDAHTGLQFEARIAQRQSLVKQLHERQLDLLIT")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+++    + S ++   L  F  V +      AAE+LYLT   ++ +IR LE +L   LF R    +  +  GE +  YA+ + +  Q     V +    N  F +G + +L + +++  L   + + +  H    FE   +  + L++QL + +LD++I+")
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
							pcdata("349502446")
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
					pcdata("121")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("121")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 19621 19683 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("21")
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
									pcdata("5.84140542885505e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("19621")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("19683")
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
									pcdata("21")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SAAPPKLRWGRXISCTLPRFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SAAPPKLRWGR*ISCTLPRF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SAAPPKLRWGR ISCTLPRF ")
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
							pcdata("32477718")
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
					pcdata("122")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("122")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 19684 19740 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("19")
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
									pcdata("40.4317928403579")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.25717802592458e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("19684")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("19740")
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
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LGSLFARLRHCFHMYAGEX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LGSLFARLRHCFHMYAGE*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LGSLFARLRHCFHMYAGE ")
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
							pcdata("29384602")
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
					pcdata("123")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("123")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 19741 19800 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("20")
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
									pcdata("43.5133894476967")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("101")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.12821485925321e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("19741")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("19800")
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
									pcdata("20")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IEESDYFLHGDAEKSSNCWX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IEESDYFLHGDAEKSSNCW*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IEESDYFLHGDAEKSSNCW ")
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
							pcdata("30931160")
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
					pcdata("124")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("124")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 19801 19848 +1")
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
					pcdata("125")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("125")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 19849 19965 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("39")
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
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.3309641035014e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("19849")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("19965")
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
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC ")
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
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.3309641035014e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("278440")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("278556")
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
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC ")
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
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.3309641035014e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("289911")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("290027")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.3309641035014e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3582049")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3582165")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.3309641035014e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1976580")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1976696")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIAC ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("74.3293555210849")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("181")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.05139304230299e-15")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1049599")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1049715")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGYTAQFAAYIAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCHAAPPILRTTATSVPAVPGAASDSGY AQFAAYIAC ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("51.6025805419611")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("122")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.30994603832305e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4517093")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4517206")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("38")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CHAAPPILRTTATSVPAVPGAASDSGYAAQFAAYIACX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CHATPPILRTTVTSVPALPDVVSDSDYVAQCAECNAC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CHA PPILRTT TSVPA+P   SDS Y AQ A   AC ")
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
							pcdata("38663600")
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
					pcdata("126")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("126")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 19966 20058 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("19966")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("20058")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS ")
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
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("278557")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("278649")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS ")
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
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("290028")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("290120")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3581956")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3582048")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1976697")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1976789")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.5373364099508e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1049506")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1049598")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAAFPSGGIHTAASHPSSISPRQRVTAGS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("42.742990295862")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.78783575219234e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4517003")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4517089")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RAAFPSGGIHTAASHPSSISPRQRVTAGS")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RAAFPSGVIHTAASHPSSIPRPQRPTAGS")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RAAFPSG IHTAASHPSSI   QR TAGS")
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
							pcdata("38663800")
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
					pcdata("127")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("127")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 20059 20160 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("34")
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
									pcdata("3.33022424386427e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("20059")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("20160")
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
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPASP*CVHRIRAQQPSSGDCHTRKTASAGAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAPASP CVHRIRAQQPSSGDCHTRKTASAGAI ")
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
									pcdata("3.33022424386427e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3581854")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3581955")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPASP*CVHRIRAQQPSSGDCHTRKTASAGAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAPASP CVHRIRAQQPSSGDCHTRKTASAGAI ")
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
									pcdata("3.33022424386427e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1976790")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1976891")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPASP*CVHRIRAQQPSSGDCHTRKTASAGAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAPASP CVHRIRAQQPSSGDCHTRKTASAGAI ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("63.543767395399")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("153")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.81920392565725e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("278650")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("278751")
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
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPASP*CVHRIRAQQPSSGACHTRKTASAGAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAPASP CVHRIRAQQPSSG CHTRKTASAGAI ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("63.543767395399")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("153")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.81920392565725e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("290121")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("290222")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPASP*CVHRIRAQQPSSGACHTRKTASAGAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAPASP CVHRIRAQQPSSG CHTRKTASAGAI ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("63.543767395399")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("153")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.81920392565725e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1049404")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1049505")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPASP*CVHRIRAQQPSSGACHTRKTASAGAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAPASP CVHRIRAQQPSSG CHTRKTASAGAI ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("46.5949860550355")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("109")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.39348531829557e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4516898")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4516999")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAPASPXCVHRIRAQQPSSGDCHTRKTASAGAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAPVWPECVHRRRAPQPSSVSCHTRKTASADVI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAP  P CVHR RA QPSS  CHTRKTASA  I ")
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
							pcdata("38663725")
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
					pcdata("128")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("128")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 20161 20268 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("36")
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
									pcdata("47.3653852068702")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("111")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.21651034189162e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("20161")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("20268")
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
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PRHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXVTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PRHSPTVRPFPRRR*RHCPAVCARLPTAA*VF*VT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PRHSPTVRPFPRRR RHCPAVCARLPTAA VF VT ")
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
									pcdata("47.3653852068702")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("111")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.21651034189162e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3581746")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3581853")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PRHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXVTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PRHSPTVRPFPRRR*RHCPAVCARLPTAA*VF*VT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PRHSPTVRPFPRRR RHCPAVCARLPTAA VF VT ")
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
									pcdata("47.3653852068702")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("111")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.21651034189162e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1976892")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1976999")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PRHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXVTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PRHSPTVRPFPRRR*RHCPAVCARLPTAA*VF*VT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PRHSPTVRPFPRRR RHCPAVCARLPTAA VF VT ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("40.8169924162752")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.14499850427025e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4516790")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4516894")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXVTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRSPTVRPFQRRQSHHCPVVCARLPTAA*VF*VT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R SPTVRPF RR+  HCP VCARLPTAA VF VT ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("39.6613936885231")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("91")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.58330386087767e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("278752")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("278853")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PRHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PRRIPTVRPFPRRR*RHCPAVCARLPTAA*VF*M")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PR  PTVRPFPRRR RHCPAVCARLPTAA VF +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("39.6613936885231")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("91")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.58330386087767e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("290223")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("290324")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PRHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PRRIPTVRPFPRRR*RHCPAVCARLPTAA*VF*M")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PR  PTVRPFPRRR RHCPAVCARLPTAA VF +")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("39.2761941126058")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("90")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.69368465345507e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1049302")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1049403")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PRHSPTVXXXXXXXXXHCPAVCARLPTAAXVFXV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PRRIPTVRPFPRRR*RHCPAVCARLPTAA*VF*M")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PR  PTVRPFPRRR RHCPAVCARLPTAA VF +")
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
							pcdata("38663675")
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
					pcdata("129")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("129")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 20269 20475 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("69")
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
									pcdata("141.739281306622")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("356")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.56373691001664e-36")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("20269")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("20475")
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
									pcdata("68")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS ")
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
									pcdata("140.968882154787")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("354")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.43657823013994e-35")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("290331")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("290537")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNAGGCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGSAFAVTHHPVSS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNAG CPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSG AFAVTHHPVSS ")
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
									pcdata("140.198483002952")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("352")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.96883703910296e-35")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3581539")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3581745")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("140.198483002952")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("352")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.96883703910296e-35")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1977000")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1977206")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("138.272485123366")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("347")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.38964800916576e-35")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1049089")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1049295")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGSAFAVTHHPVSS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSG AFAVTHHPVSS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("135.190888516027")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("339")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.21858221127671e-34")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("278860")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("279066")
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
									pcdata("66")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNAGGCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGSAFAVTHHPVSS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNAG CPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSG AFAVTHHPVSS ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("105.530521170391")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("262")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.15036301585441e-25")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4516586")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4516789")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("56")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NRVEANAHNAGCCPASNAIHGHINDFLVRTGLRSGVSELQLPCFTAVRAEIALMSGGAFAVTHHPVSS")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NRVEANAHNACTGATSDAIHGHINDFLVLTGLRGGVSEL*LPCFTARRAEIALMSGSAFAVTHHAFSS")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NRVEANAHNA     S+AIHGHINDFLV TGLR GVSEL LPCFTA RAEIALMSG AFAVTHH  SS")
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
							pcdata("38662850")
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
					pcdata("130")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("130")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 20476 20622 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("49")
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
									pcdata("101.2933258353")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("251")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.49052867595847e-23")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("20476")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("20622")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHLPQCVSQYPYSLWGSLEIEX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHLPQCVSQYPYSLWGSLEIE*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHLPQCVSQYPYSLWGSLEIE ")
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
									pcdata("64.6993661231511")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("156")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.31938220453229e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("290538")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("290624")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
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
									pcdata("62.7733682435643")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("151")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.22723443850331e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3581449")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3581538")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHL")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHL")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("60.8473703639776")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("146")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.30823867922811e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("279067")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("279153")
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
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("60.8473703639776")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("146")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.30823867922811e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1977207")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1977293")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHH")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("59.6917716362255")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("143")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.85048406495403e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1048993")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1049088")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TGGTADRNRSHWSTSKTPSYTKSVSWQHHLPQC")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TGGTADRNRSHWRTSKTPSYTKSVSWQHH-PYC")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TGGTADRNRSHW TSKTPSYTKSVSWQHH P C")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("39.6613936885231")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("91")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.02383179952593e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4516499")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4516579")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GGTADRNRSHWSTSKTPSYTKSVSWQH")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GRTSDGNGSHASTLKSPSYTKSVSWQH")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("G T+D N SH ST K+PSYTKSVSWQH")
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
							pcdata("38663350")
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
					pcdata("131")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("131")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 20623 20688 +1")
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
									pcdata("46.5949860550355")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("109")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.46886181220405e-07")
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
									pcdata("20623")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("20688")
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
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
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
									pcdata("IFLFRMYQPMVLRFLSHEEFRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IFLFRMYQPMVLRFLSHEEFR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IFLFRMYQPMVLRFLSHEEFR ")
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
					pcdata("132")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("132")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 20689 21063 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("125")
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
									pcdata("249.980362139398")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("637")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.19739735966327e-68")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("125")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("20689")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21063")
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
									pcdata("124")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("124")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("125")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("YNGMSLLTIWQPAVCSLTHHKSSRHKKTRLRGLFHKASANWRLSQFVDLCSQVSLMTCSFVFVDQTFSSLTVHDRLHFVKCFLCSSFVASFDSCVYFLDESTHHRATACVVLTSLFRLNGALLSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("YNGMSLLTIWQPAVCSLTHHKSSRHKKTRLRGLFHKASANWRLSQFVDLCSQVSLMTCSFVFVDQTFSSLTVHDRLHFVKCFLCSSFVASFDSCVYFLDESTHHRATACVVLTSLFRLNGALLS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("YNGMSLLTIWQPAVCSLTHHKSSRHKKTRLRGLFHKASANWRLSQFVDLCSQVSLMTCSFVFVDQTFSSLTVHDRLHFVKCFLCSSFVASFDSCVYFLDESTHHRATACVVLTSLFRLNGALLS ")
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
							pcdata("75777618")
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
					pcdata("133")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("133")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21064 21171 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("36")
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
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.90605273351568e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21064")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21171")
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
									pcdata("35")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FDISQGPTPKCVLYGQFKGRGICPFSLLLSMDLCKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FDISQGPTPKCVLYGQFKGRGICPFSLLLSMDLCK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FDISQGPTPKCVLYGQFKGRGICPFSLLLSMDLCK ")
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
							pcdata("38663675")
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
					pcdata("134")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("134")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21172 21399 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("76")
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
									pcdata("152.910069008225")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("385")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.59273015546638e-39")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21172")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21399")
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
									pcdata("75")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("75")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("APLMCRHSLRSDGAGFYQLAGCEYSFSAIKIAAGGQFLPVICAMAMKSHFFLISVLNRRLTLTAVQGILGRFSLFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("APLMCRHSLRSDGAGFYQLAGCEYSFSAIKIAAGGQFLPVICAMAMKSHFFLISVLNRRLTLTAVQGILGRFSLF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("APLMCRHSLRSDGAGFYQLAGCEYSFSAIKIAAGGQFLPVICAMAMKSHFFLISVLNRRLTLTAVQGILGRFSLF ")
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
							pcdata("38662675")
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
					pcdata("135")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("135")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21400 21477 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("26")
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
									pcdata("54.6841771492999")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("130")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.53454772056926e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21400")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21477")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ARHEADTRHTXSQPGPARRVCADYWX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ARHEADTRHT*SQPGPARRVCADYW*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ARHEADTRHT SQPGPARRVCADYW ")
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
							pcdata("38663925")
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
					pcdata("136")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("136")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21478 21669 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("64")
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
									pcdata("102.834124138969")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("255")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.71273664021558e-24")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("12")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("64")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21511")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21669")
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
									pcdata("50")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("53")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VTGLAGRRAQAQLTGDGDAFXTSTTGTVCYRXSPGKTDPAAGKTALPCRVWRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("VTGLAGRRAQAQLTGDGDAF*TSTTGTVCYR*SPGKTDPAAGKTALPCRVWR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("VTGLAGRRAQAQLTGDGDAF TSTTGTVCYR SPGKTDPAAGKTALPCRVWR ")
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
							pcdata("38662975")
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
					pcdata("137")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("137")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21670 21780 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("37")
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
									pcdata("70.092160185994")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("170")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.35714713186098e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21670")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21780")
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
									pcdata("36")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LRAVRAFRQAFRGVNRAKFHQRSSGEAFARKISCRRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LRAVRAFRQAFRGVNRAKFHQRSSGEAFARKISCRR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LRAVRAFRQAFRGVNRAKFHQRSSGEAFARKISCRR ")
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
							pcdata("38663650")
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
					pcdata("138")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("138")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21781 21933 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("51")
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
									pcdata("75.484954248837")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("184")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.33089946158417e-16")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("51")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21781")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("21933")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("51")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XFPLWRWSXRRFLVITESWHGIRLRYHQYANFLXXXXXXXXXXXXSGPCGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*FPLWRWS*RRFLVITESWHGIRLRYHQYANFLRRWRAHQQHRRASGPCG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" FPLWRWS RRFLVITESWHGIRLRYHQYANFLRRWRAHQQHRRASGPCG ")
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
							pcdata("38663300")
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
					pcdata("139")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("139")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 21934 22107 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("58")
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
									pcdata("112.464113536903")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("280")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.74083750157768e-27")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("21934")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22107")
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
									pcdata("55")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QSGSGREFTGAPVCHLRACSPRXXIRAHYRFPDGECTAAPSGFPGERGLCGRSAGPRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QSGSGREFTGAPVCHLRACSPR**IRAHYRFPDGECTAAPSGFPGERGLCGRSAGPR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("QSGSGREFTGAPVCHLRACSPR  IRAHYRFPDGECTAAPSGFPGERGLCGRSAGPR ")
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
							pcdata("38663125")
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
					pcdata("140")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("140")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22108 22302 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("65")
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
									pcdata("126.331298269928")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("316")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.21972525841864e-31")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22108")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22302")
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
									pcdata("62")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("62")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KAVTRRGKHRNTPNGCRYSPAAGSAFVRCCNGPLRSPYTSSAAXKNTQXAAICVAGRTESADCAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KAVTRRGKHRNTPNGCRYSPAAGSAFVRCCNGPLRSPYTSSAA*KNTQ*AAICVAGRTESADCA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KAVTRRGKHRNTPNGCRYSPAAGSAFVRCCNGPLRSPYTSSAA KNTQ AAICVAGRTESADCA ")
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
							pcdata("38662950")
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
					pcdata("141")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("141")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22303 22398 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("32")
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
									pcdata("64.6993661231511")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("156")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.37560459844018e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22303")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22398")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XINRPRIFWANKTGLSLLCNQTEIRNRESDEX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*INRPRIFWANKTGLSLLCNQTEIRNRESDE*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" INRPRIFWANKTGLSLLCNQTEIRNRESDE ")
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
							pcdata("38663775")
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
					pcdata("142")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("142")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22399 22491 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("61.6177695158123")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("148")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.12629546323459e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22399")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22491")
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
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LXINPEFAGNRVPDAWRSRQARTRNAGALDX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("L*INPEFAGNRVPDAWRSRQARTRNAGALD*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("L INPEFAGNRVPDAWRSRQARTRNAGALD ")
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
							pcdata("38663800")
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
					pcdata("143")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("143")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22492 22725 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("78")
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
									pcdata("148.287674097217")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("373")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.07212983841292e-37")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("78")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22492")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22725")
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
									pcdata("71")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("71")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("78")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XXSVRHHPCGXKRQKNLHSAXWPSLCEWQHSYWSLGXQDSERHYREVQRAFRLXLAVCAWLGLPRSADRAESRARIRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("**SVRHHPCG*KRQKNLHSA*WPSLCEWQHSYWSLG*QDSERHYREVQRAFRL*LAVCAWLGLPRSADRAESRARIR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("  SVRHHPCG KRQKNLHSA WPSLCEWQHSYWSLG QDSERHYREVQRAFRL LAVCAWLGLPRSADRAESRARIR ")
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
							pcdata("38662625")
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
					pcdata("144")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("144")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22726 22794 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("23")
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
							pcdata("35570834")
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
					pcdata("145")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("145")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22795 22875 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("27")
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
									pcdata("55.4545763011346")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("132")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.2761893533683e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22795")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22875")
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
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RSTQRLYPSGRAGRLVAPVPDHGLQNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RSTQRLYPSGRAGRLVAPVPDHGLQN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RSTQRLYPSGRAGRLVAPVPDHGLQN ")
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
							pcdata("38663900")
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
					pcdata("146")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("146")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22876 22959 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("28")
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
									pcdata("53.9137779974652")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("128")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.59578934575579e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22876")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("22959")
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
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SQHHPRAGQNHRQRSPAQRREASSLVRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SQHHPRAGQNHRQRSPAQRREASSLVR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SQHHPRAGQNHRQRSPAQRREASSLVR ")
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
							pcdata("38663875")
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
					pcdata("147")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("147")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22960 22992 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("11")
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
							pcdata("17012138")
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
					pcdata("148")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("148")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 22993 23085 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("63.543767395399")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("153")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.89066835193306e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("22993")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("23085")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VLRQNFSVHRRCFPGSRSGCTESKICRKQRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("VLRQNFSVHRRCFPGSRSGCTESKICRKQR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("VLRQNFSVHRRCFPGSRSGCTESKICRKQR ")
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
							pcdata("38663800")
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
					pcdata("149")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("149")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23086 23226 +1")
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
									pcdata("65.4697652749858")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("158")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.83967389601125e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("23086")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("23175")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RPNLAGNLDHHAVDSACQPRNLYCTRFRLC")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RPNLAGNLDHHAVDSACQPRNLYCTRFRLC")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RPNLAGNLDHHAVDSACQPRNLYCTRFRLC")
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
					pcdata("150")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("150")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23227 23454 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("76")
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
									pcdata("143.665279186209")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("361")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.77631878870409e-36")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("23227")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("23454")
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
									pcdata("74")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KRNAAYRRDRLHHSRHGKRCGAXAAALYPSVYGLRRSGNPRRSRYPGCRYRCRSHRAWPRPGRLCDRSEIRPGNRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KRNAAYRRDRLHHSRHGKRCGA*AAALYPSVYGLRRSGNPRRSRYPGCRYRCRSHRAWPRPGRLCDRSEIRPGNR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KRNAAYRRDRLHHSRHGKRCGA AAALYPSVYGLRRSGNPRRSRYPGCRYRCRSHRAWPRPGRLCDRSEIRPGNR ")
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
							pcdata("38662675")
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
					pcdata("151")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("151")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23455 23763 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("103")
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
									pcdata("154.836066887812")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("390")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.21213734598871e-39")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("23455")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("23763")
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
									pcdata("101")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("101")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PGWPGRHLSAGHLSDAGWRERLQSEXXXXXXXXXXXXXXXXXXXXXQLSVLLASQNADHLPRDAAVVRQHGSERSACAVTERDQRRAVDPGLGPGAYRVDGCX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PGWPGRHLSAGHLSDAGWRERLQSERHRRCAAAGKRRAAAR*ENAAQLSVLLASQNADHLPRDAAVVRQHGSERSACAVTERDQRRAVDPGLGPGAYRVDGC*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PGWPGRHLSAGHLSDAGWRERLQSERHRRCAAAGKRRAAAR ENAAQLSVLLASQNADHLPRDAAVVRQHGSERSACAVTERDQRRAVDPGLGPGAYRVDGC ")
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
							pcdata("47941066")
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
					pcdata("152")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("152")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23764 23862 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("33")
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
									pcdata("65.8549648509032")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("159")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.82507130629727e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("23764")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("23862")
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
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PSXLVYLPSAHLGCTDVTVRAQRHGRAASAYPX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PS*LVYLPSAHLGCTDVTVRAQRHGRAASAYP*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PS LVYLPSAHLGCTDVTVRAQRHGRAASAYP ")
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
							pcdata("38663750")
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
					pcdata("153")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("153")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23863 23955 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("59.3065720603082")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("142")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.02566379069519e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("23863")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("23955")
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
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TDGRSGKTRXSRWHPGVVGSRCERDPRRRSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TDGRSGKTR*SRWHPGVVGSRCERDPRRRS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TDGRSGKTR SRWHPGVVGSRCERDPRRRS ")
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
							pcdata("38663800")
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
					pcdata("154")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("154")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23956 23997 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("14")
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
							pcdata("21651812")
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
					pcdata("155")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("155")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 23998 24027 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("10")
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
							pcdata("15465580")
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
					pcdata("156")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("156")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24028 24081 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("18")
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
							pcdata("27838044")
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
					pcdata("157")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("157")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24082 24138 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("19")
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
									pcdata("41.2021919921926")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.39121556469927e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("24082")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("24138")
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
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PTPRLVHVFPNDLHRDEGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PTPRLVHVFPNDLHRDEG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PTPRLVHVFPNDLHRDEG ")
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
							pcdata("29384602")
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
					pcdata("158")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("158")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24139 24300 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("54")
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
									pcdata("109.382516929564")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("272")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.565408676985e-26")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("24139")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("24300")
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
									pcdata("52")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("52")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SAVSSGTDPRLYRGWSGPQDVXIHRQYRFAAGCDEQTGRGYSASVGGINRLHRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SAVSSGTDPRLYRGWSGPQDV*IHRQYRFAAGCDEQTGRGYSASVGGINRLHR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SAVSSGTDPRLYRGWSGPQDV IHRQYRFAAGCDEQTGRGYSASVGGINRLHR ")
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
							pcdata("38663225")
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
					pcdata("159")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("159")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24301 24555 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("85")
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
									pcdata("160.228860950655")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("404")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.26918450685709e-41")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("85")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("24301")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("24555")
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
									pcdata("80")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("80")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("85")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("NGRFXRDPETCCRXLSSYPXHRALPAGKPERFXSSKRYGETGRDGGTGSLGRRLCESGTGRHPQGVRSIRFPRSGTASDALLLRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NGRF*RDPETCCR*LSSYP*HRALPAGKPERF*SSKRYGETGRDGGTGSLGRRLCESGTGRHPQGVRSIRFPRSGTASDALLLR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("NGRF RDPETCCR LSSYP HRALPAGKPERF SSKRYGETGRDGGTGSLGRRLCESGTGRHPQGVRSIRFPRSGTASDALLLR ")
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
							pcdata("38662450")
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
					pcdata("160")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("160")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24556 24705 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("98.9821283797956")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("245")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.09515017354434e-23")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("24556")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("24705")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DGFLLPRHHQRPSVHRQSGQCGASXLPDCAISHRRSAGALDGTNPLLHRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DGFLLPRHHQRPSVHRQSGQCGAS*LPDCAISHRRSAGALDGTNPLLHR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DGFLLPRHHQRPSVHRQSGQCGAS LPDCAISHRRSAGALDGTNPLLHR ")
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
							pcdata("38663325")
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
					pcdata("161")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("161")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24706 24738 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("11")
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
							pcdata("17012138")
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
					pcdata("162")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("162")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24739 24759 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("7")
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
							pcdata("10825906")
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
					pcdata("163")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("163")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24760 24795 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("164")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("164")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24796 24861 +1")
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
									pcdata("44.6689881754487")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("104")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.36290225791372e-06")
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
									pcdata("24796")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("24861")
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
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
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
									pcdata("SDERCVLGRAVESAWRSEQSHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SDERCVLGRAVESAWRSEQSH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SDERCVLGRAVESAWRSEQSH ")
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
					pcdata("165")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("165")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 24862 25011 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("97.8265296520435")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("242")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.45410251699954e-22")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("24862")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("25011")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ASACRQESGWLAGSGSNLVCRTGTVGETDRAGRXITICPVDLRRYRCRLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ASACRQESGWLAGSGSNLVCRTGTVGETDRAGR*ITICPVDLRRYRCRL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ASACRQESGWLAGSGSNLVCRTGTVGETDRAGR ITICPVDLRRYRCRL ")
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
							pcdata("38663325")
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
					pcdata("166")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("166")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 25012 25182 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("57")
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
									pcdata("65.0845656990685")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("157")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.19367099365766e-12")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("53")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("25030")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("25170")
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
									pcdata("45")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("45")
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
									pcdata("SAERSTQRAXXXXXXXXXXXXPTLLALHPGCRQGGGTRRNLRPLCQQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SAERSTQRAESRVE*SRR*EVPTLLALHPGCRQGGGTRRNLRPLCQQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SAERSTQRAESRVE SRR EVPTLLALHPGCRQGGGTRRNLRPLCQQ")
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
							pcdata("38663150")
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
					pcdata("167")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("167")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 25183 25701 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("173")
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
									pcdata("317.775487500852")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("813")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.24352880783591e-88")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("173")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("25189")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("25701")
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
									pcdata("169")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("169")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("171")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KTXVCLMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KT*VCLMSQSICSTGLRWLWLVVVVLIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KT VCLMSQSICSTGLRWLWLVVVVLIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ ")
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
							pcdata("143822454")
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
					pcdata("168")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("168")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 25702 25833 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("44")
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
									pcdata("67.3957631545726")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("163")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.06977400749656e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("25702")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("25833")
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
									pcdata("42")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XTLPDAMLTHLIRPTDCCEIVGXXXXXXXXXQKSLNIRANLHVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*TLPDAMLTHLIRPTDCCEIVGRIRRLRRIRQKSLNIRANLHV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" TLPDAMLTHLIRPTDCCEIVGRIRRLRRIRQKSLNIRANLHV ")
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
							pcdata("38663475")
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
					pcdata("169")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("169")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 25834 25851 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("6")
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
							pcdata("9279348")
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
					pcdata("170")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("170")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 25852 26148 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("99")
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
									pcdata("191.044827024043")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("484")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.48162526526382e-50")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("25852")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("26148")
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
									pcdata("93")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RRPGALHAKTRRWHHRRVYPQQRXTGAVPPGXCFSFXRAGATPVGAESGRXNHLLVGARCGVWRAVTGPDSVLLPPXIYGCRRARNWRNHAFYRNGWQX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRPGALHAKTRRWHHRRVYPQQR*TGAVPPG*CFSF*RAGATPVGAESGR*NHLLVGARCGVWRAVTGPDSVLLPP*IYGCRRARNWRNHAFYRNGWQ*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RRPGALHAKTRRWHHRRVYPQQR TGAVPPG CFSF RAGATPVGAESGR NHLLVGARCGVWRAVTGPDSVLLPP IYGCRRARNWRNHAFYRNGWQ ")
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
							pcdata("41755122")
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
					pcdata("171")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("171")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26149 26322 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("58")
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
									pcdata("114.004911840572")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("284")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.11362170850221e-27")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("26149")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("26322")
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
									pcdata("53")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("53")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DAWRDPRNXRRLHYRXFQPSAGRADRSFXYXSAGNRSGTGGVTCRSCWPTRVVFVPGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DAWRDPRN*RRLHYR*FQPSAGRADRSF*Y*SAGNRSGTGGVTCRSCWPTRVVFVPG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DAWRDPRN RRLHYR FQPSAGRADRSF Y SAGNRSGTGGVTCRSCWPTRVVFVPG ")
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
							pcdata("38663125")
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
					pcdata("172")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("172")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26323 26484 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("54")
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
									pcdata("105.530521170391")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("262")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.17166617323183e-25")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("26323")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("26484")
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
									pcdata("53")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("53")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TALSALLKTRWPFTAHRYMSVTKWYITAMWSIACVSVGLSLLSRLAKYRTARSX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("TALSALLKTRWPFTAHRYMSVTKWYITAMWSIACVSVGLSLLSRLAKYRTARS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("TALSALLKTRWPFTAHRYMSVTKWYITAMWSIACVSVGLSLLSRLAKYRTARS ")
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
							pcdata("38663225")
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
					pcdata("173")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("173")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26485 26544 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("20")
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
									pcdata("41.2021919921926")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.24974548220948e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("26485")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("26544")
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
									pcdata("20")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FSPHTVFLRRYVTKQKVAIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FSPHTVFLRRYVTKQKVAI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FSPHTVFLRRYVTKQKVAI ")
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
							pcdata("30931160")
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
					pcdata("174")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("174")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26545 26574 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("10")
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
							pcdata("15465580")
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
					pcdata("175")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("175")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26575 26736 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("54")
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
									pcdata("108.997317353647")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("271")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.3602673907913e-26")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("26575")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("26736")
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
									pcdata("53")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("53")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PKCIWKSPAPVAVAKNLFSSVTPGTRKWKGQWASTVTRKGECIWSNRRTMCGNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PKCIWKSPAPVAVAKNLFSSVTPGTRKWKGQWASTVTRKGECIWSNRRTMCGN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PKCIWKSPAPVAVAKNLFSSVTPGTRKWKGQWASTVTRKGECIWSNRRTMCGN ")
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
							pcdata("38663225")
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
					pcdata("176")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("176")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26737 26808 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
							pcdata("37117392")
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
					pcdata("177")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("177")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 26809 27009 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("67")
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
									pcdata("127.872096573597")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("320")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.55040893948556e-31")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("26809")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("27009")
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
									pcdata("66")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("STRCVNASRKLSVRAKMTSATPRLTVRKRYAPWQNRRKLCWWSVRKTPPTPTVWRSWPSVWANARFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("STRCVNASRKLSVRAKMTSATPRLTVRKRYAPWQNRRKLCWWSVRKTPPTPTVWRSWPSVWANARF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("STRCVNASRKLSVRAKMTSATPRLTVRKRYAPWQNRRKLCWWSVRKTPPTPTVWRSWPSVWANARF ")
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
							pcdata("38662900")
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
					pcdata("178")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("178")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27010 27045 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("179")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("179")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27046 27243 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("66")
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
									pcdata("124.020100814424")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("310")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.80217952867679e-30")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("27046")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("27243")
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
									pcdata("64")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("64")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KRLNASAXLRAHRLRIFWCRMWWHVCSSWAVVKPFRWKAVKKTLFSKCRKSCVSIFVKSIKSLAAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KRLNASA*LRAHRLRIFWCRMWWHVCSSWAVVKPFRWKAVKKTLFSKCRKSCVSIFVKSIKSLAA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KRLNASA LRAHRLRIFWCRMWWHVCSSWAVVKPFRWKAVKKTLFSKCRKSCVSIFVKSIKSLAA ")
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
							pcdata("38662925")
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
					pcdata("180")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("180")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27244 27321 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("26")
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
									pcdata("55.0693767252173")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("131")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.13639468398407e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("27244")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("27321")
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
									pcdata("25")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VMRKCRSCYRHFLWRKHAFTYLPRYX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("VMRKCRSCYRHFLWRKHAFTYLPRY*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("VMRKCRSCYRHFLWRKHAFTYLPRY ")
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
							pcdata("38663925")
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
					pcdata("181")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("181")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27322 27519 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("66")
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
									pcdata("91.2781368614485")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("225")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.36098452289192e-20")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("27379")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("27519")
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
									pcdata("44")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("44")
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
									pcdata("PATDDHRRGXCLGXENYPQCPATAAFLECGDSARPRGRCATGTRTAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PATDDHRRG*CLG*ENYPQCPATAAFLECGDSARPRGRCATGTRTA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PATDDHRRG CLG ENYPQCPATAAFLECGDSARPRGRCATGTRTA ")
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
							pcdata("38662925")
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
					pcdata("182")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("182")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27520 27570 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("17")
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
							pcdata("26291486")
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
					pcdata("183")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("183")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27571 27786 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("72")
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
									pcdata("149.443272824969")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("376")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.10704953468157e-38")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("72")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("27571")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("27786")
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
									pcdata("71")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("71")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("72")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AQPKAARDTGVSGDSGCPDACTRACYPGGHRPVNQYCAVTFTMPGMQAVYSPSGDHGWFCRTRQLYAKRRVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AQPKAARDTGVSGDSGCPDACTRACYPGGHRPVNQYCAVTFTMPGMQAVYSPSGDHGWFCRTRQLYAKRRV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AQPKAARDTGVSGDSGCPDACTRACYPGGHRPVNQYCAVTFTMPGMQAVYSPSGDHGWFCRTRQLYAKRRV ")
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
							pcdata("38662775")
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
					pcdata("184")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("184")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27787 27837 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("17")
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
							pcdata("26291486")
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
					pcdata("185")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("185")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27838 27888 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("17")
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
							pcdata("26291486")
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
					pcdata("186")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("186")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 27889 28101 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("71")
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
									pcdata("138.657684699283")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("348")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.42444638899021e-35")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("71")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("27889")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28101")
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
									pcdata("68")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("71")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LSLYTAAVKPYRENASRPVXPLPXRQYAKRLANARSLRHRLAGAPGPVHSQTLFCGSGNSGRIYLRHDGGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LSLYTAAVKPYRENASRPV*PLP*RQYAKRLANARSLRHRLAGAPGPVHSQTLFCGSGNSGRIYLRHDGG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LSLYTAAVKPYRENASRPV PLP RQYAKRLANARSLRHRLAGAPGPVHSQTLFCGSGNSGRIYLRHDGG ")
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
							pcdata("38662800")
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
					pcdata("187")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("187")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28102 28185 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("28")
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
									pcdata("55.839775877052")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("133")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.7179848956057e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("28102")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28185")
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
									pcdata("27")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("YRRLPGQASQCTGGIGSGCERLPAVGGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("YRRLPGQASQCTGGIGSGCERLPAVGG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("YRRLPGQASQCTGGIGSGCERLPAVGG ")
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
							pcdata("38663875")
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
					pcdata("188")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("188")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28186 28260 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("25")
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
									pcdata("48.9061835105396")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("115")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.15306454941058e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("28186")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28260")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GAGSGVVTCHMLLACSHSSTHAFHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GAGSGVVTCHMLLACSHSSTHAFH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("GAGSGVVTCHMLLACSHSSTHAFH ")
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
							pcdata("38663950")
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
					pcdata("189")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("189")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28261 28290 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("10")
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
							pcdata("15465580")
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
					pcdata("190")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("190")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28291 28338 +1")
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
					pcdata("191")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("191")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28339 28371 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("11")
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
							pcdata("17012138")
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
					pcdata("192")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("192")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28372 28437 +1")
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
									pcdata("45.8245869032008")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("107")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.53491678733265e-07")
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
									pcdata("28372")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28437")
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
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
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
									pcdata("LCMMQTSALPSREPGGVWAASX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LCMMQTSALPSREPGGVWAAS*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LCMMQTSALPSREPGGVWAAS ")
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
					pcdata("193")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("193")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28438 28581 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("48")
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
									pcdata("95.130132620622")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("235")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.74337636381375e-22")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("28438")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28581")
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
									pcdata("48")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FRRRWHXRACSWALRWSVKDLLYWAATPVSWPEPGKQALPCKAASMRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FRRRWH*RACSWALRWSVKDLLYWAATPVSWPEPGKQALPCKAASMR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FRRRWH RACSWALRWSVKDLLYWAATPVSWPEPGKQALPCKAASMR ")
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
							pcdata("38663375")
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
					pcdata("194")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("194")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28582 28632 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("17")
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
							pcdata("26291486")
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
					pcdata("195")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("195")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28633 28674 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("14")
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
							pcdata("21651812")
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
					pcdata("196")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("196")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28675 28812 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("46")
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
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.12654528709374e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("28675")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28812")
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
									pcdata("45")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("45")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SALRGLTKPVNKQFVXXXXXXXXXXXXXXXXXXXXXXSCWRKQPKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SALRGLTKPVNKQFVTPLPILRLSLLPILALALTSCLSCWRKQPK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SALRGLTKPVNKQFVTPLPILRLSLLPILALALTSCLSCWRKQPK ")
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
							pcdata("38663425")
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
					pcdata("197")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("197")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28813 28932 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("40")
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
									pcdata("81.2629478875973")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("199")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.45622123389464e-17")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("28813")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("28932")
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
									pcdata("39")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("39")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("WVTTPISKLLKHIIDIKLMRRQAPHWQWERRSPTPLIKIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("WVTTPISKLLKHIIDIKLMRRQAPHWQWERRSPTPLIKI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("WVTTPISKLLKHIIDIKLMRRQAPHWQWERRSPTPLIKI ")
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
							pcdata("38663575")
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
					pcdata("198")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("198")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 28933 29097 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("55")
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
									pcdata("101.2933258353")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("251")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.51560468819998e-23")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("28933")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29097")
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
									pcdata("54")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("55")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KIARSTVVKATPVNVCLAPLVLPPCVQVTSLVNIPRCLPILASVWRSPIRRPAVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KIARSTVVKATPVNVCLAPLVLPPCVQVTSLVNIPRCLPILASVWRSPIRRPAV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KIARSTVVKATPVNVCLAPLVLPPCVQVTSLVNIPRCLPILASVWRSPIRRPAV ")
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
							pcdata("38663200")
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
					pcdata("199")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("199")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29098 29118 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("7")
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
							pcdata("10825906")
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
					pcdata("200")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("200")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29119 29226 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("36")
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
									pcdata("70.092160185994")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("170")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.03729462136792e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("29119")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29226")
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
									pcdata("34")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("DRLCGXVVRKAVFLICEMYLISIICNHKIFVMVQKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("DRLCG*VVRKAVFLICEMYLISIICNHKIFVMVQK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("DRLCG VVRKAVFLICEMYLISIICNHKIFVMVQK ")
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
							pcdata("38663675")
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
					pcdata("201")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("201")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29227 29235 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("3")
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
							pcdata("4639674")
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
					pcdata("202")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("202")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29236 29259 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("8")
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
							pcdata("12372464")
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
					pcdata("203")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("203")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29260 29295 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("0")
							] ),
						element( 'Statistics_kappa',
							[],
							[
							pcdata("-1")
							] ),
						element( 'Statistics_lambda',
							[],
							[
							pcdata("-1")
							] ),
						element( 'Statistics_entropy',
							[],
							[
							pcdata("-1")
							] )
						] )
					] ),
				element( 'Iteration_message',
					[],
					[
					pcdata("lcl|203 U00096 29260 29295 +1:  lcl|203 U00096 29260 29295 +1: Warning: Could not calculate ungapped Karlin-Altschul parameters due to an invalid query sequence or its translation. Please verify the query sequence(s) and/or filtering options  No hits found")
					] )
				] ),
			element( 'Iteration',
				[],
				[
				element( 'Iteration_iter-num',
					[],
					[
					pcdata("204")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("204")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29296 29355 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("20")
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
									pcdata("38.8909945366884")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("89")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.94898826943923e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("29296")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29355")
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
									pcdata("20")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IVYKCQKLHVLSSVFVVLMX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IVYKCQKLHVLSSVFVVLM*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IVYKCQKLHVLSSVFVVLM ")
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
							pcdata("30931160")
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
					pcdata("205")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("205")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29356 29445 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("30")
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
									pcdata("60.0769712121429")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("144")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.16945466346349e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("29356")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29445")
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
									pcdata("29")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ILTIWSTFFCSFLFHAIFLLRKRFPEQVRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ILTIWSTFFCSFLFHAIFLLRKRFPEQVR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ILTIWSTFFCSFLFHAIFLLRKRFPEQVR ")
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
							pcdata("38663825")
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
					pcdata("206")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("206")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29446 29481 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("207")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("207")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29482 29622 +1")
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
									pcdata("92.4337355892006")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("228")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.05849839837851e-21")
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
									pcdata("29482")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29622")
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
									pcdata("43")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("43")
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
									pcdata("ATKXYKKSRHXVDFXRPYLQNAAVCQKFVGKQICIDLRHHCELICKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ATK*YKKSRH*VDF*RPYLQNAAVCQKFVGKQICIDLRHHCELICK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ATK YKKSRH VDF RPYLQNAAVCQKFVGKQICIDLRHHCELICK ")
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
					pcdata("208")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("208")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29623 29658 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("12")
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
							pcdata("18558696")
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
					pcdata("209")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("209")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29659 29784 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("42")
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
									pcdata("83.1889457671841")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("204")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.09668735269202e-18")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("29659")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29784")
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
									pcdata("41")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VSAIGSGRRNPVSRSGHRGNRFGGWGSRFQYFNDRLSRNPHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("VSAIGSGRRNPVSRSGHRGNRFGGWGSRFQYFNDRLSRNPH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("VSAIGSGRRNPVSRSGHRGNRFGGWGSRFQYFNDRLSRNPH ")
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
							pcdata("38663525")
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
					pcdata("210")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("210")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29785 29931 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("49")
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
									pcdata("97.4413300761261")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("241")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.15231881618296e-22")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("29785")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("29931")
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
									pcdata("49")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SFLFSSNRYSYLSPYWQCRHQXRRXRIFSGTCTRSGDSRPAADCQQLPX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SFLFSSNRYSYLSPYWQCRHQ*RR*RIFSGTCTRSGDSRPAADCQQLP*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SFLFSSNRYSYLSPYWQCRHQ RR RIFSGTCTRSGDSRPAADCQQLP ")
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
							pcdata("38663350")
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
					pcdata("211")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("211")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 29932 30060 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("43")
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
									pcdata("83.9593449190188")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("206")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.13691999527976e-18")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("43")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("29932")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("30060")
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
									pcdata("40")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("40")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("43")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("YRRPLFLPETPXHRGDCRYRYPXADAFTARERRTEWLHYRGRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("YRRPLFLPETP*HRGDCRYRYP*ADAFTARERRTEWLHYRGR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("YRRPLFLPETP HRGDCRYRYP ADAFTARERRTEWLHYRGR ")
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
							pcdata("38663500")
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
					pcdata("212")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("212")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 30061 30243 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("61")
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
									pcdata("115.160510568324")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("287")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("8.23293298729278e-28")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("61")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("30061")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("30243")
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
									pcdata("59")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("59")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("61")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PGCGAGVRKSPRVPRSEWHGSGKRSDHRRSLXLDTRELDVDRWPARSEKRRRAAVPRRGLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PGCGAGVRKSPRVPRSEWHGSGKRSDHRRSL*LDTRELDVDRWPARSEKRRRAAVPRRGL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PGCGAGVRKSPRVPRSEWHGSGKRSDHRRSL LDTRELDVDRWPARSEKRRRAAVPRRGL ")
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
							pcdata("38663050")
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
					pcdata("213")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("213")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 30244 30543 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("100")
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
									pcdata("199.134018118307")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("505")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.02010103806789e-53")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("100")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("30244")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("30543")
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
									pcdata("98")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("98")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("100")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FWCQAQHPADAGGXRLSPDHRSGANFCGRCAENESRRHLPLQRSWRPGPVRLRHYRHPEIPRNRYSGIRHLSRSSAAGAGERCEDCQNEIWSPRRQPSGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FWCQAQHPADAGG*RLSPDHRSGANFCGRCAENESRRHLPLQRSWRPGPVRLRHYRHPEIPRNRYSGIRHLSRSSAAGAGERCEDCQNEIWSPRRQPSG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FWCQAQHPADAGG RLSPDHRSGANFCGRCAENESRRHLPLQRSWRPGPVRLRHYRHPEIPRNRYSGIRHLSRSSAAGAGERCEDCQNEIWSPRRQPSG ")
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
							pcdata("43301608")
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
					pcdata("214")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("214")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 30544 30639 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("32")
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
									pcdata("4.17169398787815e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("30544")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("30639")
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
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RCGEKRGNDHRPEPRFCGGRSNITCKPACHAX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RCGEKRGNDHRPEPRFCGGRSNITCKPACHA*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RCGEKRGNDHRPEPRFCGGRSNITCKPACHA ")
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
							pcdata("38663775")
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
					pcdata("215")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("215")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 30640 30684 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("15")
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
							pcdata("23198370")
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
					pcdata("216")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("216")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 30685 30714 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("10")
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
							pcdata("15465580")
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
					pcdata("217")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("217")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 30715 34038 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("1108")
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
									pcdata("2270.73733740183")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("5883")
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
									pcdata("1108")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("30715")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("34038")
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
									pcdata("1104")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("1104")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("1108")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SQPWSTRRRAVVRPLYRVNXAVPXNRXVIRSKRAMPKRTDIKSILILGAGPIVIGQACEFDYSGAQACKALREEGYRVILVNSNPATIMTDPEMADATYIEPIHWEVVRKIIEKERPDAVLPTMGGQTALNCALELERQGVLEEFGVTMIGATADAIDKAEDRRRFDVAMKKIGLETARSGIAHTMEEALAVAADVGFPCIIRPSFTMGGSGGGIAYNREEFEEICARGLDLSPTKELLIDESLIGWKEYEMEVVRDKNDNCIIVCSIENFDAMGIHTGDSITVAPAQTLTDKEYQIMRNASMAVLREIGVETGGSNVQFAVNPKNGRLIVIEMNPRVSRSSALASKATGFPIAKVAAKLAVGYTLDELMNDITGGRTPASFEPSIDYVVTKIPRFNFEKFAGANDRLTTQMKSVGEVMAIGRTQQESLQKALRGLEVGATGFDPKVSLDDPEALTKIRRELKDAGADRIWYIADAFRAGLSVDGVFNLTNIDRWFLVQIEELVRLEEKVAEVGITGLNADFLRQLKRKGFADARLAKLAGVREAEIRKLRDQYDLHPVYKRVDTCAAEFATDTAYMYSTYEEECEANPSTDREKIMVLGGGPNRIGQGIEFDYCCVHASLALREDGYETIMVNCNPETVSTDYDTSDRLYFEPVTLEDVLEIVRIEKPKGVIVQYGGQTPLKLARALEAAGVPVIGTSPDAIDRAEDRERFQHAVERLKLKQPANATVTAIEMAVEKAKEIGYPLVVRPSYVLGGRAMEIVYDEADLRRYFQTAVSVSNDAPVLLDHFLDDAVEVDVDAICDGEMVLIGGIMEHIEQAGVHSGDSACSLPAYTLSQEIQDVMRQQVQKLAFELQVRGLMNVQFAVKNNEVYLIEVNPRAARTVPFVSKATGVPLAKVAARVMAGKSLAEQGVTKEVIPPYYSVKEVVLPFNKFPGVDPLLGPEMRSTGEVMGVGRTFAEAFAKAQLGSNSTMKKHGRALLSVREGDKERVVDLAAKLLKQGFELDATHGTAIVLGEAGINPRLVNKVHEGRPHIQDRIKNGEYTYIINTTSGRRAIEDSRVIRRSALQYKVHYDTTLNGGFATAMALNADATEKVISVQEMHAQIKX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SQPWSTRRRAVVRPLYRVN*AVP*NR*VIRSKRAMPKRTDIKSILILGAGPIVIGQACEFDYSGAQACKALREEGYRVILVNSNPATIMTDPEMADATYIEPIHWEVVRKIIEKERPDAVLPTMGGQTALNCALELERQGVLEEFGVTMIGATADAIDKAEDRRRFDVAMKKIGLETARSGIAHTMEEALAVAADVGFPCIIRPSFTMGGSGGGIAYNREEFEEICARGLDLSPTKELLIDESLIGWKEYEMEVVRDKNDNCIIVCSIENFDAMGIHTGDSITVAPAQTLTDKEYQIMRNASMAVLREIGVETGGSNVQFAVNPKNGRLIVIEMNPRVSRSSALASKATGFPIAKVAAKLAVGYTLDELMNDITGGRTPASFEPSIDYVVTKIPRFNFEKFAGANDRLTTQMKSVGEVMAIGRTQQESLQKALRGLEVGATGFDPKVSLDDPEALTKIRRELKDAGADRIWYIADAFRAGLSVDGVFNLTNIDRWFLVQIEELVRLEEKVAEVGITGLNADFLRQLKRKGFADARLAKLAGVREAEIRKLRDQYDLHPVYKRVDTCAAEFATDTAYMYSTYEEECEANPSTDREKIMVLGGGPNRIGQGIEFDYCCVHASLALREDGYETIMVNCNPETVSTDYDTSDRLYFEPVTLEDVLEIVRIEKPKGVIVQYGGQTPLKLARALEAAGVPVIGTSPDAIDRAEDRERFQHAVERLKLKQPANATVTAIEMAVEKAKEIGYPLVVRPSYVLGGRAMEIVYDEADLRRYFQTAVSVSNDAPVLLDHFLDDAVEVDVDAICDGEMVLIGGIMEHIEQAGVHSGDSACSLPAYTLSQEIQDVMRQQVQKLAFELQVRGLMNVQFAVKNNEVYLIEVNPRAARTVPFVSKATGVPLAKVAARVMAGKSLAEQGVTKEVIPPYYSVKEVVLPFNKFPGVDPLLGPEMRSTGEVMGVGRTFAEAFAKAQLGSNSTMKKHGRALLSVREGDKERVVDLAAKLLKQGFELDATHGTAIVLGEAGINPRLVNKVHEGRPHIQDRIKNGEYTYIINTTSGRRAIEDSRVIRRSALQYKVHYDTTLNGGFATAMALNADATEKVISVQEMHAQIK*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SQPWSTRRRAVVRPLYRVN AVP NR VIRSKRAMPKRTDIKSILILGAGPIVIGQACEFDYSGAQACKALREEGYRVILVNSNPATIMTDPEMADATYIEPIHWEVVRKIIEKERPDAVLPTMGGQTALNCALELERQGVLEEFGVTMIGATADAIDKAEDRRRFDVAMKKIGLETARSGIAHTMEEALAVAADVGFPCIIRPSFTMGGSGGGIAYNREEFEEICARGLDLSPTKELLIDESLIGWKEYEMEVVRDKNDNCIIVCSIENFDAMGIHTGDSITVAPAQTLTDKEYQIMRNASMAVLREIGVETGGSNVQFAVNPKNGRLIVIEMNPRVSRSSALASKATGFPIAKVAAKLAVGYTLDELMNDITGGRTPASFEPSIDYVVTKIPRFNFEKFAGANDRLTTQMKSVGEVMAIGRTQQESLQKALRGLEVGATGFDPKVSLDDPEALTKIRRELKDAGADRIWYIADAFRAGLSVDGVFNLTNIDRWFLVQIEELVRLEEKVAEVGITGLNADFLRQLKRKGFADARLAKLAGVREAEIRKLRDQYDLHPVYKRVDTCAAEFATDTAYMYSTYEEECEANPSTDREKIMVLGGGPNRIGQGIEFDYCCVHASLALREDGYETIMVNCNPETVSTDYDTSDRLYFEPVTLEDVLEIVRIEKPKGVIVQYGGQTPLKLARALEAAGVPVIGTSPDAIDRAEDRERFQHAVERLKLKQPANATVTAIEMAVEKAKEIGYPLVVRPSYVLGGRAMEIVYDEADLRRYFQTAVSVSNDAPVLLDHFLDDAVEVDVDAICDGEMVLIGGIMEHIEQAGVHSGDSACSLPAYTLSQEIQDVMRQQVQKLAFELQVRGLMNVQFAVKNNEVYLIEVNPRAARTVPFVSKATGVPLAKVAARVMAGKSLAEQGVTKEVIPPYYSVKEVVLPFNKFPGVDPLLGPEMRSTGEVMGVGRTFAEAFAKAQLGSNSTMKKHGRALLSVREGDKERVVDLAAKLLKQGFELDATHGTAIVLGEAGINPRLVNKVHEGRPHIQDRIKNGEYTYIINTTSGRRAIEDSRVIRRSALQYKVHYDTTLNGGFATAMALNADATEKVISVQEMHAQIK ")
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
									pcdata("55.4545763011346")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("132")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.69912172150777e-08")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("739")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("909")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3404383")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3404904")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("86")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("187")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AKEIGYPLVVRPSYVLGGRAMEIVYDEADLRRYF-----QTAVSVSNDAPVLLDHFLDDAVEVDVDAICDGEMVLIGGIMEHIEQAGVHSGDSACSL-----------PAYTLSQEIQDVMRQQVQKLAFELQVRGLMNVQFAVKNNEVYLIEVNPRAARTVPFVSKATGVPLAKVAARVMAGKSLA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AKRIGYPVIIKASGGGGGRGMRVVRGDAELAQSISMTRAEAKAAFSNDM-VYMEKYLENPRHVEIQVLADG------------QGNAIYLAERDCSMQRRHQKVVEEAPAPGITPELRRYIGERCAKACVDIGYRGAGTFEFLFENGEFYFIEMNTRIQVEHPVTEMITGVDLIKEQLRIAAGQPLS")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AK IGYP++++ S   GGR M +V  +A+L +       +   + SND  V ++ +L++   V++  + DG            +   ++  +  CS+           PA  ++ E++  + ++  K   ++  RG    +F  +N E Y IE+N R     P     TGV L K   R+ AG+ L+")
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
							pcdata("1563472071")
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
					pcdata("218")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("218")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 34039 34077 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("13")
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
							pcdata("20105254")
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
					pcdata("219")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("219")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 34078 34167 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("30")
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
									pcdata("61.6177695158123")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("148")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.19403794313689e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("34078")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("34167")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SNNXYGSLMRTVFFCPLSKSFGESIFLMTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SNN*YGSLMRTVFFCPLSKSFGESIFLMT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SNN YGSLMRTVFFCPLSKSFGESIFLMT ")
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
							pcdata("38663825")
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
					pcdata("220")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("220")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 34168 34695 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("176")
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
									pcdata("358.606642548092")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("919")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.0181444357656e-100")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("176")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("34168")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("34695")
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
									pcdata("175")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("175")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("176")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AGFSSHLSTVKLHTIDISTILIWPCLIASKRVIARLLICETGVRMCEGYVEKPLYLLIAEWMMAENRWVIAREISIHFDIEHSKAVNTLTYILSEVTEISCEVKMIPNKLEGRGCQCQRLVKVVDIDEQIYARLRNNSREKLVGVRKTPRIPAVPLTELNREQKWQMMLSKSMRRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AGFSSHLSTVKLHTIDISTILIWPCLIASKRVIARLLICETGVRMCEGYVEKPLYLLIAEWMMAENRWVIAREISIHFDIEHSKAVNTLTYILSEVTEISCEVKMIPNKLEGRGCQCQRLVKVVDIDEQIYARLRNNSREKLVGVRKTPRIPAVPLTELNREQKWQMMLSKSMRR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AGFSSHLSTVKLHTIDISTILIWPCLIASKRVIARLLICETGVRMCEGYVEKPLYLLIAEWMMAENRWVIAREISIHFDIEHSKAVNTLTYILSEVTEISCEVKMIPNKLEGRGCQCQRLVKVVDIDEQIYARLRNNSREKLVGVRKTPRIPAVPLTELNREQKWQMMLSKSMRR ")
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
							pcdata("146915315")
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
					pcdata("221")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("221")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 34696 34791 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("32")
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
									pcdata("68.1661623064073")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.4100634246869e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("34696")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("34791")
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
									pcdata("31")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("32")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FYLVDTGRPACQMRCCSILSSNQVASGKITVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FYLVDTGRPACQMRCCSILSSNQVASGKITV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FYLVDTGRPACQMRCCSILSSNQVASGKITV ")
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
							pcdata("38663775")
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
					pcdata("222")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("222")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 34792 34941 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("100.137727107548")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("248")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.34837105887717e-23")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("34792")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("34941")
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
									pcdata("49")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ASHPSSPANGGDFPPFASVAAFHVTIHDSARQDPDTLWYSTVSSVTRRHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ASHPSSPANGGDFPPFASVAAFHVTIHDSARQDPDTLWYSTVSSVTRRH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("ASHPSSPANGGDFPPFASVAAFHVTIHDSARQDPDTLWYSTVSSVTRRH ")
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
							pcdata("38663325")
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
					pcdata("223")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("223")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 34942 35010 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("23")
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
									pcdata("48.5209839346223")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("114")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.05485497678155e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("34942")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("35010")
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
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("HCVQRGLAYPSTTGVSRRESPLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("HCVQRGLAYPSTTGVSRRESPL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("HCVQRGLAYPSTTGVSRRESPL ")
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
							pcdata("35570834")
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
					pcdata("224")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("224")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 35011 35157 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("49")
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
									pcdata("65.4697652749858")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("158")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.67067750403114e-13")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("35011")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("35157")
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
									pcdata("49")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QSSWRQQCSLRQXXXXXXXXXXXXXMHRDQSNNHAGSLRARYGRFPQRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QSSWRQQCSLRQ*PRHP*SRCSSRPMHRDQSNNHAGSLRARYGRFPQR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("QSSWRQQCSLRQ PRHP SRCSSRPMHRDQSNNHAGSLRARYGRFPQR ")
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
							pcdata("38663350")
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
					pcdata("225")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("225")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 35158 35904 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("249")
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
									pcdata("505.367680972603")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1300")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.31865952381318e-144")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("249")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("35158")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("35904")
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
									pcdata("247")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("247")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("249")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CQCRSSHAXCSHPEYWLPLARSTARSHHAVRRVGRCRHRLPQSHRQSRRHWDGQTPSGEPPELTPQRRNSSRLLTSFPHRIALLGKRQRPFNGIFRMQNRWIVFQHAAANIAIRFFYRHFAGGAVDLFQRRNRQRGAVNQLLSQFAGVIHQFRLANHAVDYPPTQRLFCAHSSAGHHHFIDDGRRQDLRQTQHTAAVRNDAQFGFRQGEAGVIGTNNKICRQRQFKAAAKGIAVHSCDNRFVEIENFGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CQCRSSHA*CSHPEYWLPLARSTARSHHAVRRVGRCRHRLPQSHRQSRRHWDGQTPSGEPPELTPQRRNSSRLLTSFPHRIALLGKRQRPFNGIFRMQNRWIVFQHAAANIAIRFFYRHFAGGAVDLFQRRNRQRGAVNQLLSQFAGVIHQFRLANHAVDYPPTQRLFCAHSSAGHHHFIDDGRRQDLRQTQHTAAVRNDAQFGFRQGEAGVIGTNNKICRQRQFKAAAKGIAVHSCDNRFVEIENFG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CQCRSSHA CSHPEYWLPLARSTARSHHAVRRVGRCRHRLPQSHRQSRRHWDGQTPSGEPPELTPQRRNSSRLLTSFPHRIALLGKRQRPFNGIFRMQNRWIVFQHAAANIAIRFFYRHFAGGAVDLFQRRNRQRGAVNQLLSQFAGVIHQFRLANHAVDYPPTQRLFCAHSSAGHHHFIDDGRRQDLRQTQHTAAVRNDAQFGFRQGEAGVIGTNNKICRQRQFKAAAKGIAVHSCDNRFVEIENFG ")
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
									pcdata("42.742990295862")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("99")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.63325903421858e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("137")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("244")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4028374")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4028697")
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
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("49")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("108")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AVNQLLSQFAGVIHQFRLANHAVDYPPTQRLFCAHSSAGHHHFIDDGRRQDLRQTQHTAAVRNDAQFGFRQGEAGVIGTNNKICRQRQFKAAAKGIAVHSCDNRFVEI")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AVNGLA*NRLCAFNQFFCFDYAIHQTDFQRLIRADIFTGGNDFQRTVSAQHTWHTYRTAKARHDAQFGFRQTDAQIRRRQTIIGRQHAFAATAQRIAIDGSNGRHRQI")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AVN L        +QF   ++A+     QRL  A    G + F      Q    T  TA  R+DAQFGFRQ +A +      I RQ  F A A+ IA+   + R  +I")
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
							pcdata("255168210")
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
					pcdata("226")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("226")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 35905 35967 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("21")
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
									pcdata("40.4317928403579")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("93")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.1468707794421e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("35905")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("35967")
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
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SRKTTRTKVSIRCFAFCCRFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SRKTTRTKVSIRCFAFCCRF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SRKTTRTKVSIRCFAFCCRF ")
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
							pcdata("32477718")
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
					pcdata("227")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("227")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 35968 36024 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("19")
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
									pcdata("38.5057949607711")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("88")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.38113832990253e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("35968")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("36024")
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
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("IPARGKELLSGTGNNGDTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IPARGKELLSGTGNNGDT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("IPARGKELLSGTGNNGDT ")
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
							pcdata("29384602")
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
					pcdata("228")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("228")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 36025 36048 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("8")
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
							pcdata("12372464")
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
					pcdata("229")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("229")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 36049 36129 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("27")
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
									pcdata("56.6101750288867")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("135")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.46355324006003e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("36049")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("36129")
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
									pcdata("26")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KYFAHFKAGFCINSIRFWTIKGNFQNX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KYFAHFKAGFCINSIRFWTIKGNFQN*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KYFAHFKAGFCINSIRFWTIKGNFQN ")
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
							pcdata("38663900")
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
					pcdata("230")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("230")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 36130 36150 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("7")
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
							pcdata("10825906")
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
					pcdata("231")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("231")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 36151 36228 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("26")
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
									pcdata("54.6841771492999")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("130")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.44748771450824e-09")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("36151")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("36228")
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
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("XFTHSFSPFLLFQGRQHPCKKCILFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("*FTHSFSPFLLFQGRQHPCKKCILF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" FTHSFSPFLLFQGRQHPCKKCILF ")
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
							pcdata("38663925")
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
					pcdata("232")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("232")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 36229 36981 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("251")
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
									pcdata("506.138080124438")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1302")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.26862945068583e-144")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("251")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("36229")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("36981")
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
									pcdata("250")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("250")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("251")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SVIISWQGSSLLFHLFQILSNYFPRAITWQIFSDLQIRGHFKFRHILFAEAEKFLFAQCFTFIQHHKCFDGFIANRIFNTHNHDVLNFRVRGDNIFQLHAGDILAATFNHIAAAIDEIKKVLFVAVSGIAGMQPAIGFQHFRSGLWVEKVLFEDGFPRYAFNADFTDLTSRERAIVIVADLRFVAKPHPTNRRPASFIARTIADNAHNGFRHPIRRQQANAEAFTKRIFFLLRQVEIKHHFPQAVLLIVRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SVIISWQGSSLLFHLFQILSNYFPRAITWQIFSDLQIRGHFKFRHILFAEAEKFLFAQCFTFIQHHKCFDGFIANRIFNTHNHDVLNFRVRGDNIFQLHAGDILAATFNHIAAAIDEIKKVLFVAVSGIAGMQPAIGFQHFRSGLWVEKVLFEDGFPRYAFNADFTDLTSRERAIVIVADLRFVAKPHPTNRRPASFIARTIADNAHNGFRHPIRRQQANAEAFTKRIFFLLRQVEIKHHFPQAVLLIVR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SVIISWQGSSLLFHLFQILSNYFPRAITWQIFSDLQIRGHFKFRHILFAEAEKFLFAQCFTFIQHHKCFDGFIANRIFNTHNHDVLNFRVRGDNIFQLHAGDILAATFNHIAAAIDEIKKVLFVAVSGIAGMQPAIGFQHFRSGLWVEKVLFEDGFPRYAFNADFTDLTSRERAIVIVADLRFVAKPHPTNRRPASFIARTIADNAHNGFRHPIRRQQANAEAFTKRIFFLLRQVEIKHHFPQAVLLIVR ")
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
							pcdata("256714518")
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
					pcdata("233")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("233")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 36982 37317 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("112")
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
									pcdata("196.437621086886")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("498")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.08903931929937e-52")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("112")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("36982")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("37317")
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
									pcdata("111")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("111")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("112")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RRLYHQRTDHHRNTFGNGGAVLLYLSPEGAGAVFLDQXXXXXXXXXXXXXSTLAIDMKRRHYRQVDVIVTQCTLPGGVISSEAQVVMGNHHTFRSGGGAGGEENFRRIVSRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRLYHQRTDHHRNTFGNGGAVLLYLSPEGAGAVFLDQHKGGPGRKRRHRRSTLAIDMKRRHYRQVDVIVTQCTLPGGVISSEAQVVMGNHHTFRSGGGAGGEENFRRIVSR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RRLYHQRTDHHRNTFGNGGAVLLYLSPEGAGAVFLDQHKGGPGRKRRHRRSTLAIDMKRRHYRQVDVIVTQCTLPGGVISSEAQVVMGNHHTFRSGGGAGGEENFRRIVSR ")
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
							pcdata("58766392")
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
					pcdata("234")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("234")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 37318 37626 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("103")
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
									pcdata("206.838009636654")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("525")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.97092006460853e-55")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("37318")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("37626")
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
									pcdata("102")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RRCIAQGGRLLIFQLSKRTHAIISGKCHICQANVPQLSGIFLLNLLIHRIELRTGHQQARLAILQDPRAFLAQQAGVNRHHNRANFCQPEPAKDKFRAVVEMX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRCIAQGGRLLIFQLSKRTHAIISGKCHICQANVPQLSGIFLLNLLIHRIELRTGHQQARLAILQDPRAFLAQQAGVNRHHNRANFCQPEPAKDKFRAVVEM*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RRCIAQGGRLLIFQLSKRTHAIISGKCHICQANVPQLSGIFLLNLLIHRIELRTGHQQARLAILQDPRAFLAQQAGVNRHHNRANFCQPEPAKDKFRAVVEM ")
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
							pcdata("47941066")
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
					pcdata("235")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("235")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 37627 37698 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("48.5209839346223")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("114")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.06458804181472e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("37627")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("37698")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CNLVAFANPQRIKQVCRAVNLLIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CNLVAFANPQRIKQVCRAVNLLI*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CNLVAFANPQRIKQVCRAVNLLI ")
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
							pcdata("37117392")
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
					pcdata("236")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("236")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 37699 37800 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("34")
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
									pcdata("69.7069606100767")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("169")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.24065816681188e-14")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("37699")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("37800")
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
									pcdata("33")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("33")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("34")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LKITIPVNDSAAGFTNQRRFMTVNVRKIVPHLTX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LKITIPVNDSAAGFTNQRRFMTVNVRKIVPHLT*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LKITIPVNDSAAGFTNQRRFMTVNVRKIVPHLT ")
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
							pcdata("38663725")
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
					pcdata("237")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("237")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 37801 37887 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("29")
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
									pcdata("60.0769712121429")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("144")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.19601250064185e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("37801")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("37887")
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
									pcdata("28")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("29")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("MLSANDIHCTSIHFCSFVIGRALVRQADX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("MLSANDIHCTSIHFCSFVIGRALVRQAD*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("MLSANDIHCTSIHFCSFVIGRALVRQAD ")
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
							pcdata("38663850")
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
					pcdata("238")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("238")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 37888 38037 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("50")
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
									pcdata("100.522926683465")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("249")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.65076685265625e-23")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("37888")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("38037")
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
									pcdata("48")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("48")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("50")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RHAFSPQLWPDLCXPTPECRFRCSRYFSKWQPCPCHVRAFRARFVRGYFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RHAFSPQLWPDLC*PTPECRFRCSRYFSKWQPCPCHVRAFRARFVRGYF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RHAFSPQLWPDLC PTPECRFRCSRYFSKWQPCPCHVRAFRARFVRGYF ")
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
							pcdata("38663325")
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
					pcdata("239")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("239")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38038 38049 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("4")
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
							pcdata("6186232")
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
					pcdata("240")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("240")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38050 38277 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("76")
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
									pcdata("152.524869432308")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("384")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.97446910554351e-39")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("38050")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("38277")
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
									pcdata("74")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("74")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("76")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("CSALCRCDHPSFATEXLIREPHTVDCFPVPVRSALWRRRYSVQQSVLLLPRWYVSPARHRVSLQPVGRKGIRYGGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("CSALCRCDHPSFATE*LIREPHTVDCFPVPVRSALWRRRYSVQQSVLLLPRWYVSPARHRVSLQPVGRKGIRYGG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("CSALCRCDHPSFATE LIREPHTVDCFPVPVRSALWRRRYSVQQSVLLLPRWYVSPARHRVSLQPVGRKGIRYGG ")
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
							pcdata("38662675")
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
					pcdata("241")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("241")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38278 38337 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("20")
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
									pcdata("5.47117695831896e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("38278")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("38337")
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
									pcdata("20")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AECLPGFLACQADVRGQYLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AECLPGFLACQADVRGQYL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AECLPGFLACQADVRGQYL ")
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
							pcdata("30931160")
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
					pcdata("242")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("242")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38338 38469 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("44")
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
									pcdata("86.6557419504402")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("213")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.83104451050438e-19")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("38338")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("38469")
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
									pcdata("42")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("42")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("STLQFGXCPPVPSRCSRRHIYTDRNRRSRDLYLCSCAGTFRRRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("STLQFG*CPPVPSRCSRRHIYTDRNRRSRDLYLCSCAGTFRRR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("STLQFG CPPVPSRCSRRHIYTDRNRRSRDLYLCSCAGTFRRR ")
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
							pcdata("38663475")
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
					pcdata("243")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("243")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38470 38676 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("69")
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
									pcdata("139.0428842752")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("349")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.45038008388566e-35")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("38470")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("38676")
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
									pcdata("67")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("67")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SNPSXSTGPYAASLHTWRCRYFRLYRFHALYAVLPAPPWWRSGQKSNRRYTGRQALAGQHHRSESGNHX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SNPS*STGPYAASLHTWRCRYFRLYRFHALYAVLPAPPWWRSGQKSNRRYTGRQALAGQHHRSESGNH*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("SNPS STGPYAASLHTWRCRYFRLYRFHALYAVLPAPPWWRSGQKSNRRYTGRQALAGQHHRSESGNH ")
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
							pcdata("38662850")
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
					pcdata("244")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("244")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38677 38895 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("73")
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
									pcdata("143.665279186209")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("361")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.68519355620374e-36")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("73")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("38677")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("38895")
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
									pcdata("68")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("73")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RPGRXCYRPEDWCTPRCRTDQNRTGERXPVSGCAATVLHRXCHDGQRPDLYWLRXRYRWFPLISETLRGLHLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RPGR*CYRPEDWCTPRCRTDQNRTGER*PVSGCAATVLHR*CHDGQRPDLYWLR*RYRWFPLISETLRGLHL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RPGR CYRPEDWCTPRCRTDQNRTGER PVSGCAATVLHR CHDGQRPDLYWLR RYRWFPLISETLRGLHL ")
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
							pcdata("38662750")
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
					pcdata("245")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("245")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 38896 39141 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("82")
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
									pcdata("73.5589563692502")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("179")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.32819059312301e-15")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("82")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("38896")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("39141")
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
									pcdata("80")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("80")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("82")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KYLTTARANCGGRVAGSSVEREWCXXXXXXXXXXXXXXXXXTXXXXXXXXXXXXXXXXXXXXXXXXSAWVDDPLFAPEKFMX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KYLTTARANCGGRVAGSSVEREWCRPRRRSRSR*LPRPILRTFARQTVRRFRYRRRQRAIRPTARISAWVDDPLFAPEKFM*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KYLTTARANCGGRVAGSSVEREWCRPRRRSRSR LPRPILRTFARQTVRRFRYRRRQRAIRPTARISAWVDDPLFAPEKFM ")
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
							pcdata("38662525")
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
					pcdata("246")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("246")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 39142 39519 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("126")
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
									pcdata("259.995551113249")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("663")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.01447581860244e-71")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("126")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("39142")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("39519")
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
									pcdata("125")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("125")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("126")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RMTAGCGVNALSGLHSCSRRPDKTHQRRIRQRTDLAVLLQHRTTQRQDLHFVRSPGDAVYTQITPEAADAVVARNPDTAQHLHCAIHNFKCRIGAEVFAHRCIARGDGAVVCFPRCFIQHVFHGVX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RMTAGCGVNALSGLHSCSRRPDKTHQRRIRQRTDLAVLLQHRTTQRQDLHFVRSPGDAVYTQITPEAADAVVARNPDTAQHLHCAIHNFKCRIGAEVFAHRCIARGDGAVVCFPRCFIQHVFHGV*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RMTAGCGVNALSGLHSCSRRPDKTHQRRIRQRTDLAVLLQHRTTQRQDLHFVRSPGDAVYTQITPEAADAVVARNPDTAQHLHCAIHNFKCRIGAEVFAHRCIARGDGAVVCFPRCFIQHVFHGV ")
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
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.14780425327886e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2833090")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2833170")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AGCGVNALSGLHSCSRRPDKTHQRRIR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AGCGVNALSGLHMRPRRSDKTRQRRIR")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AGCGVNALSGLH   RR DKT QRRIR")
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
									pcdata("43.5133894476967")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("101")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.37648950510095e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3752883")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3752966")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AGCGVNALSGLHSCSRRPDKTHQRRIRQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AGCGVYALSGLRSGSRRHDKTRKRRIRQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AGCGV ALSGL S SRR DKT +RRIRQ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("43.1281898717793")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("100")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.3958358705084e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3229365")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3229442")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RMTAGCGVNALSGLHSCSRRPDKTHQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RATGGCGVNALSALHVCSRRSDKTRQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R T GCGVNALS LH CSRR DKT Q")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("41.5873915681099")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("96")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.77977226597401e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4243154")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4243243")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("TAGCGVNALSGLHSCSR-RPDKTHQRRIRQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("NAGCGVNALSGLQNRNVCRPDKTRQRRIRQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" AGCGVNALSGL + +  RPDKT QRRIRQ")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("41.5873915681099")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("96")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.94696331561009e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("762027")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("762104")
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
									pcdata("20")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("20")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("26")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("GCGVNALSGLHSCSRRPDKTHQRRIR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("GCGACALSDLHWWSRRPDKTRQRRIR")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("GCG  ALS LH  SRRPDKT QRRIR")
									] )
								] ),
							element( 'Hsp',
								[],
								[
								element( 'Hsp_num',
									[],
									[
									pcdata("7")
									] ),
								element( 'Hsp_bit-score',
									[],
									[
									pcdata("40.8169924162752")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("4.11453265102823e-05")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4025524")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4025625")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AGCGVNALSGL---HSCSRRPDKTHQRRIRQRTDLA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AGCGVNALSGLPIRHQC--RPDKTRQRRIRHHAPIA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AGCGVNALSGL   H C  RPDKT QRRIR    +A")
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
							pcdata("77324100")
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
					pcdata("247")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("247")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 39520 40023 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("168")
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
									pcdata("303.52310319191")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("776")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.89067931537614e-84")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("168")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("39520")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("40023")
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
									pcdata("167")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("167")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("168")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FDRHVGEFFLNQLETTNSLAKLHALIGVARRIFKGAHRRTVVGEGYQETFMVELFFDAVKAVTFPTEHVFLVQFHVVKGDFTAAIHTQTELFKFGHFDARFAHINKPFGVDRFVRRSPVARHHHDVRGVGAAGXXXXXXXXXXXXXSTGISRFQATHVGARARFGNRX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FDRHVGEFFLNQLETTNSLAKLHALIGVARRIFKGAHRRTVVGEGYQETFMVELFFDAVKAVTFPTEHVFLVQFHVVKGDFTAAIHTQTELFKFGHFDARFAHINKPFGVDRFVRRSPVARHHHDVRGVGAAGNKTLTTIKINLTISTGISRFQATHVGARARFGNR*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FDRHVGEFFLNQLETTNSLAKLHALIGVARRIFKGAHRRTVVGEGYQETFMVELFFDAVKAVTFPTEHVFLVQFHVVKGDFTAAIHTQTELFKFGHFDARFAHINKPFGVDRFVRRSPVARHHHDVRGVGAAGNKTLTTIKINLTISTGISRFQATHVGARARFGNR ")
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
									pcdata("58.9213724843908")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("141")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.70154665215214e-10")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("127")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1775706")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1776089")
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
									pcdata("39")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("61")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("128")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FDRHVGEFFLNQLETTNSLAKLHALIGVARRIFKGAHRRTVVGEGYQETFMVELFFDAVKAVTFPTEHVFLVQFHVVKGDFTAAIHTQTELFKFGHFDARFAHINKPFGVDRF-VRRSPVARHHHDVR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LNRHQRQFFLDHLVMTNGLTKSDTLVGIAGGIFKGAFGKAGAARGVDQALHLKVVHHIEETHSFFAHHVALFNLHVVEIDFAGAEHMPADFMQRVNLNAGLTGVDPPQGEGFFGIFRLRIARQHQHIR")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata(" +RH  +FFL+ L  TN L K   L+G+A  IFKGA  +     G  +   +++     +  +F   HV L   HVV+ DF  A H   +  +  + +A    ++ P G   F + R  +AR H  +R")
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
							pcdata("136090064")
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
					pcdata("248")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("248")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 40024 40374 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("117")
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
									pcdata("238.424374861877")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("607")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.11071672124156e-64")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("117")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("40024")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("40374")
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
									pcdata("116")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("116")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("117")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VPHLLTGAAESHNFVDLLLCAFAQEGVEPARQLVQHIGWCPQTSQLHPHGGESNKPRVQTTVLFRDQQTVDTHIRQCFDKTFRVDAVTVALGKIGLPVFAGHQFADTGDKQFLLIIX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("VPHLLTGAAESHNFVDLLLCAFAQEGVEPARQLVQHIGWCPQTSQLHPHGGESNKPRVQTTVLFRDQQTVDTHIRQCFDKTFRVDAVTVALGKIGLPVFAGHQFADTGDKQFLLII*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("VPHLLTGAAESHNFVDLLLCAFAQEGVEPARQLVQHIGWCPQTSQLHPHGGESNKPRVQTTVLFRDQQTVDTHIRQCFDKTFRVDAVTVALGKIGLPVFAGHQFADTGDKQFLLII ")
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
							pcdata("64952286")
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
					pcdata("249")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("249")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 40375 40401 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("9")
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
							pcdata("13919022")
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
					pcdata("250")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("250")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 40402 40575 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("58")
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
									pcdata("93.5893343169526")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("231")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.56523121698671e-21")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("40402")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("40539")
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
									pcdata("45")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("45")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("YFGGXLIFPVLFRVFNKGERHHNVDEEQRASSGDNGGLNRFQAAER")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("YFGG*LIFPVLFRVFNKGERHHNVDEEQRASSGDNGGLNRFQAAER")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("YFGG LIFPVLFRVFNKGERHHNVDEEQRASSGDNGGLNRFQAAER")
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
							pcdata("38663125")
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
					pcdata("251")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("251")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 40576 40860 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("95")
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
									pcdata("199.519217694225")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("506")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.88949340297889e-53")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("40576")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("40860")
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
									pcdata("94")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("94")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("95")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PTDTHQQRWFFTIAYFAASGHRQGIRAGVNQRNGGNKAEDEEAPHGGGAEWQSGPGFNDGARHTVLFDQIWNVDDVFIYQQQSVTTEYSPQDPGX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PTDTHQQRWFFTIAYFAASGHRQGIRAGVNQRNGGNKAEDEEAPHGGGAEWQSGPGFNDGARHTVLFDQIWNVDDVFIYQQQSVTTEYSPQDPG*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("PTDTHQQRWFFTIAYFAASGHRQGIRAGVNQRNGGNKAEDEEAPHGGGAEWQSGPGFNDGARHTVLFDQIWNVDDVFIYQQQSVTTEYSPQDPG ")
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
							pcdata("38662200")
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
					pcdata("252")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("252")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 40861 41472 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("204")
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
									pcdata("414.460581056108")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("1064")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.66134688282599e-117")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("204")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("40861")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("41472")
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
									pcdata("203")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("203")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("204")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RCQPQHHAEAQFTHSTTTGDAGEEDTHLDSINHPPCPVEHGPALREAAFSDGIGIEQHARQIHQQHPHRIGEVVHDEAGTADNEHPTQHQEAQVTTHVTGDTYPFLQTASDANGVEDNPAGNDDSVQLQGMRNAKQPLHTLGHQRRGKAQTGTHREDQGDEIEVVDDSAEQPFGVFFTYQRHQCRAGANHFHFADKEEVSEGDX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RCQPQHHAEAQFTHSTTTGDAGEEDTHLDSINHPPCPVEHGPALREAAFSDGIGIEQHARQIHQQHPHRIGEVVHDEAGTADNEHPTQHQEAQVTTHVTGDTYPFLQTASDANGVEDNPAGNDDSVQLQGMRNAKQPLHTLGHQRRGKAQTGTHREDQGDEIEVVDDSAEQPFGVFFTYQRHQCRAGANHFHFADKEEVSEGD*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RCQPQHHAEAQFTHSTTTGDAGEEDTHLDSINHPPCPVEHGPALREAAFSDGIGIEQHARQIHQQHPHRIGEVVHDEAGTADNEHPTQHQEAQVTTHVTGDTYPFLQTASDANGVEDNPAGNDDSVQLQGMRNAKQPLHTLGHQRRGKAQTGTHREDQGDEIEVVDDSAEQPFGVFFTYQRHQCRAGANHFHFADKEEVSEGD ")
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
									pcdata("44.2837885995314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.3104344786204e-06")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("12")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("130")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1881725")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1882075")
									] ),
								element( 'Hsp_query-frame',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("37")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("54")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("6")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("121")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FTHSTTTGDAGEEDTHLDSINHPPCPVEHGPALREAAFSDGIGIEQHARQIHQQHPHRIGEVVHDEAGTADNEHPTQHQEAQVTTHVTGDTYPFLQTASDAN--GVEDNPAGNDDSVQLQG")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FFNLTTAGNAGDKHCYARGV*DPPQPVENGPVAGK*TIAKRIGE*AHLQEVLRGQADGIDDAVGNKFGWADNQHQ*WQQERAPANHFA----QTLQTVVDTNPRA*AKNAAGKQDHKCLHG")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("F + TT G+AG++  +   +  PP PVE+GP   +   +  IG   H +++ +     I + V ++ G ADN+H    QE     H        LQT  D N      N AG  D   L G")
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
							pcdata("188670072")
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
					pcdata("253")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("253")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 41473 41565 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("31")
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
									pcdata("61.6177695158123")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("148")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.33083810251451e-11")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("41473")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("41565")
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
									pcdata("30")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("30")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("31")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("KEAVSGPRQRSPVEQAVSQPQLFRPCRVRFX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KEAVSGPRQRSPVEQAVSQPQLFRPCRVRF*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("KEAVSGPRQRSPVEQAVSQPQLFRPCRVRF ")
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
							pcdata("38663800")
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
					pcdata("254")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("254")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 41566 41877 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("104")
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
									pcdata("216.082799458671")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("549")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.45806904944639e-58")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("104")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("41566")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("41877")
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
									pcdata("102")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("102")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("104")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("AKRRGGDVVVDLNRSPEQYGSRRTGGEHHKDPTGGAKFWRFVTXTLFGIRAKHQPEPTEKHHHHIPFKCPSPYIGDVTEYSINNDIRCIQISDCKPTKYADDYX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("AKRRGGDVVVDLNRSPEQYGSRRTGGEHHKDPTGGAKFWRFVT*TLFGIRAKHQPEPTEKHHHHIPFKCPSPYIGDVTEYSINNDIRCIQISDCKPTKYADDY*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AKRRGGDVVVDLNRSPEQYGSRRTGGEHHKDPTGGAKFWRFVT TLFGIRAKHQPEPTEKHHHHIPFKCPSPYIGDVTEYSINNDIRCIQISDCKPTKYADDY ")
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
							pcdata("47941035")
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
					pcdata("255")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("255")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 41878 41949 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("24")
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
									pcdata("46.9801856309529")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("110")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.6433753947924e-07")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("41878")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("41949")
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
									pcdata("23")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("23")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("0")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RRKENLRFYSRFSLFILHELIPLX")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("RRKENLRFYSRFSLFILHELIPL*")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("RRKENLRFYSRFSLFILHELIPL ")
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
							pcdata("37117392")
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
					pcdata("256")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("256")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 41950 41994 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("15")
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
							pcdata("23198370")
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
					pcdata("257")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("257")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 41995 42006 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("4")
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
							pcdata("6186232")
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
					pcdata("258")
					] ),
				element( 'Iteration_query-ID',
					[],
					[
					pcdata("258")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("U00096 42007 42075 +1")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("23")
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
							pcdata("35570834")
							] ),
						element( 'Statistics_kappa',