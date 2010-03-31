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
			pcdata("tblastn 2.2.21 [Jun-14-2009]")
			] ),
		element( 'BlastOutput_reference',
			[],
			[
			pcdata("~Reference: Altschul, Stephen F., Thomas L. Madden, Alejandro A. Schaffer, ~Jinghui Zhang, Zheng Zhang, Webb Miller, and David J. Lipman (1997), ~""Gapped BLAST and PSI-BLAST: a new generation of protein database search~programs"",  Nucleic Acids Res. 25:3389-3402.")
			] ),
		element( 'BlastOutput_db',
			[],
			[
			pcdata("EnteroBacteriales_RS.nt")
			] ),
		element( 'BlastOutput_query-ID',
			[],
			[
			pcdata("lcl|1_0")
			] ),
		element( 'BlastOutput_query-def',
			[],
			[
			pcdata("u00096_25183/25205-25701_+1_AA")
			] ),
		element( 'BlastOutput_query-len',
			[],
			[
			pcdata("165")
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
					pcdata("10")
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
					pcdata("F")
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
					pcdata("lcl|1_0")
					] ),
				element( 'Iteration_query-def',
					[],
					[
					pcdata("u00096_25183/25205-25701_+1_AA")
					] ),
				element( 'Iteration_query-len',
					[],
					[
					pcdata("165")
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
							pcdata("lcl|NC_000913")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Escherichia coli str. K-12 substr. MG1655, complete genome")
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
									pcdata("308.145")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("788")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.57609e-084")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("25204")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("25698")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("154")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("154")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMSQSICSTGLRWLWLVVVVLIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LMSQSICSTG           IIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
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
									pcdata("28.4906")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("62")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.04477")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("72")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("132")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2057262")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2057444")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("18")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("61")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("FFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGD")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("FFAAIDLGTTVVVAFSLGKRDRRRARVATRQSLVIMTLFAVLLATLIHHFGEQIIDFVAGD")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("FFA I +G +V++A  + +    +       +L+I      L   L H F   +IDF  GD")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("2")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_011283")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Klebsiella pneumoniae 342, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_011283")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("5641239")
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
									pcdata("286.574")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("732")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.90028e-078")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4785025")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4785519")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("139")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("148")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMSKSICSTGLRWLWVVVAVLIIDLGSKFLILQNFALGETVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFSGIAIGICVVLTVLMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADSAICIGAALIVLEGFLPKPTAKE")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LMS+SICSTG           IIDLGSK+LILQNFALG+TVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFF+GIAIGI V+L V+MYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLAD+AIC+GAALIVLEGFLP    K+")
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
									pcdata("28.4906")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("62")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.45098")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("69")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("123")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("5582584")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("5582757")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("16")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("28")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("59")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("QRWFFAGIAIGISVILAVMMY----RSKATQKLNNIAYALIIGGALGNLFDRLWHGFVV")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("QAWAVA-IALGVEDKITAPMFEAVQKTQTVQSVADIRKVFVDAGVKGEDYDAAWNSFVV")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("Q W  A IA+G+   +   M+    +++  Q + +I    +  G  G  +D  W+ FVV")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("3")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_010067")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Salmonella enterica subsp. arizonae serovar 62:z4,z23:--, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_010067")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4600800")
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
									pcdata("284.263")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("726")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.37696e-077")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2874124")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2874618")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("141")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("146")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMSKPLCSTGLRWLWLVVVVLIIDLGSKYLILQNFALGDTVGLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGICVILVAMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADSAICIGAALIVLEGFLPKPAAKE")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LMS+ +CSTG           IIDLGSKYLILQNFALGDTV LFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGI VIL  MMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLAD+AIC+GAALIVLEGFLP  A K+")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("4")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_009792")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Citrobacter koseri ATCC BAA-895, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_009792")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4720462")
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
									pcdata("280.411")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("716")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("3.46108e-076")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("161")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3134603")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3135085")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("139")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("144")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("161")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMSKPLCSTGLRWLWLVVVVLIIDLGSKYLILQNFALGDTVSLFSSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGICVILMVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADSAICIGAALIVLEGFLPKK")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LMS+ +CSTG           IIDLGSKYLILQNFALGDTV LF SLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGI VIL VMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLAD+AIC+GAALIVLEGFLP +")
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
									pcdata("27.7202")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("60")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("5.0652")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("70")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("137")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("2117147")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("2117338")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("36")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("68")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFAT")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("KWFVRGI-VGLTE*VVFIDAQRVIGQKMD-LTDVQFVGGKIGNGFDRLFIG--VKTLNERHADDHFFT")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+WF  GI +G++  +  +  +    QK++ +     +GG +GN FDRL+ G  V  ++    D HF T")
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
									pcdata("26.5646")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.008")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("63")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("97")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("25198")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("25302")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-2")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("13")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("22")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("35")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("ADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKL")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("ADTGSQQGYFLAGRSLKAPVIAASLMLTNLSTEQL")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("AD+G  Q +F AG ++   VI A +M  + +T++L")
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
									pcdata("26.5646")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("57")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("9.95662")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("90")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("114")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1511627")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1511701")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("12")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("15")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("25")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("RSKATQKLNNIAYALIIGGALGNLF")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("REAATESLNAIAQRMVIGDLLGQAF")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("R  AT+ LN IA  ++IG  LG  F")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("5")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_009436")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Enterobacter sp. 638, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_009436")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4518712")
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
									pcdata("279.641")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("714")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.91789e-076")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("659164")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("659658")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("135")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("148")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMTKSFCSTGLRWLWLVVVVLIIDLGSKFLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIALGICLVLTVMMYRAKASQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGNWHFATFNLADSAICFGAAMIVLEGFLPNAAAKK")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LM++S CSTG           IIDLGSK+LILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIA+GI ++L VMMYR+KA+QKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVG+WHFATFNLAD+AIC GAA+IVLEGFLP+ A K+")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("6")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_004547")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Erwinia carotovora subsp. atroseptica SCRI1043, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_004547")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("5064019")
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
									pcdata("241.891")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("616")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.4223e-064")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("4334865")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("4335371")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("121")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("132")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("169")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("MSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSR-----AKKQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMNKICSSGLRWLWLAILVLVIDLGSKQWILAHFALGDTVPVMPSLNLHYARNYGAAFSFLADKGGWQRWFFAGIAIAIVVALLVMMYRGSVKQRLNNIAYSLIIGGALGNLFDRTWHGFVVDFIDFYVGNWHFATFNLADTAICIGAALIVLEGFFSTHDDTDIAKKK")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+   ICS+G           +IDLGSK  IL +FALGDTVP+ PSLNLHYARNYGAAFSFLAD GGWQRWFFAGIAI I V L VMMYR    Q+LNNIAY+LIIGGALGNLFDR WHGFVVD IDFYVG+WHFATFNLADTAIC+GAALIVLEGF  +      AKK+")
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
									pcdata("28.4906")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("62")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("2.37053")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("123")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("161")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1430943")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1431065")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("15")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("21")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("2")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("VDMIDFYVGD--WHFATFNLADTAICVGAALIVLEGFLPSR")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("IDNIESIAGDEAWEMAVFNLYNRIQETGRALLLITGDRPPR")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+D I+   GD  W  A FNL +     G AL+++ G  P R")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("7")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_008800")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Yersinia enterocolitica subsp. enterocolitica 8081, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_008800")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("4615899")
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
									pcdata("215.312")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("547")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("1.36735e-056")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("164")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("710398")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("710889")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("104")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("125")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("164")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKK")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LMSKPICSTGLRWLWLAVLVVIVDLSSKQWVMTHFALYESVPLIPFFNLTYAQNFGAAFSFLADKSGWQRWFFAGIAIGISVLLMVLMYRSTAKQRLLNCAYALIIGGALGNLFDRMVHGAVIDFIDFHVNNWHFPTFNIADTAICIGAALVIFEGFISPAEKT")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LMS+ ICSTG           I+DL SK  ++ +FAL ++VPL P  NL YA+N+GAAFSFLAD  GWQRWFFAGIAIGISV+L V+MYRS A Q+L N AYALIIGGALGNLFDR+ HG V+D IDF+V +WHF TFN+ADTAIC+GAAL++ EGF+    K ")
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
									pcdata("32.7278")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("73")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("0.141271")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("65")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("144")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("3842897")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("3843145")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("24")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("41")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("5")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("85")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("SGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNL-----FDRLWHGFVVDMIDFYVGDWHFATFNLADTA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("SCGSERIAYAGVQVGLAFLLTVLQGFGPSTDL--GVALDRVLGILLGNLVVYLIFTRLWPVAIADAVHIHIRNALKGLTNLATLA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("S G +R  +AG+ +G++ +L V+     +T     +A   ++G  LGNL     F RLW   + D +  ++ +      NLA  A")
									] )
								] )
							] )
						] ),
					element( 'Hit',
						[],
						[
						element( 'Hit_num',
							[],
							[
							pcdata("8")
							] ),
						element( 'Hit_id',
							[],
							[
							pcdata("lcl|NC_010694")
							] ),
						element( 'Hit_def',
							[],
							[
							pcdata("Erwinia tasmaniensis Et1/99, complete genome")
							] ),
						element( 'Hit_accession',
							[],
							[
							pcdata("NC_010694")
							] ),
						element( 'Hit_len',
							[],
							[
							pcdata("3883467")
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
									pcdata("199.904")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("507")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("6.19818e-052")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("1")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("794973")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("795467")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("123")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("165")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LMSQSICSTGXXXXXXXXXXXIIDLGSKYLILQNFALGDTVPLFPSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMMYRSKATQKLNNIAYALIIGGALGNLFDRLWHGFVVDMIDFYVGDWHFATFNLADTAICVGAALIVLEGFLPSRAKKQ")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("MMSKPVLSTGLRWLWLVLVVIAIDFVSKQWIMNNLMLHESMPVMPFFNFFYAHNYGAAFSFLADKGGWQRWFFAGIAVAIVVVLLVMMYRSKASDRLNNIAYALIVGGALGNLFDRAYHGFVVDFIDFTIGDWHFATFNIADCGICIGAALIVLEGFINPTSKRS")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("+MS+ + STG            ID  SK  I+ N  L +++P+ P  N  YA NYGAAFSFLAD GGWQRWFFAGIA+ I V+L VMMYRSKA+ +LNNIAYALI+GGALGNLFDR +HGFVVD IDF +GDWHFATFN+AD  IC+GAALIVLEGF+   +K+ ")
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
									pcdata("30.0314")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("66")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("0.915692")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("45")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("88")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1352469")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1352585")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("19")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("44")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("PSLNLHYARNYGAAFSFLADSGGWQRWFFAGIAIGISVILAVMM")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("PAL-LARAGNKGLAFS----ENGYLRWYLASMSIGAVVVLALLM")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("P+L L  A N G AFS      G+ RW+ A ++IG  V+LA++M")
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
									pcdata("26.9498")
									] ),
								element( 'Hsp_score',
									[],
									[
									pcdata("58")
									] ),
								element( 'Hsp_evalue',
									[],
									[
									pcdata("7.25126")
									] ),
								element( 'Hsp_query-from',
									[],
									[
									pcdata("62")
									] ),
								element( 'Hsp_query-to',
									[],
									[
									pcdata("103")
									] ),
								element( 'Hsp_hit-from',
									[],
									[
									pcdata("1755362")
									] ),
								element( 'Hsp_hit-to',
									[],
									[
									pcdata("1755499")
									] ),
								element( 'Hsp_hit-frame',
									[],
									[
									pcdata("-3")
									] ),
								element( 'Hsp_identity',
									[],
									[
									pcdata("15")
									] ),
								element( 'Hsp_positive',
									[],
									[
									pcdata("27")
									] ),
								element( 'Hsp_gaps',
									[],
									[
									pcdata("4")
									] ),
								element( 'Hsp_align-len',
									[],
									[
									pcdata("46")
									] ),
								element( 'Hsp_qseq',
									[],
									[
									pcdata("LADSGGWQR--WFFAGIAIGISVILAVMMYRSKA--TQKLNNIAYA")
									] ),
								element( 'Hsp_hseq',
									[],
									[
									pcdata("LADAFGWRANFWFLAAYAVFIAALIALFLPETRPLDTQKVKGLPLA")
									] ),
								element( 'Hsp_midline',
									[],
									[
									pcdata("LAD+ GW+   WF A  A+ I+ ++A+ +  ++   TQK+  +  A")
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
							pcdata("8")
							] ),
						element( 'Statistics_db-len',
							[],
							[
							pcdata("12561424")
							] ),
						element( 'Statistics_hsp-len',
							[],
							[
							pcdata("93")
							] ),
						element( 'Statistics_eff-space',
							[],
							[
							pcdata("9.04369e+008")
							] ),
						element( 'Statistics_kappa',
							[],
							[
							pcdata("0.041")
							] ),
						element( 'Statistics_lambda',
							[],
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
