# ========================================
# Analysis of FIDES - Quotas and Quota Thresholds by Country
	# 2021-09-05, Nuno Prista, SLU, Sweden 
# ========================================

# Dev notes	
	# Still to be resolved - should UK feature in the % EU?
	# Interpretation of Comm Impl Dec 2021/1168 chpt II 2(a)
	# In some cases, there are no quotas per country - e.g., WHB *04A-C; BLI *24X7C - I do not know how to handle this cases.
	# how to evaluate the 25% threshold in multi-year situations

# Input files:
	# FIDES extracts obtained from HaV

rm(list=ls())

library(data.table)
library(xlsx)

FIDES<-data.table(read.table('Inputs/Quotas2018-2020.txt', sep="\t", header=T, quote=""))

# a look into object structure	
FIDES[,.N,list(Definition.Year, Species.Code, Area.Code, Level.Description)]

# creates ID	
FIDES$QuotaID<-paste(FIDES$Definition.Year,FIDES$Species.Code, FIDES$Area.Code)
FIDES[QuotaID=="2019 SAN 234_4",]

# select country
targetCTR<-"SWE"

# CTR quotas
	QuotaTableCTR<-FIDES[,list(CTR = targetCTR, QuotaInitialCTR = sum(Initial.Quantity[Level.Code==targetCTR]), QuotaAdaptedCTR = sum(Adapted.Quota[Level.Code==targetCTR]), QuotaInitialEU=sum(Initial.Quantity[Level.Code=="EEC"]), QuotaAdaptedEU=sum(Adapted.Quota[Level.Code=="EEC"])),list(QuotaID, Definition.Year, Species.Code, Area.Code)]
	QuotaTableCTR$PercQuotaInitialCTRinEU<-QuotaTableCTR$QuotaInitialCTR/QuotaTableCTR$QuotaInitialEU
	QuotaTableCTR$PercQuotaAdaptedCTRinEU<-QuotaTableCTR$QuotaAdaptedCTR/QuotaTableCTR$QuotaAdaptedEU
	
	# demo or basic quota table
	QuotaTableCTR[QuotaInitialCTR>0 | QuotaAdaptedCTR>0,]

# identification of situations when Quota of a country is < 10% but sum of minor (<10%) quotas > 25%
	# two alternatives computed:
		# sum of minor (<10%) quotas within EU > 25% EU
		# sum of minor (<10%) quotas within EU > 25% EU - looks like the one that most approximates 2021/1168 chpt II 2(a)
	
	quotas_countries<-FIDES[,list(QuotaInitialCTR = sum(Initial.Quantity),QuotaAdaptedCTR = sum(Adapted.Quota)),list(QuotaID, Definition.Year, Species.Code, Area.Code, Level.Description)]
	
	quotas_eu<-FIDES[Level.Description=="European union",list(QuotaInitialUE = sum(Initial.Quantity), QuotaAdaptedEU = sum(Adapted.Quota)),list(QuotaID)]
	
	a<-merge(quotas_countries[!Level.Description %in% c("Faroe Islands", "European union", "Norway", "Total Allowable Catches for all countries"),], quotas_eu, by="QuotaID")
	
	# more reliable alternative (sum of countries)
	quotas_eu2<-FIDES[!Level.Description %in% c("Faroe Islands", "European union", "Norway", "Total Allowable Catches for all countries"),list(QuotaInitialUE2 = sum(Initial.Quantity, na.rm=T), QuotaAdaptedEU2 = sum(Adapted.Quota, na.rm=T)),list(QuotaID)]
	a<-merge(quotas_countries[!Level.Description %in% c("Faroe Islands", "European union", "Norway", "Total Allowable Catches for all countries"),], quotas_eu2, all.x=T)
	
	a$QuotaInitialCTR[is.na(a$QuotaInitialCTR)]<-0
	a$QuotaAdaptedCTR[is.na(a$QuotaAdaptedCTR)]<-0
	a$PercQuotaInitialCTRinEU<-a$QuotaInitialCTR/a$QuotaInitialUE2
	a$PercQuotaAdaptedCTRinEU<-a$QuotaAdaptedCTR/a$QuotaAdaptedEU2

	# TACs [TAC2 = sum of initial quantities of individual countries]
	tacs2<-FIDES[!Level.Description %in% c("European union", "Total Allowable Catches for all countries"),list(InitialTAC2 = sum(Initial.Quantity, na.rm=T)),list(QuotaID)]
	

	aux<-FIDES[Level.Description=="Total Allowable Catches for all countries",]
	# clean up of situations where TAC is 0
		aux<-aux[!Initial.Quantity==0 & !Adapted.Quota==0,]
	
	a$InitialTAC <- aux$Initial.Quantity[match(a$QuotaID,aux$QuotaID)]
	a$AdaptedTAC <- aux$Adapted.Quota[match(a$QuotaID,aux$QuotaID)]
	a$InitialTAC2 <- tacs2$InitialTAC2[match(a$QuotaID,tacs2$QuotaID)]
	
	# InitialTAC == AdaptedTAC; InitialTAC2 inputed where missing
	a$TACtype <- NA
	a$TACtype[!is.na(a$AdaptedTAC)]<-"Original"
	a$TACtype[is.na(a$AdaptedTAC) & !is.na(a$InitialTAC2)]<-"SumOfCountries"
	a$InitialTAC[is.na(a$InitialTAC)]<-a$InitialTAC2[is.na(a$InitialTAC)]
	a$AdaptedTAC[is.na(a$AdaptedTAC)]<-a$InitialTAC2[is.na(a$AdaptedTAC)]

	
	#Adds QuotaTAC to QuotaTableCTR
		QuotaTableCTR$QuotaTAC<-a$AdaptedTAC[match(QuotaTableCTR$QuotaID,a$QuotaID)]
		QuotaTableCTR$QuotaTACtype<-a$TACtype[match(QuotaTableCTR$QuotaID,a$QuotaID)]
		QuotaTableCTR$PercQuotaInitialCTRinTAC<-QuotaTableCTR$QuotaInitialCTR/QuotaTableCTR$QuotaTAC
		QuotaTableCTR$PercQuotaAdaptedCTRinTAC<-QuotaTableCTR$QuotaAdaptedCTR/QuotaTableCTR$QuotaTAC

	a$PercQuotaInitialCTRinTAC<-a$QuotaInitialCTR/a$InitialTAC
	a$PercQuotaAdaptedCTRinTAC<-a$QuotaAdaptedCTR/a$AdaptedTAC
	
	# situations where the sum of the MS with small quotas exceeds 25% of the Union share of the TAC [note: regulation hints at 25% of the TAC, including non-EU!]
	a[a$PercQuotaAdaptedCTRinEU<0.1,list(sumPercQuota=sum(PercQuotaAdaptedCTRinEU)), QuotaID][sumPercQuota>.25,]
	QuotaTableAux<-a[a$PercQuotaAdaptedCTRinEU<0.1,list(sumPercQuota=sum(PercQuotaAdaptedCTRinEU)), QuotaID][sumPercQuota>.25,]
	
	# situations where the sum of the MS with small quotas exceeds 25% of the TAC 
	a[a$PercQuotaAdaptedCTRinEU<0.1,list(sumPercQuota=sum(PercQuotaAdaptedCTRinTAC)), QuotaID][sumPercQuota>.25,]
	QuotaTableAux<-a[a$PercQuotaAdaptedCTRinEU<0.1,list(sumPercQuota=sum(PercQuotaAdaptedCTRinTAC)), QuotaID][sumPercQuota>.25,]
	
	QuotaTableCTR$MinorQuotaSituation<-QuotaTableCTR$QuotaID %in% QuotaTableAux$QuotaID
		
		
	# averages for period
	QuotaTableCTR$QuotaIDPeriod<-paste(QuotaTableCTR$Species.Code, QuotaTableCTR$Area.Code)
		
	QuotaTableCTRPeriod<-QuotaTableCTR
	QuotaTableCTRPeriod$QuotaIDPeriod<-paste(QuotaTableCTRPeriod$Species.Code, QuotaTableCTRPeriod$Area.Code)
	
	QuotaTableCTRPeriod<-QuotaTableCTRPeriod[, list(Definition.Year=paste0(range(Definition.Year), collapse=":"),
					Species.Code = Species.Code[1], Area.Code = Area.Code[1], CTR = CTR[1],  
						PercQuotaInitialCTRinEU = sum(PercQuotaInitialCTRinEU, na.rm=T)/length(Definition.Year),
							PercQuotaAdaptedCTRinEU = sum(PercQuotaAdaptedCTRinEU, na.rm=T)/length(Definition.Year),
								PercQuotaInitialCTRinTAC = sum(PercQuotaInitialCTRinTAC, na.rm=T)/length(Definition.Year),
									PercQuotaAdaptedCTRinTAC = sum(PercQuotaAdaptedCTRinTAC, na.rm=T)/length(Definition.Year),
										YearsWithMinorQuotaSituation = sum(MinorQuotaSituation)), QuotaIDPeriod] 
	
	# adds threshols
		# less than 10% of union total
		QuotaTableCTR$Threshold_Initial_10prctEU<-QuotaTableCTR$PercQuotaInitialCTRinEU >= 0.1
		QuotaTableCTR$Threshold_Adapted_10prctEU<-QuotaTableCTR$PercQuotaAdaptedCTRinEU >= 0.1
		
		QuotaTableCTRPeriod$Threshold_Initial_10prctEU<-QuotaTableCTRPeriod$PercQuotaInitialCTRinEU >= 0.1
		QuotaTableCTRPeriod$Threshold_Adapted_10prctEU<-QuotaTableCTRPeriod$PercQuotaAdaptedCTRinEU >= 0.1

		# MinorQuotaSituation
		QuotaTableCTR$Adapted_MinorQuotasSituationInTAC<-QuotaTableCTR$PercQuotaAdaptedCTRinEU < 0.1 & QuotaTableCTR$MinorQuotaSituation == TRUE
		# ATT! - selects for sampling when at least one year has met threshold in MinorQuotasSituation
			# alternative could be to for that threshold in MinorQuotasSituation is met in 2 or 3 years
			# or calculate the threshold based on a average of the small countries % contributions over the 3 years
			# or calculate the threshold based on the sum of the small countries and sum of TACs over the 3 years
		QuotaTableCTRPeriod$Adapted_MinorQuotasSituationInTAC<-QuotaTableCTRPeriod$PercQuotaAdaptedCTRinEU < 0.1 & QuotaTableCTRPeriod$YearsWithMinorQuotaSituation>0 

# demonstration examples		
	# TAC missing in FIDES [obtained from SumOfCountries]
	QuotaTableCTRPeriod[QuotaIDPeriod=="HER *25B-F",]	
	QuotaTableCTR[QuotaIDPeriod=="HER *25B-F",]	
	FIDES[QuotaID=="2020 HER *25B-F",]		

	# TAC in FIDES[obtained from Original] and minorCatchesSituatio
	QuotaTableCTRPeriod[QuotaIDPeriod=="SOL 24-C.",]	
	QuotaTableCTR[QuotaIDPeriod=="SOL 24-C.",]	
	FIDES[QuotaID=="2019 SOL 24-C.",]	

	# Quotas and TACs per country not available
	QuotaTableCTRPeriod[QuotaIDPeriod=="BLI *24X7C",]	
	QuotaTableCTR[QuotaIDPeriod=="BLI *24X7C",]	
	FIDES[QuotaID=="2020 BLI *24X7C",]	

	QuotaTableCTRPeriod[QuotaIDPeriod=="WHB *04A-C",]	
	QuotaTableCTR[QuotaIDPeriod=="WHB *04A-C",]	
	FIDES[QuotaID=="2018 WHB *04A-C",]	

# save 
	filename<-paste0("Outputs/Quotas_",targetCTR,"_2018-2020.xlsx",sep="")
	write.xlsx(QuotaTableCTR, file = filename, sheetName=gsub("CTR",targetCTR,"QuotaTableCTR"), row.names = FALSE) 
	write.xlsx(QuotaTableCTRPeriod, file = filename, sheetName=gsub("CTR",targetCTR,"QuotaTableCTRPeriod"), row.names = FALSE, append=T) 