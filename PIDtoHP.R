#Script to convert PID Database output to a version readable by HaploPainter

#If cleanup = 1, removes the source file when the script finishes. Toggle this to something else if you want to debug.
cleanup <- 1

family <- read.table("//ME-FILER1/GROUPS3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Temp/Family.csv", header=TRUE, sep=",")

#If ProbName == 1, sets the output file name to the same as the proband. If else, sets to PIDPedigree
ProbName <- 1

#WORK OUT WHETHER THE THING NEEDS UPDATING

family$Ped_father <- as.character(family$Ped_father)
family$Ped_mother <- as.character(family$Ped_mother)
family$Local_ref <- as.character(family$Local_ref)
family$Local_ref[is.na(family$Local_ref)] <- ""

#this bit tests to see if the pedigree information is in 'old format' - relative to whole database rather than within pedigree
#if it is in 'old format', this is mapped across to the standard used in .ped files.

if ((is.na(family$Ped_mother) == FALSE && is.na(family$Ped_father) == FALSE) == FALSE) {
  #as the test for needing to change father and mother has passed, sets them to 0
  family3$FATHER <- "0"
  family3$MOTHER <- "0"
  
  #Sets up the variables to keep and pipes to a new dataframe
  iprvar <- c("Index", "Pedigree.Reference")
  iprvars <- family[iprvar]
  
  #renames the father index variable to avoid conflicts when merging
  names(iprvars)[names(iprvars) == "Pedigree.Reference"] <- "iprvars"
  
  #creates a new dataframe, merging by index of father and index, junking anyone from the smaller dataset who is not someone's father
  family2 <- merge(family,iprvars,
                   by.x = "Index.of.Father", by.y = "Index",
                   all.x = TRUE, all.y = FALSE)
  
  #renames the mother index variable to avoid conflicts when merging
  names(iprvars)[names(iprvars) == "iprvars"] <- "iprvars2"
  
  #creates a new dataframe, merging by index of mother and index, junking anyone from the smaller dataset who is not someone's mother
  family3 <- merge(family2,iprvars,
                   by.x = "Index.of.Mother", by.y = "Index",
                   all.x = TRUE, all.y = FALSE)
  
  family3$iprvars <- as.character(family3$iprvars)
  family3$iprvars2 <- as.character(family3$iprvars2)
  
  family3$FATHER <- "0"
  family3$MOTHER <- "0"
  
  #if parent value is 0 and the merged parent value is not null, conditionally overwrites the parent value
  family3$FATHER <- ifelse((is.na(family3$iprvars) == FALSE),family3$iprvars,"0")  
  family3$MOTHER <- ifelse((is.na(family3$iprvars2) == FALSE),family3$iprvars2,"0")  
  
  #renames the dataframe so that the script will run for any value of RefCorr
  family <- family3
  
  #nulls dataframes and values not in use
  family2 <- NULL
  family3 <- NULL
  family$iprvars <- NULL
  family$iprvars2 <- NULL
} else {
  #copies parental data across then if the field is empty, sets it to zero
  #note - these will not be null no no is.na checking is needed. 
  family$FATHER <- family$Ped_father
  family$FATHER[nchar(family$Ped_father) == 0] <- "0"
  family$MOTHER <- family$Ped_mother
  family$MOTHER[nchar(family$Ped_mother) == 0] <- "0"
}

#New Variable[condition]				<-	Old Variable
family$"*FAMILY"						<-	family$Family.ID
family$PERSON						<- 	family$Pedigree.Reference
family$GENDER						<- 	family$Gender
family$Severity <- as.numeric(family$Severity)

#sets the colours for Haplopainter. More colours can be found on the website.
family$AFFECTION[family$Is.Affected == 3] <- 0 #blue
family$AFFECTION[family$Is.Affected == 1] <- 2 #black
family$AFFECTION[family$Is.Affected == 0] <- 1 #clear
family$AFFECTION[family$Is.Affected == 2] <- 6 #grey

family$"IS_DECEASED"					<-	family$Is.Deceased
family$"IS_DECEASED"[family$Is.Deceased == "0"]	<-	""
family$"IS_DECEASED"[is.na(family$Is.Deceased)]  <-	""
family$"IS_SAB_OR_TOP"					<-	""
family$"IS_PROBAND"					<-	family$Is.Proband
family$"IS_PROBAND"[family$Is.Proband == "0"]	<-	""
family$"IS_PROBAND"[is.na(family$Is.Proband)] <- ""
family$"IS_ADOPTED"					<-	""
family$DZ.Twin.Set[is.na(family$DZ.Twin.Set)]	<- 	""
family$MZ.Twin.Set[is.na(family$MZ.Twin.Set)]	<- 	""
family$"ARE_TWINS"					<-	paste(family$DZ.Twin.Set, family$MZ.Twin.Set, sep = "") 
family$Consanguineous.With.Index[is.na(family$Consanguineous.With.Index)]	<-	""
family$"ARE_CONSANGUINEOUS"				<-	family$Consanguineous.With.Index
family$"INNER_SYMBOL_TEXT"				<-	""

family$"SIDE_SYMBOL_TEXT"	[family$Personal_details == 3 & family$Front_sheet ==3]			<-	"IP"
family$"SIDE_SYMBOL_TEXT" [(family$Personal_details != 3 | is.na(family$Personal_details)) & (family$Front_sheet != 3 | is.na(family$Front_sheet))]			<-	""
family$"SIDE_SYMBOL_TEXT" [(family$Personal_details != 3 | is.na(family$Personal_details)) & family$Front_sheet ==3] <- "P"
family$"SIDE_SYMBOL_TEXT" [family$Personal_details == 3 & (family$Front_sheet != 3 | is.na(family$Front_sheet))]  		<-	"I"



family$LOWER_SYMBOL_TEXT1				<-	paste("Onset: ", as.character(family$Age.at.Presentation))
#
family$LOWER_SYMBOL_TEXT1[family$Severity == 0] <- "Onset: NA"
family$LOWER_SYMBOL_TEXT1[(family$Severity > 0 | is.na(family$Severity)) & is.na(family$Age.at.Presentation)] <- "Onset: U/NA"
family$LOWER_SYMBOL_TEXT1[family$AFFECTION == 1] <- "Onset: NA"
family$LOWER_SYMBOL_TEXT1[(family$AFFECTION == 2 | family$AFFECTION == 0) & is.na(family$Age.at.Presentation) ] <- "Onset: U"
#family$LOWER_SYMBOL_TEXT1[is.na(family$Local_ref) == FALSE] 			<-	as.character(family$Local_ref)
#family$LOWER_SYMBOL_TEXT1[gsub(" ","",as.character(family$Local_ref)) =="" | is.na(family$Local_ref)] <- ""


# family$Year.of.Birth[is.na(family$Year.of.Birth)] <- 2100
# family$Month.of.Birth[is.na(family$Month.of.Birth)] <- 6
family$age <- 999
family$age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(family$Year.of.Birth)
family$age[is.na(family$age)] <- 999
# family$Month.of.Birth[is.na(family$Month.of.Birth)] <- as.numeric(format(Sys.Date(), "%m"))
# family$age[(as.numeric(format(Sys.Date(), "%m")) - as.numeric(family$Month.of.Birth)) > 0] <- family$age - 1 

family$ageString = "U"
family$ageString <- as.character(family$age)
family$ageString[family$ageString == "999" | family$ageString == "998"] <- "U"

family$LOWER_SYMBOL_TEXT1 <- paste("Age: ", family$ageString , ", ", family$LOWER_SYMBOL_TEXT1, sep="")


family$"LOWER_SYMBOL_TEXT2"      <- " "
family$"LOWER_SYMBOL_TEXT2"[family$SeqStat == 1]        <-  "Awaiting Sample"
family$"LOWER_SYMBOL_TEXT2"[family$SeqStat == 2 | family$SeqStat == 3]        <-  "Sample Received"
family$"LOWER_SYMBOL_TEXT2"[family$SeqStat == 4]        <-  "Passed QC. Not sent"
family$"LOWER_SYMBOL_TEXT2"[family$SeqStat == 5]        <-  "Sent for sequencing"
family$"LOWER_SYMBOL_TEXT2"[family$SeqStat == 6]        <-  "Sequence data received"
family$"LOWER_SYMBOL_TEXT2"[family$SeqStat == 9]        <-  "Sample failed QC"
family$"LOWER_SYMBOL_TEXT2"[is.na(family$SeqStat)]        <-  "No Sample"

family$"LOWER_SYMBOL_TEXT3"			<-	" "
family$"LOWER_SYMBOL_TEXT3"[is.na(family$Severity)]  		<-	"Severity: Unknown"
family$"LOWER_SYMBOL_TEXT3"[family$Severity == 1]      <-	"Severity: Mild"
family$"LOWER_SYMBOL_TEXT3"[family$Severity == 2]      <-	"Severity: Severe"
family$"LOWER_SYMBOL_TEXT3"[family$Severity == 0]       <-  "Severity: Unaffected"

family$"LOWER_SYMBOL_TEXT4"<-  as.character(family$Phenotype)
family$"LOWER_SYMBOL_TEXT4"[is.na(as.character(family$Phenotype))]<-   ""


# CONSANG UPDATE BIT. IF CONSANG, STICKS CONSANG IN THE FAMILY NAME FOR THE TITLE.8

family$"Parent.Consang"[is.na(family$"Parent.Consang")] <- 0

aggdata2 <- subset((aggregate(family$"Parent.Consang", 
                              list(family=family$"*FAMILY"), sum)), x > 0)

if (nrow(aggdata2[ which(aggdata2$x > 0), ]) > 0) {
  family$"*FAMILY" <-paste(family$"*FAMILY", "CONSANG", sep="-")
}
#option to auto-name 

if (ProbName == 1) {
  
  #perhaps stick a check in here that someone within the pedigree is defined as the proband.
  #if noone is, check for a match to the family ID or set as family id?
  #alternatively just default to the option if ProbName != 1?
  
  #creates a data frame with only entried where the person is defined as proband, this should only be 1 per pedigree.
  iprvar <- c("PERSON", "IS_PROBAND")
  iprvars <- subset(family[iprvar], IS_PROBAND == 1)
  
  #puts the entry into a variable
  if (iprvars$"IS_PROBAND" == 1) {
	ProbNames <- iprvars$PERSON
  } else {
	ProbNames <- c("PIDPedigree")
	}
}	


#Nulls input variables

family$Family.ID<-NULL
family$Index<-NULL
family$Index.of.Father<-NULL
family$Index.of.Mother<-NULL
family$Ped_father <- NULL
family$Ped_mother <- NULL
family$Gender<-NULL
family$Is.Affected<-NULL
family$Is.Deceased<-NULL
family$Is.Proband<-NULL
family$MZ.Twin.Set<-NULL
family$DZ.Twin.Set<-NULL
family$MZ.Twin.Index<-NULL
family$DZ.Twin.Index<-NULL
family$Month.of.Birth<-NULL
family$Year.of.Birth<-NULL
family$Pedigree.Reference<-NULL
family$Familial.Phenotype<-NULL
family$Sample.Exists<-NULL
family$Consanguineous.With.Index<-NULL
family$Phenotype<-NULL
family$Study.ID<-NULL
family$SeqStat<-NULL
family$Severity<-NULL
family$Front_sheet <- NULL
family$Personal_details <- NULL
family$Local_ref <- NULL
family$Age.at.Presentation <- NULL
family$Parent.Consang <- NULL
family$age <- NULL
family$ageString <- NULL

#reorders variables to be compatible with HaploPainter
family <- family[,c(3,4,1,2,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]

#export step

if (exists("ProbNames")) {
  
  #Removes the asterisk
  ProbNames <- gsub("[/*]","",as.character(ProbNames))
  ProbNames <- gsub("[/:]","",as.character(ProbNames))
  
  #Writes output as tab delimited
  write.table(family, file=paste("//ME-FILER1/GROUPS3$/hAEMATOLOGY/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/OutpuT/", ProbNames ,".txt", sep=""),
              quote = FALSE, row.names = F, sep="\t")
  print(ProbNames)
  
}else{
  
  #If no proband has been defined or guessed

  write.table(family, file=paste("//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Output/NoProb.txt", sep=""),
              quote = FALSE, row.names = F, sep="\t")
  print("No proband defined or guessed, saved as NoProb.txt")
}

if (cleanup == 1) {
	file.remove("//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Temp/Family.csv")
}
