#This R script creates a CSV file with a list of samples in the database that are down as being in AC but either passed or failed on 4R.
#It will aslso pick up samples that aren't known ont he database if they have passed or failed.

#in order for this to work, copy into a text file everything from 4R report 16, both passes and fails. Only the first one that you do needs to keep the header.
#copy across on a USB stick, save as /WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Reports/4RUpdate/Update.txt

#if you are having issues and want to disable the memory clean up for debugging, set this to 1
debug4rupdate <- 0

#reads in the input from 4R, tab delimited.
recd <- read.table("//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Reports/4RUpdate/Update.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

#gets rid of any spaces, changes the name to something more usable
recd$Status <- gsub(" ", "", recd$Samples..sampleid.qc.final.dilution.)
recd$Samples..sampleid.qc.final.dilution. <- NULL
#gets rid of spaces, creates a new variable with the checksum missing as there have been errors in 4R wuch as the asterisk character not displaying
recd$Subject.ID<- gsub(" ", "", recd$Subject.ID)
recd$Study.ID<- substr(recd$Subject.ID, 1, 9)
#Creates a new status update variable, initialised as blank text, if the string contains Pass, calls it a psss, otherwise fail.
recd$Statup <- ""
recd$Statup[grepl("Pass", recd$Status)] <- "Pass"
recd$Statup[grepl("Pass", recd$Status) == FALSE]  <- "Fail"

#reads in the output from the access db, every non adult-onset vasculitis patient 
sentnotrecd <- read.table("//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Reports/4RUpdate/SeqStatCheck.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
#gets rid of spaces and chops off the final letter so that it wil merge successfully with the variable created earlier
sentnotrecd$Study.ID<- gsub(" ", "", sentnotrecd$Study.ID)
sentnotrecd$Study.ID<- substr(sentnotrecd$Study.ID, 1, 9)

#merges the two data frames by the created Study ID variable.
merge1 <- merge(recd, sentnotrecd,
                by="Study.ID",
                all.x = TRUE, all.y = FALSE
  )

if (debug4rupdate != 1){
  #removes the data sets from memory
  rm(recd, sentnotrecd)
}

merge1$SeqStat[is.na(merge1$SeqStat)] <- 2
toupdate <- merge1[ which(merge1$SeqStat < 4),]
toupdate <- toupdate[,c(3,6,5)]

if (debug4rupdate != 1){
  #removes the previous merged data set from the 
  rm(merge1)
}

#writes the merged file to csv
write.csv(toupdate, file=paste("//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Reports/4RUpdate/ToUpdate", (as.character(Sys.Date(), format="%Y%m%d")), ".csv"), row.names=FALSE)

if (debug4rupdate != 1){
  #removes toupdate from the memory
  rm(toupdate)
}

#removes the debug state variable (not toggleable, so the debug state has to be stated at the beginning of each run)
rm(debug4rupdate)
