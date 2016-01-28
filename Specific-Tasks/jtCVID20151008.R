# Reads in the full PID Main Table. This is a straight export from the Access DB
# to excel, find and replace, with . then saved as csv.
main.table <- read.csv(file = "//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/SpecificTasks/JT_CVID_20151008/maintable.csv", 
                       header = TRUE, stringsAsFactors = FALSE)



# Filters to only keep where the category is 2,3 or 4 or (not xor) phenotype
# contains CVID, CID or ntibody. Also filters to remove any where where phenotype
# contains SCID or disease category = 1.

# This next but is the only bit that has changed.

main.table <- main.table[which((main.table$Disease.Category != 11 | is.na(main.table$Disease.Category)) 
                               & (is.na(main.table$SeqStat) == FALSE & main.table$SeqStat > 1)
                               & (grepl("VM", main.table$Sent_By) == FALSE)), ]

#main.table <- main.table[which((main.table$Disease.Category == 3 | main.table$Disease.Category == 
#                                  2 | main.table$Disease.Category == 4 | grepl("CVID", main.table$Phenotype) | 
#                                  grepl("CID", main.table$Phenotype) | grepl("ntibody", main.table$Phenotype)) & 
#                                 (grepl("SCID", main.table$Phenotype) == FALSE & (main.table$Disease.Category == 
 #                                                                                   1) == FALSE | is.na(main.table$Disease.Category == 1)) & (gsub(" ", "", main.table$Study.ID) == 
#                                                                                                                                                "") == FALSE), ]

# specifies the free text fields to loop through

myfields <- c("Pheno.Detail", "Treat_Other", "Inf_spe1", "Inf_spe2", "Patient.Significant", 
              "Patient.Malignancy", "Patient.AutoImmune", "Patient.Other")

# TEXT VARIABLE INITIALISATION

# initialises the text viariables as text
main.table$warttext <- ""
main.table$granulotext <- ""
main.table$maligtext <- ""
main.table$virustext <- ""
main.table$fungustext <- ""
main.table$autoimmunotext <- ""

# FOR LOOP

for (i in myfields) {
  # Warts
  main.table$warttext <- ifelse(grepl("wart", tolower(main.table[[i]])), paste(main.table[[i]]), 
                                paste(main.table$warttext))
  # Granulo
  main.table$granulotext <- ifelse(grepl("granulo", tolower(main.table[[i]])) | 
                                     grepl("glild", tolower(main.table[[i]])) | grepl("lip", tolower(main.table[[i]])), 
                                   paste(main.table[[i]]), paste(main.table$granulotext))
  # Malignancy
  main.table$maligtext <- ifelse((grepl("oma", tolower(main.table[[i]])) & grepl("ymptomat", 
                                                                                 tolower(main.table[[i]])) == FALSE) | (grepl("aemia", tolower(main.table[[i]])) & 
                                                                                                                          grepl("anaemia", tolower(main.table[[i]]) == FALSE)) | grepl("cancer", tolower(main.table[[i]])) | 
                                   grepl("nhl", tolower(main.table[[i]])) | grepl("odgki", tolower(main.table[[i]])), 
                                 paste(main.table[[i]]), paste(main.table$maligtext))
  # Virus
  main.table$virustext <- ifelse(grepl("ebv", tolower(main.table[[i]])) | grepl("warts", 
                                                                                tolower(main.table[[i]])) | grepl("hpv", tolower(main.table[[i]])) | grepl("papilloma", 
                                                                                                                                                           tolower(main.table[[i]])) | grepl("papiloma", tolower(main.table[[i]])) | 
                                   grepl("molusc", tolower(main.table[[i]])) | grepl("mollusc", tolower(main.table[[i]])) | 
                                   grepl("cmv", tolower(main.table[[i]])) | grepl("herpes", tolower(main.table[[i]])) | 
                                   grepl("hsv", tolower(main.table[[i]])) | grepl("vsv", tolower(main.table[[i]])) | 
                                   grepl("hhv", tolower(main.table[[i]])) | grepl("virus", tolower(main.table[[i]])) | 
                                   grepl("viral", tolower(main.table[[i]])), paste(main.table[[i]]), paste(main.table$virustext))
  # Fungus
  main.table$fungustext <- ifelse((grepl("asperg", tolower(main.table[[i]])) & 
                                     grepl("asperger", tolower(main.table[[i]])) == FALSE) | grepl("crypto", tolower(main.table[[i]])) | 
                                    grepl("pcp", tolower(main.table[[i]])) | grepl("cystis", tolower(main.table[[i]])) | 
                                    grepl("toxoplasm", tolower(main.table[[i]])) | grepl("candida", tolower(main.table[[i]])) | 
                                    grepl("muco cut", tolower(main.table[[i]])) | grepl("thrus", tolower(main.table[[i]])) | 
                                    grepl("fungus", tolower(main.table[[i]])) | grepl("fungal", tolower(main.table[[i]])), 
                                  paste(main.table[[i]]), paste(main.table$fungustext))
  # Autoimmune
  main.table$autoimmunotext <- ifelse((grepl("penia", tolower(main.table[[i]]))) & 
                                        (grepl("lymph", tolower(main.table[[i]])) == FALSE), paste(main.table[[i]]), 
                                      paste(main.table$autoimmunotext))
}

# BINARY FLAGS sets a binary flag for if warttext has anything in it
main.table$warttextbin <- ifelse(grepl("wart", tolower(main.table$warttext)), 1, 
                                 0)
# sets a binary flag for if granulotext has anything in it
main.table$granulotextbin <- ifelse(grepl("granulo", tolower(main.table$granulotext)) | 
                                      grepl("glild", tolower(main.table$granulotext)) | grepl("lip", tolower(main.table$granulotext)), 
                                    1, 0)
# sets a binary flag for if warttext has anything in it
main.table$maligtextbin <- ifelse((grepl("oma", tolower(main.table$maligtext)) & 
                                     grepl("ymptomat", tolower(main.table$maligtext)) == FALSE) | (grepl("aemia", 
                                                                                                         tolower(main.table$maligtext)) & grepl("anaemia", tolower(main.table$maligtext) == 
                                                                                                                                                  FALSE)) | grepl("cancer", tolower(main.table$maligtext)) | grepl("nhl", tolower(main.table$maligtext)) | 
                                    grepl("odgki", tolower(main.table$maligtext)), 1, 0)
# ditto virus
main.table$virustextbin <- ifelse(grepl("ebv", tolower(main.table$virustext)) | grepl("warts", 
                                                                                      tolower(main.table$viru	stext)) | grepl("hpv", tolower(main.table$virustext)) | 
                                    grepl("papilloma", tolower(main.table$virustext)) | grepl("papiloma", tolower(main.table$virustext)) | 
                                    grepl("molusc", tolower(main.table$virustext)) | grepl("mollusc", tolower(main.table$virustext)) | 
                                    grepl("cmv", tolower(main.table$virustext)) | grepl("herpes", tolower(main.table$virustext)) | 
                                    grepl("hsv", tolower(main.table$virustext)) | grepl("vsv", tolower(main.table$virustext)) | 
                                    grepl("hhv", tolower(main.table$virustext)) | grepl("virus", tolower(main.table$virustext)) | 
                                    grepl("viral", tolower(main.table$virustext)), 1, 0)
# fungus
main.table$fungustextbin <- ifelse((grepl("asperg", tolower(main.table$fungustext)) & 
                                      grepl("asperger", tolower(main.table$fungustext)) == FALSE) | grepl("crypto", 
                                                                                                          tolower(main.table$fungustext)) | grepl("pcp", tolower(main.table$fungustext)) | 
                                     grepl("cystis", tolower(main.table$fungustext)) | grepl("toxoplasm", tolower(main.table$fungustext)) | 
                                     grepl("candida", tolower(main.table$fungustext)) | grepl("muco cut", tolower(main.table$fungustext)) | 
                                     grepl("thrus", tolower(main.table$fungustext)) | grepl("fungus", tolower(main.table$fungustext)) | 
                                     grepl("fungal", tolower(main.table$fungustext)), 1, 0)
# autoimmune
main.table$autoimmunotextbin <- ifelse((grepl("penia", tolower(main.table$autoimmunotext))) & 
                                         (grepl("lymph", tolower(main.table$autoimmunotext)) == FALSE), 1, 0)
# creates a binary variable for familial

main.table$is_familial <- ifelse(gsub(" ", "", as.character(main.table$Family.ID)) == 
                                   "" | is.na(main.table$Family.ID), 0, 1)
# This bit works out whether there are at least 2 samples in the family T his
# cannibalises a lot of code from the ped file generation script, so is ugly and
# probably has superflous bits This relies on the output used to produce the ped
# file so if you think current data may not exist or be out of date run the ped
# script beforehand.

# Reads in the MS Access CSV output
family <- read.table("//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/Temp/FamSet.csv", 
                     header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Remaps input to output variables
family$"*FAMILY" <- family$Family.ID
family$PERSON <- family$Pedigree.Reference
family$FATHER <- family$Ped_father
family$FATHER[nchar(family$Ped_father) == 0] <- 0
family$MOTHER <- family$Ped_mother
family$MOTHER[nchar(family$Ped_mother) == 0] <- 0
family$GENDER <- 0
family$GENDER[family$Gender == 1] <- 1
family$GENDER[family$Gender == 2] <- 2
family$AFFECTION <- 0
family$AFFECTION[family$Is.Affected == 0] <- 1
family$AFFECTION[family$Is.Affected == 1] <- 2
family$AFFECTION[family$Is.Affected == 3] <- 1

# nulls the variables not in use in the final file
family$Index <- NULL
family$Gender <- NULL
family$Month.of.Birth <- NULL
family$Year.of.Birth <- NULL
family$Family.ID <- NULL
family$Pedigree.Reference <- NULL
family$Index.of.Father <- NULL
family$Index.of.Mother <- NULL
family$Is.Proband <- NULL
family$Familial.Phenotype <- NULL
family$Is.Deceased <- NULL
family$MZ.Twin.Index <- NULL
family$DZ.Twin.Index <- NULL
family$Consanguineous.With.Index <- NULL
family$Is.Affected <- NULL
family$Sample.Exists <- NULL
family$MZ.Twin.Set <- NULL
family$DZ.Twin.Set <- NULL
family$Phenotype <- NULL
family$Study.ID <- NULL
family$Ped_father <- NULL
family$Ped_mother <- NULL

familysort <- family[order(family$"*FAMILY", family$PERSON), ]

# copies the familysort dataframe, enabling the original to be easily reverted to
familyjunk <- familysort

# CHANGE THIS FROM HAS DATA TO HAS SAMPLE IE SEQSTAT >1 sets up the sequenca dta
# variable 0, if sequence data has been received, sets to 1.
familyjunk$hasseq <- 0
familyjunk$hasseq[is.na(familyjunk$SeqStat)] <- 0
familyjunk$hasseq[familyjunk$SeqStat > 1] <- 1

# creates a dataframe containing only family ID and hasseq variable
famvar <- c("*FAMILY", "hasseq")
familyjunk <- familyjunk[famvar]

# aggregate sums the hasseq variables by family, then only entries for which x ==
# 1 are kept.
aggdata2 <- subset((aggregate(familyjunk$hasseq, list(family = familyjunk$"*FAMILY"), 
                              sum)), x > 0)

names(aggdata2)[names(aggdata2) == "x"] <- "familysamples"
aggdata2$infile <- 0
aggdata2$infile[aggdata2$familysamples > 1] <- 1

main.table2 <- merge(main.table, aggdata2, by.x = "Family.ID", by.y = "family", all.x = TRUE, 
                     all.y = FALSE)

# reads in the full PID Main Table. This is a straight export from the Access DB
# to excel, find and replace, with . then saved as csv.
lookup <- read.csv(file = "//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/SpecificTasks/JT_CVID_20151008/Lookup2.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

main.table3 <- merge(main.table2, lookup, by.x = "Study.ID", by.y = "Patient.ID", 
                     all.x = TRUE, all.y = FALSE)



# Selects variables to keep

keepvars <- c("Study.ID","Exome.ID", "WGS.ID", "Illumina.ID", "Phenotype", "Pheno.Detail", "Disease.Category", 
              "Gender", "SeqStat", "is_familial", "Family.ID", "familysamples", "infile", "warttext", 
              "warttextbin", "granulotext", "granulotextbin", "maligtext", "maligtextbin", 
              "virustext", "virustextbin", "fungustext", "fungustextbin", "autoimmunotext", 
              "autoimmunotextbin", "Lab_IgG", "Lab_IgA", "Lab_IgM", "Lab_IgE", "Lab_CD4", "Lab_CD8", 
              "Lab_NK", "Lab_B", "AIF_Hashi", "AIF_Heman", "AIF_Neutr", "AIF_Thromb", "AIF_SLE", 
              "AIF_ANA", "AIF_Rheumf", "AIF_ANCA", "AIF_SMA", "AIF_APL", "Inf_vir", "Inf_bact", 
              "Inf_fung", "AB_Neo", "AB_Neo_Lymph", "AB_Neo_Leuk", "AB_Neo_Solid", "AB_Neo_Ecto", 
              "AB_Neo_Endo", "AB_Neo_Meso")

main.table3$Exome.ID[is.na(main.table3$Exome.ID)] <- ""
main.table3$WGS.ID[is.na(main.table3$WGS.ID)] <- ""
main.table3$Illumina.ID[is.na(main.table3$Illumina.ID)] <- ""

MT1 <- main.table3[keepvars]
MT1$familysamples <- ifelse(is.na(MT1$familysamples), 0, MT1$familysamples)

MT1$infile[is.na(MT1$infile)] <- 0

write.csv(MT1, file = "//Me-filer1/groups3$/Haematology/WO Group/BioResource for Rare Diseases/NIHR BR - Rare Diseases/PID/SpecificTasks/JT_CVID_20151008/JT_CVID_20151008_V2.csv")
