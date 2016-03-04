# R script to automatically append pedigree diagrams to the end of any pdfs that 
# mention them that are placed in a folder.

#gets a list of the full path names for all pdf files in the input folder
pdflist <- file.info(list.files(path="/var/www/bpd/latest/PED/PDF/pdftktest/input/", 
                                all.files=TRUE,recursive=FALSE,include.dirs=FALSE,
                                pattern=".pdf",full.names=TRUE))
pdfiles  <- rownames(pdflist)
pdflists <- as.data.frame(pdfiles)
#creates a seperate name variable containing only the filenames, no paths
pdflists$name <- gsub("/var/www/bpd/latest/PED/PDF/pdftktest/input//", "", 
                      pdflists$pdfiles)
#files the paths to make them useable
pdflists$pdfiles <- gsub("//", "/", pdflists$pdfiles)
#for loop which carries out the process on every file
for (i in (nrow(pdflists))) {
  # takes IDs from pdfs, converts to family IDs by comparing to most recent ped.
  # uses family ID to grab relevant diagram pdfs.
  
  # sets path variable, name variable and reduced name variable using the nth 
  # row of the pdf list  
  path <- pdflists$pdfiles[i]
  name22 <- pdflists$name[i]
  name22redux <-gsub(".pdf", "",pdflists$name[i])
  
  # NOTE: requires pdftotext so if you move system etc, might fail.
  # takes all txt from page 1 of pdf, saves to temp
  system(paste("pdftotext -l 1 /var/www/bpd/latest/PED/PDF/pdftktest/input/", 
               name22, " /var/www/bpd/latest/PED/PDF/pdftktest/temp/",name22redux,
               ".txt", sep="")
         # sets the file name to the txt file just created
         fileName <- paste("/var/www/bpd/latest/PED/PDF/pdftktest/temp/",
                           name22redux,".txt", sep="")
         # reads in the txt file created by pdftotext
         x <- readChar(fileName, file.info(fileName)$size)
         # removes breaks and new lines
         x <- gsub("\\\n", "",x)
         x <- gsub("\\\r", " ",x)
         # adds a space after each comma
         x <- gsub(",",", ", x)
         # splits the string into a vector, splitting by space
         x <- unlist(strsplit(x, split=" "))
         # keeps the first hundred words
         x <-head(x,n=100)
         # keeps everything before controls and after the final cases.
         x <- head(x,n=(grep("Controls", x) -1))
         x <- tail(x,n=(0-(max(grep("Cases", x)))))
         # removes anythign containing a comma
         x <- subset(x, grepl(",",x)==FALSE)
         # removes anything containing parenthesis
         x <- subset(x, grepl("\\(", x)==FALSE)
         # removes any empty vector elements
         inpdf <- x[ x != "" ]
         # converts to a data frame
         y <- as.data.frame(inpdf, stringsAsFactors = FALSE)
         # adds a flag that the entry needs to be converted from individualid to familyid
         y$convertme <- 1
         
         # grabs the names of all the .ped files in the PED directory and lists
         # them in descending date order.
         pedlist <- file.info(list.files(path="/var/www/bpd/latest/PED/",
                                         all.files=TRUE,recursive=FALSE,
                                         include.dirs=FALSE,pattern=".ped",
                                         full.names=TRUE))
         
         pedlist = pedlist[with(pedlist, order(as.POSIXct(mtime), 
                                               decreasing=TRUE)), ]
         
         peds = gsub("/var/www/bpd/latest/PED//", "", rownames(pedlist))
         
         #reads int he most recent ped files
         ped1 <- subset(read.delim(file=paste("/var/www/bpd/latest/PED/",
                                              peds[1],sep=""), 
                                   header = FALSE, sep = "\t", 
                                   stringsAsFactors=FALSE), select = c(1,2))
         #adds a flag
         ped1$inped <- 1
         
         #merges the imported ped file with the processed pedigree text dataframe
         z <- merge(ped1, y,
                    by.x = "V2", by.y = "inpdf",
                    all = TRUE)
         
         #keeps all which are to be converted
         z <- z[ which(is.na(z$convertme)==FALSE), ]
         #creates a new data frame with only ones which can be
         z1 <- z[ which(is.na(z$inped)==FALSE), ]
         
         #inserts a .pdf in every space and at the end so that every file name is present
         zz <- paste(gsub(" ", ".pdf ",paste(z1$V1, collapse = ' ')), ".pdf", sep="")
         
         # reads the names of all the files in the PDF directory, 
         # puts into a dataframe starting with full paths.
         file <- NULL
         files <- gsub("^.*?/","",list.files(path="/var/www/bpd/latest/PED/PDF",
                                             all.files=TRUE,recursive=TRUE,
                                             include.dirs=FALSE,pattern=".pdf",
                                             full.names=TRUE))
         
         file <- as.data.frame(files,
                               stringsAsFactors = FALSE)
         # Creates a new variable with just the name of the pdf
         file$names <- gsub("^.*?/","",list.files(path="/var/www/bpd/latest/PED/PDF",
                                                  all.files=TRUE,recursive=TRUE,
                                                  include.dirs=FALSE, pattern=".pdf",
                                                  full.names=FALSE))
         
         #splits apart the string of pdfs that need to be appended and adds a flag
         pdf <- unlist(strsplit(zz, split=" "))
         zx <- as.data.frame(pdf,
                             stringsAsFactors = FALSE)
         zx$pickme <- 1
         
         #merges the list of files that are needed with the list of files available
         list1 <- merge(zx, file,
                        by.x = "pdf", by.y = "names",
                        all = TRUE)
         
         #keeps those on the list that need to be appended
         list2 <- list1[ which(list1$pickme == 1), ]
         #removes any duplicates in place from testing
         list2 <- list2[ which(grepl("pdftk",list2$files)==FALSE), ]
         
         #collapses the list onto a string againn
         list3 <- paste(list2$files, collapse = ' ')
         #adds a slash to var so that it is recognised by the command line
         list3 <- gsub("var", "/var", list3)
         #creates the pdftk command
         list4 <- paste("pdftk /var/www/bpd/20140611/PED/PDF/pdftktest/input/",
                        name22," ", list3, 
                        " cat output /var/www/bpd/latest/PED/PDF/pdftktest/output/",
                        name22redux,"_PedApp.pdf",sep="")
         #finally pipes the pdftk command to the command line
         system(list4)
         
}
