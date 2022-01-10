# This code was made by an NIH researcher and taken from somewhere on the Internet.
#load necessary libraries
library(tidyverse)
library(stringr)
library(foreign)
library(survey)
library(RCurl)

#set the temporary directory to download all files to!
setwd("c:/users/atala/documents/NHIS")

#main FTP directory with data sets..
nhis_file_ftp <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/"

#choose which years to download
which_years_to_download <- 2016:2018

for ( year in which_years_to_download ){
  
  #create the full string to the FTP folder of the current year
  year_ftp_dir <- paste( nhis_file_ftp , year , "/" , sep="" )
  
  #figure out what all of the files within that folder are named
  filenames <- getURL( year_ftp_dir , dirlistonly=T )
  filenames <- strsplit(filenames, "\r*\n") %>% flatten_chr() %>% str_subset("samadult")
  
  #if a file.zip and a file.exe both exist, only take the file.exe!
  exe_filenames <- filenames[ grepl( ".exe" , filenames ) ]
  zip_filenames <- filenames[ grepl( ".zip" , filenames ) ]
  exe_filenames <- gsub( ".exe" , "" , exe_filenames )
  zip_filenames <- gsub( ".zip" , "" , zip_filenames )
  exe_and_zip_filenames <- zip_filenames[ (zip_filenames %in% exe_filenames) ]
  zip_filenames_with_exe_matches <- paste( exe_and_zip_filenames , ".zip" , sep = "" )
  filenames <- filenames[ ! (filenames %in% zip_filenames_with_exe_matches) ]
  
  #throw out readme.txt!
  filenames <- filenames[ ! ( filenames %in% "readme.txt" ) ]
  
  #throw out folders (files without a . in them)
  filenames <- filenames[ grepl( "\\." , filenames ) ]
  
  #MANUAL RECODE FOR A SUBSET OF FILES
  #to only run a subset of files within a particular year, change the filenames line here!
  #filenames <- c("samadult.exe")
  #END MANUAL RECODE FOR A SUBSET OF FILES
  
  #manual recode to throw out a few files!
  
  #skip 1988 mdevices file!
  if ( year == 1988 ) filenames <- filenames[ ! ( filenames %in% "mdevices.exe" ) ]
  
  #skip 1994 and 1995 dfs files!
  if ( year %in% c(1994,1995) ) filenames <- filenames[ ! ( filenames %in% c("dfschild.exe","dfsadult.exe") ) ]
  
  #skip the 1992 cancepid and all of the nursing home files!
  if ( year == 1992 ) filenames <- filenames[ ! ( filenames %in% c("conditnh.exe","cancepid.exe","drvisinh.exe","hospitnh.exe","househnh.exe","personnh.exe") ) ]
  
  #skip the 2007 alternative medicine and injury verbatim files!
  if ( year == 2007 ) filenames <- filenames[ ! ( filenames %in% c("althealt.exe","injverbt.exe") ) ]
  
  #skip the 1999 and 2000 injury verbatim file!
  if ( year %in% c(1998:2000,2008,2009) ) filenames <- filenames[ ! ( filenames %in% "injverbt.exe" ) ]
  
  #manual recode for 2004 - since everything is in its own directory,
  #just go straight to the file!
  if ( year == 2004 ){
    filenames <- c(
      "./familyfile/familyxx.exe",
      "./household/househld.exe",
      "./InjuryPoison/injpoiep.exe",
      #note that this skips the injury verbatim file!
      #"./InjuryVerbatim/injverbt.exe",
      "./person/personsx.exe",
      "./sampleadult/samadult.exe",
      "./samplechild/samchild.exe" )
  }
  
  #end of manual recodes!
  
  for ( filename in filenames ){
    
    #print the current year and file name
    print( "currently working on..")
    print( year )
    print( "file..")
    print( filename )
    
    #create a temporary file..
    tf <- tempfile()
    #create a temporary directory..
    td <- tempdir()
    
    #full file location
    full_file_location <- paste( year_ftp_dir , filename , sep="" )
    
    file_download <- download.file( 
      full_file_location , 
      tf , 
      mode = "wb" 
    ) 
    
    
    #now there's a zipped file in the temporary directory.. unzip it
    #this command does two things-- it unzips the ZIP file
    #and it stores a vector of character strings in the file name (fn) variable, so we now know where the unzipped files landed.
    fn <- unzip( tf , exdir = td , overwrite = T )
    
    #find the filename before the .
    fileval <- substr( filename , 1 , regexpr("\\.",filename) - 1 )
    
    #path to the SAS read-in line
    sas_ri <- paste( "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/" , year , "/" , fileval , ".sas" , sep = "" )
    
    #manual recode for 2004
    if (year == 2004){
      fileval <- substr( filename , gregexpr("\\/",filename)[[1]][2] + 1 , gregexpr("\\.",filename)[[1]][2] - 1)
      
      sas_ri <- paste( "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/" , year , "/" , substr( filename , 1 , gregexpr("\\.",filename)[[1]][2] - 1 ) , ".sas" , sep = "" )
    }	
    #end manual recode for 2004
    
    
    #############now download the SAS import instructions from the FTP site
    #first store the FTP path to the SAS instructions in sas_ri
    
    #wait ten seconds so as not to overwhelm the connection
    Sys.sleep( 10 )
    
    ##and now actually pull the entire file into R from the FTP site, line-by-line
    SASinput <- readLines( sas_ri )
    
    ##find the first line with the word INPUT in it, which is where the ASCII variable locations occur.
    
    #lines that start with input
    ip <- grep("INPUT",toupper(SASinput))
    #lines that aren't commented out
    nc <- which( ! grepl( "\\*" , SASinput ) )
    
    
    firstline <- min( intersect( ip , nc ) )
    
    ##find the first semicolon ending that input line
    a<-grep(";",toupper(SASinput))
    lastline<-min(a[a>firstline])
    
    
    #manual recode to account for ending semicolon on last input line instead of its own line
    
    if ( year == 1999 & fileval %in% "injverbt" ) lastline <- lastline + 1
    if ( year == 2007 & fileval %in% "ratcat07" ) lastline <- lastline + 1
    
    #end of manual recodes!
    
    
    ##isolate the Fixed-Width File (FWF) input lines, throwing out the first line (which is just INPUT) and the last line (which is just the semicolon)
    FWFlines<-SASinput[(firstline+1):(lastline-1)]
    
    #throw out all lines that have been commented out
    FWFlines <- FWFlines[ !grepl( "\\*" , FWFlines ) ]
    
    #throw out all lines that are empty
    FWFlines <- FWFlines[ which( FWFlines != "" ) ]
    
    #manual recode
    
    if (year == 1994 & fileval == "personsx") FWFlines <- gsub("-"," - " , FWFlines)
    
    #end manual recode
    
    ##break apart all FWF lines
    z<-strsplit(FWFlines," ",perl=T)
    
    ##create FWF structure file (x)
    x<-NULL
    
    
    #manually fix problems with the SAS input files
    
    #the 1968 conditionf file should not have QUESTS2 QUESTS3 in the SAS input line!
    if ( year == 1968 & fileval == "conditionf") z <- z[-41]
    
    #the 1974 healthin file has WTBDD2W and WTBDD2WB in the wrong order
    if ( year == 1974 & fileval == "healthin") z <- c( z[1:50] , z[56] , z[ 51:55 ] , z[ 57:83 ] )
    
    #end manual fixes
    
    
    #now cycle through every line of the FWFlines..
    for ( j in 1:length(z) ){
      
      #make a new variable with just one line
      y<-z[[j]]
      
      #get rid of dashes!
      y <- gsub( "-" , "" , y )
      
      #throw out empty strings
      y<-y[! y %in% ""]
      
      
      #if there's anything in the current FWF line and it's not commented out..
      if ( length(y) > 0 & ! ("\\*" %in% z[[j]] )){
        
        #create a SKIP variable to 'exit' the loop..
        skip2 <- 0
        
        #loop through all the different words in the specific FWF line..
        for ( k in 1:length(y) ){
          #..so long as the SKIP hasn't been triggered
          if ( skip2 < k ) {
            
            #if the SAS line is a character variable starting AT a location in the file..
            if ( grepl( "\\@" , y[k] ) ){
              
              #get rid of the at sign
              pm <- sub( "\\@" , "" , y[k] )
              
              #specify this variable to be a CHARACTER type ($s are character input formats in SAS)
              fm <-  "$"
              
              #create a one-line table with the variable name, the variable's placement in the file, and character format
              df <- data.frame( varname = y[k+1] , placement = as.numeric(pm) , format = "$" , divisor = 1 )
              
              #add the one-line table to the full FWF structure file
              x<-rbind(x,df)
              
              #and skip the next two words in the line (which are the variable name and the placement location.
              skip2 <- k+2
              
            } else {
              
              #if the current word isn't a number AND it's not a symbol, then assume it's the variable's name
              if ( is.na( as.numeric( y[k]) ) & ! (y[k] %in% c("-","$") ) ) {
                
                #set the default format to NUMERIC
                fm <- ""
                
                #take the word directly after the variable's name as the placement location
                pm <- y[k+1]
                
                #set the default divisor to one (assumes the numeric value doesn't need to be divided)
                div <- 1
                
                #if there's at least one word after the variable name, check if it's a $, indicating a character instead of numeric format..
                if ( length( y ) >= k + 1 ){
                  
                  #and if it is, set the format to character and look for the location-placement number in the 2nd word after the variable
                  if ( y[ k + 1 ] == "$" ) {
                    fm <- "$"
                    pm <- y[k+2]
                  }
                }
                
                #if there's more than two words after the variable name..
                if ( length( y ) >= k + 3 ){
                  #check if the third word is a divisor
                  if( !is.na( as.numeric( y[ k + 3] ) ) ) {
                    
                    if ( as.numeric( y[ k + 3 ] ) < 1 ) {
                      
                      #and if it is, add it to the FWF structure file!
                      div = y[ k + 3 ]
                      
                    }
                  }
                }
                
                #based on all of these options, create a one-row data frame..
                df <- data.frame( varname = y[k] , placement = as.numeric(pm) , format = fm , divisor = div )
                
                #and add it to the FWF structure file!
                x<-rbind(x,df)
                
              }
            }
            
            #save the final numeric value in the SASinput file so you know the length of the last field in the FWF
            if ( !is.na( as.numeric( y[k] ) ) & as.numeric( y[k] ) >= 1  ){
              last_numeric <- as.numeric( y[k] )
            }
            
          }
        }
      }
    }
    
    
    #cycle through the entire FWF structure file and subtract each placement from the previous placement, in order to calculate each variable's width (number of characters)
    for ( j in 1:(nrow(x)-1) ){
      x[ j , "width" ] <- x[ j + 1 , "placement" ] - x[ j , "placement" ]
    }
    #..and for the last field, use the last_numeric field (stored from above)
    x[ j + 1 , "width" ] <- last_numeric - x[ j + 1 , "placement" ] + 1
    
    ##determine exact width of each variable
    #x$width<-as.numeric(substr(x$format , ifelse(grepl("\\$",x$format),2,1) , regexpr("\\.",x$format)-1))
    
    ##determine number of decimals each variable should have
    #x$decimals<-as.numeric(substr(x$format , regexpr("\\.",x$format)+1,length(x$format)))
    
    ##determine whether character or string
    x$charvar<-grepl("\\$",x$format)
    
    #if the FIRST placement of the FIRST variable is NOT 1, then add a blank variable so the file reads in properly
    if ( x[ 1 , "placement" ] != 1 ) {
      df <- data.frame( varname = "TOSS" , width = (x[ 1 , "placement" ] - 1) , placement = 1 , format = "" , divisor = 1 , charvar = F )
      x <- rbind( df , x )
    }
    
    
    ##################################################################################################
    #now that we've got an appropriate FWF structure file, we can actually read the ASCII data into R!
    ##################################################################################################
    
    
    
    
    #manually fix problems with the SAS input files
    
    #the 1975 personsx file should have a smaller PER100 field in the SAS input line!
    if ( year == 1975 & fileval == "personsx") {
      x[ 58 , "width" ] <- 2
      df <- data.frame( varname = "TOSS" , width = 4 , placement = 102 , format = "" , divisor = 1 , charvar = F )
      x <- rbind( x[ 1:58 , ] , df , x[ 59:nrow(x) , ] )
    }
    
    #the 1993 immunize file should have a smaller RESPOND field in the SAS input line!
    if ( year == 1993 & fileval == "immunize") {
      x[ 49 , "width" ] <- 1
      df1 <- data.frame( varname = "TOSS" , width = 6 , placement = 92 , format = "" , divisor = 1 , charvar = F )
      
      x[ 80 , "width" ] <- 6
      df2 <- data.frame( varname = "TOSS" , width = 123 , placement = 213 , format = "" , divisor = 1 , charvar = F )
      
      x <- rbind( x[ 1:49 , ] , df1 , x[ 50:80 , ] , df2 , x[ 81:nrow(x) , ] )
      
      x[ x$varname %in% "MEAS1MO" , "width" ] <- 2
      x[ x$varname %in% "MEAS1MO" , "placement" ] <- 436
      x[ x$varname %in% "MEASMMR1" , "width" ] <- 1
      
      
    }
    
    #the 1993 familyres file should have a smaller F_172 field in the SAS input line!
    if ( year == 1993 & fileval == "famlyres") {
      x[ 3 , "width" ] <- 12
      df1 <- data.frame( varname = "TOSS" , width = 156 , placement = 17 , format = "" , divisor = 1 , charvar = F )
      x[ 4 , "width" ] <- 6
      df2 <- data.frame( varname = "TOSS" , width = 11 , placement = 178 , format = "" , divisor = 1 , charvar = F )
      x <- rbind( x[ 1:3 , ] , df1 , x[4 , ] , df2 , x[ 5:nrow(x) , ] )
    }
    
    #the 1995 aidsknow file has a typo at 434
    if ( year == 1995 & fileval == "aidsknow") {
      x[ 119 , "width" ] <- 1
      x[ 120 , "width" ] <- 1
      x[ 120 , "placement" ] <- 434
    }
    
    #the 1999 injverbt file needs to end at 677
    if ( year == 1999 & fileval == "injverbt") {
      x <- x[ 1:(nrow(x)-1) , ] 
      x[ 18 , "width" ] <- 85
    }
    
    #the 2007 ratcat07 file needs to end at 14
    if ( year == 2007 & fileval == "ratcat07") {
      x <- x[ 1:(nrow(x)-1) , ] 
      x[ 4 , "width" ] <- 2
    }
    
    #end manual fixes
    
    
    
    ##input actual SAS data text-delimited file to read in
    SASfile <- read.fwf( fn , x$width , col.names=x$varname)
    
    ##convert character strings appropriately within data frame
    for (l in 1:nrow(x)){
      if (x[l,"charvar"]) SASfile[,l]<-as.character(SASfile[,l])
      else SASfile[,l]<-as.numeric(SASfile[,l])
      
      if (x[l,"divisor"] != 1 ) SASfile[,l] <- as.character( SASfile[,l] * as.numeric( x[l,"divisor"]) )
    }
    
    #make all names lowercase
    names( SASfile ) <- tolower( names( SASfile ) )
    
    #throw out any TOSS variables
    SASfile$toss <-NULL
    SASfile$toss.1 <-NULL
    
    #output directory
    output_directory <- paste( "C:/users/atala/documents/NHIS/Data/" , year , sep = "" )
    #if the year path doesn't exist, make it!
    try( dir.create( output_directory )  , silent = T )
    
    setwd( output_directory )
    
    write.csv( SASfile , paste( fileval , ".csv" , sep = "" ) , row.names = F )
    write.dta( SASfile , paste( fileval , ".dta" , sep = "" ) )
    
    
    #trashremoval
    trash<-ls()[! ls() %in% c("trash","filename","filenames","year","which_years_to_download","nhis_file_ftp","year_ftp_dir")]
    for (ij in 1:length(trash)) rm(list=trash[ij])
    gc()
  }
}