#This is a function that takes a data frame input with columns CHR, 
#POS, and P.value and identifies loci ONLY.
#Loci is based on user-provided distance of SNPs from each other.
#Mike Francis, 03-30-2020

#Example
#testdata<-as.data.frame(matrix(data= c(1, 1000, 1, 5000, 2, 1000, 2, 10000), nrow=4, ncol=2, byrow = TRUE))
#colnames(testdata)<-c("CHR", "POS")
#lociSNP(testdata, setdistance = 5000, output_df_name = "ls_output_df", writetofile = TRUE, output_file_name = "ls_output.txt")

###=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

lociSNP<-function(data_frame, setdistance = 1000000, 
                      output_df_name = "locileadSNPoutput", 
                      writetofile=FALSE,
                      output_file_name = "locileadSNP.out"){
    #Check if CHR and POS columns are present in dataframe
    if (! "CHR" %in% colnames(data_frame)){
    stop("data frame is missing column CHR")
    }
    if (! "POS" %in% colnames(data_frame)){
        stop("data frame is missing column POS")
    }

    data_frame$LOCI<-0
    data_frame$CHR<-as.numeric(data_frame$CHR)
    data_frame$POS<-as.numeric(data_frame$POS)
    #sort by chromosome then by position
    data_frame<-data_frame[order(data_frame$CHR, data_frame$POS),]
    #set chr to chromosome numbers that are present in df
    chr=unique(data_frame$CHR)
    #initialize group
    group=0
    #initialize output table
    output<-data.frame(matrix(NA, nrow=0, ncol=ncol(data_frame)))
    colnames(output)<-colnames(data_frame)

    #Main algorithm
    for(i in chr){
        group<-group+1
        currentchr<-data_frame[data_frame$CHR==i,]
        positions<-nrow(currentchr)
        currentchr$LOCI[1]<-group
        if (positions>1){
            for (j in 1:(positions-1)){
                actualdistance<-currentchr$POS[j+1]-currentchr$POS[j]
                if (actualdistance<=setdistance){
                    currentchr$LOCI[j+1]<-group
                } else if (actualdistance>setdistance){
                    group<-group+1
                    currentchr$LOCI[j+1]<-group
                }
            } #end j for loop
        }#end if positions >1 condition
        output<-rbind(output, currentchr)	
    }#end i for loop	

    
    #Write to file if user wants to
    if (writetofile == TRUE){
    outputfilename<-output_file_name
    write.table(output, outputfilename, quote=FALSE, row.names=FALSE)
    }
  
    #create R object of output table in global environment
    assign(x = output_df_name, value = output, envir = globalenv())
}#end function
###=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
