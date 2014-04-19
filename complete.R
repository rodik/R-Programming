
complete <- function(directory, id = 1:332) {
    table = data.frame(id = numeric(0), nobs = numeric(0))
    
    for (i in id) {
        nobs <- getSingleFileNobs(directory, i)
        
        table <- rbind(table, c(i,nobs))
    }
    colnames(table) <- c("id","nobs")
    table
}

getSingleFileNobs <- function(directory, id) {
    paddedID <- substrRight(paste("00", id, sep=""), 3)
    filename <- paste("./", directory, "/", paddedID, ".csv", sep="")
    temp <- read.csv(filename)
    nrow(temp[!is.na(temp$sulfate) & !is.na(temp$nitrate),])
}

substrRight <- function(string, n){
    substr(string, nchar(string)-n+1, nchar(string))
}