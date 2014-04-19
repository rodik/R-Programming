
corr <- function(directory, threshold = 0) {
    cors <- numeric(0)
    for (i in 1:length(list.files(path=paste("./", directory, "/", sep="")))) {
        corel <- getSingleCorr(directory, i, threshold)
        if (!is.na(corel))
            cors <- c(cors, corel)
    }
    cors
}

getSingleCorr <- function(directory, id, threshold) {
    paddedID <- substrRight(paste("00", id, sep=""), 3)
    filename <- paste("./", directory, "/", paddedID, ".csv", sep="")
    temp <- read.csv(filename)
    temp <- temp[!is.na(temp$sulfate) & !is.na(temp$nitrate),]
    rows <- nrow(temp)
    
    if(rows > threshold)
        cor(temp$sulfate, temp$nitrate)
    else
        NA
}

substrRight <- function(string, n){
    substr(string, nchar(string)-n+1, nchar(string))
}