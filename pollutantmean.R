pollutantmean <- function(directory, pollutant, id = 1:332) {
    iteration <- 1
    for (i in id) {
        tempData <- openSingleFile(directory, pollutant, i)
        
        if (iteration == 1) {
            data = tempData
        } else {
            data <- rbind(data, tempData)
        }
        iteration <- iteration + 1
    }
    mean(data[[pollutant]], na.rm=TRUE)
}

openSingleFile <- function(directory, pollutant, id) {
    paddedID <- substrRight(paste("00", id, sep=""), 3)
    filename <- paste("./", directory, "/", paddedID, ".csv", sep="")
    temp <- read.csv(filename)
    temp[pollutant]
}

substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}