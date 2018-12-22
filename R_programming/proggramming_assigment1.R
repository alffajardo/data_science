pollutantmean <- function(directory,pollutant,id=1:332){

#pollutant <- "nitrate"
#id <- 23
#directory <- "specdata"
if (pollutant=="sulfate"){
  c <- 2
} else if (pollutant== "nitrate"){
    c <- 3
}
 
files <- list.files((directory))[id]
all_values <- c()

for (i in files){
  
  file <- paste(directory,i,sep = "/")
  
  values <- read.csv(file)[[c]]
  
  all_values <- append(all_values,values)
}

mean_vals <- mean(all_values,na.rm = T)
return(mean_vals)

}

complete <- function (directory,id=1:332){
 
  index <- c()
  nobs <- c()
   
  files <- list.files(directory)[id]
  for (i in files){
  
    index <- append(index,i)
    file <- paste(directory,i,sep = "/")
  f <- read.csv(file)
  n <- length(complete.cases(f)[complete.cases(f)==T])
  nobs <- append(nobs,n)
  }
  c.cases <- data.frame(index,nobs)
  names(c.cases)[1] <- "id"
  
  return(c.cases)
  
  }
  

corr <- function(directory, threshold = 0){
  
  files <- dir(directory)
  corrv <- c()
  for ( f in files ){
    
    file <- paste(directory,f,sep="/")
    file <- read.csv(file)
    file <- file[complete.cases(file),]
  
   if (nrow(file)>threshold){
     
     c <- cor(file$sulfate,file$nitrate)
     corrv <- append(corrv,c)
   }
  
  }
  
  return(corrv)
}
