library(rjson)
json_data <- fromJSON(paste(readLines('data/nytimes.json'), collapse=""))
df <- data.frame()

for (n in json_data$results){
  year <- substr(n$date, 0, 4)
  month <- substr(n$date, 5, 6)
  day <- substr(n$date, 7, 8)
  author <- substr(n$byline, 4, 30)
  title <- n$title
  if (length(author) < 1){
    author <- "unknown"
  }
  datestamp <- paste(month, "/", day, "/", year, sep="")
  datestamp <- as.Date(datestamp, "%m/%d/%Y")
  newrow <- data.frame(datestamp, author, title, stringsAsFactors=FALSE, check.rows=FALSE)
  df <- rbind(df, newrow)
}

rownames(df) = df$datestamp
yearlist <- as.POSIXlt(df$datestamp)$year+1900

stem(yearlist)

length(unique(df$author))

plot(table(df$author), axes=FALSE)

hist(yearlist)

hist(yearlist, breaks=(2005:2007), freq=TRUE, col="#CCCCCC", main="Distribution of articles about Data Visualization by NYT", xlab="Year")
