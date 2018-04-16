#############################
#### Script to get NBER working papers meta-data
#### Author: Fernando Hoces de la Guardia
start.time <- Sys.time()

## Write here the folder where you want to download the data
MY.PATH <- "C:/Users/fhocesde/Documents/learninng R"

# Create directory
dir.create(file.path(MY.PATH, "NBER"), showWarnings = FALSE)
MY.PATH <- paste(MY.PATH,"NBER/", sep="" )

if (FALSE) {
  set.seed(142857)
  list.1 <- 1:21500
  papers_1 <- sample(list.1, 15, replace = FALSE)
  # list1 =  5888  5929  6716  7601 19919  6778  4309  4509 20026  8087
  
  #Oct21
  list.2 <- list.1[!(list.1 %in% papers_1)]
  papers_2 <- sample(list.2, 15, replace = FALSE)
  # list2 =  6592 13651 20674  4012 17630  7318 13086  9586  9645 21107 19889  5404 16918  8060   157
  
  #Oct22
  list.3 <- list.2[!(list.2 %in% papers_2)]
  papers_3 <- sample(list.3, 15, replace = FALSE)
  # list3 =  4416 17754 20573 12585 10639 18330  7897 17837 21351  3935 13059 15348 16362 17184  3276
  papers <- papers_3
  
  #Oct26
  list.4 <- list.3[!(list.3 %in% papers_3)]
  papers_4 <- sample(list.4, 15, replace = FALSE)
  # list4 =  7758  8527  8768 21152  2079  9907 12692 18178  3881  3121 20983   240 14785  4789  2340
  papers <- papers_4
  
  #Oct28 NEED TO RE RUN THIS ONE
  list.5 <- list.4[!(list.4 %in% papers_4)]
  papers_5 <- sample(list.5, 15, replace = FALSE)
  # list4 =  10963 14619 10669 12049  6965 19775 11874  8773  3687 18822 16791 19213  8533 13493 2893
  #Oct29
  list.6 <- list.5[!(list.5 %in% papers_5)]
  papers_6 <- sample(list.6, 15, replace = FALSE)
  # list6 =  9831 14023 14002   530 17473  9110 21224  9021  7589 12218  7534 17390 15868 15830 6025
  papers <- papers_5
}

papers <- 1:21654

df <- data.frame(authors = rep(NA, length(papers)), 
           title  = rep(NA, length(papers)), 
           date = rep(NA, length(papers)), 
           abstract = rep(NA, length(papers)), 
           published = rep(NA, length(papers)),
           NBER_cat = rep(NA, length(papers)))
j <- 0
for (i in papers) {
  j <- j + 1
  ## read in the website that cotains links to all the pages where we want to download data
  raw_lines <- tryCatch(
              readLines( paste("https://www.nber.org/papers/w", i, sep = "") ), 
              error = function(e) NULL)
  # If there is no such paper, jump to next one
  while (is.null(raw_lines)) {
    i <- i + 1
    j <- j + 1
    raw_lines <- tryCatch(
      readLines(paste("https://www.nber.org/papers/w", i, sep = "")), 
      error = function(e) NULL)
    }
  ## define pattern of the websites that you want to go
  #Authors
  pattern_1 <- '<meta name="citation_author" content=.*' 
  #Title
  pattern_2 <- '<meta name="citation_title" content=.*' 
  #Publication date
  pattern_3 <- '<meta name="DC.Date" content=.*' 
  #pattern_4 should be the abstract, but so far I can pull it directly
  
  #What it published? NEED IMPROVING: if empty, go two lines below
  pattern_5 <- "<p id='published_line'>.*" 
  #NBER Program(s)
  # regmatches(string, gregexpr("([A-Z]{1,4})\\.html", string))
  pattern_6 <- "<b>NBER Program.*" 
  
  ## identify which lines contain those urls
  extract_lines_1 <- grepl(pattern_1, raw_lines)
  extract_lines_2 <- grepl(pattern_2, raw_lines)
  extract_lines_3 <- grepl(pattern_3, raw_lines)
  extract_lines_5 <- grepl(pattern_5, raw_lines)
  extract_lines_6 <- grepl(pattern_6, raw_lines)
  
  
  matched.lines_1 <- raw_lines[extract_lines_1]
  matched.lines_2 <- raw_lines[extract_lines_2]
  matched.lines_3 <- raw_lines[extract_lines_3]
  matched.lines_5 <- raw_lines[extract_lines_5]
  matched.lines_6 <- raw_lines[extract_lines_6]
  
  ## clean the extracted lines
  clean_lines1 <- sub('(<meta name=\"citation_author\" content=\")(.*)(\">)', "\\2", matched.lines_1)
  clean_lines2 <- sub('(<meta name="citation_title" content=\")(.*)(\">)', "\\2", matched.lines_2)
  clean_lines3 <- sub('(<meta name="DC.Date" content=\")(.*)(\">)', "\\2", matched.lines_3)
  clean_lines4 <- raw_lines[which.max(nchar(raw_lines))]
  if (length(matched.lines_5)==0) {
    clean_lines5 <- "Not published"   
  } else {
  clean_lines5 <- matched.lines_5
  }
  if (length(matched.lines_6)==0) {
    clean_lines6 <- "No Category"   
  } else {
    clean_lines6 <- matched.lines_6
  }  
  
  # Figure out how to get 'everything between .html and /, multiple times'
  #clean_lines6 <- sub('(<meta name="DC.Date" content=\")(.*)(\">)', "\\2", matched.lines_6)
  df$authors[j] <- paste(clean_lines1, collapse = "-*-" )
  df$title[j] <- clean_lines2 
  df$date[j] <- clean_lines3 
  df$abstract[j]<- clean_lines4
  df$published[j] <- clean_lines5
  df$NBER_cat[j] <- clean_lines6

  if (j%%50 == 0) {
    print(paste(round(j/length(papers), 3) * 100,"% done", sep = ""))
    } 
}

MY.FILE <- paste(MY.PATH,"", sep="" )
# CHANGE THIS EVERY DAY
save(df, file = "all_NBER_Papers_v1.RData")

if (FALSE) {
  load("C:/Users/fhocesde/Documents/Oct_20.RData", envir = parent.frame(), verbose = FALSE)
  df1 <- df
  load("C:/Users/fhocesde/Documents/all_NBER_Papers2.RData", envir = parent.frame(), verbose = FALSE)
  df1 <- rbind(df1, df)
  load("C:/Users/fhocesde/Documents/Oct_22.RData", envir = parent.frame(), verbose = FALSE)
  df1 <- rbind(df1, df)
  load("C:/Users/fhocesde/Documents/Oct_26.RData", envir = parent.frame(), verbose = FALSE)
  df1 <- rbind(df1, df)
  load("C:/Users/fhocesde/Documents/Oct_28.RData", envir = parent.frame(), verbose = FALSE)
  df1 <- rbind(df1, df)
  load("C:/Users/fhocesde/Documents/Oct_29.RData", envir = parent.frame(), verbose = FALSE)
  df1 <- rbind(df1, df)
}

print(Sys.time() - start.time)


load("C:/Users/fhocesde/Documents/all_NBER_Papers_v1.RData", envir = parent.frame(), verbose = FALSE)
aux1 <- with(df,  regmatches(NBER_cat, gregexpr("([A-Z]{1,4})\\.html", NBER_cat)))
df$NBER_cat <- sapply(aux1, function(y) paste( sub(x =y, ".html", "") , collapse = "-*-" ) )

#Clean NBER categories: 


#nchar(df$abstract)
#cbind(table(format(as.Date(df1$date), "%Y"))/length(df1$date))
#mean(df1$published!="Not published")
#mytable <- table(format(as.Date(df1$date), "%Y"), (df1$published!="Not published") )
#prop.table(mytable, 1)
#mean((df1$published!="Not published")[as.Date(df1$date)>as.Date("2000-01-01") & as.Date(df1$date)<as.Date("2013-01-01")])

#asd <- tapply((df$published!="Not published"), format(as.Date(df$date), "%Y-%m"), mean)
#plot(asd, type="l", ylim=c(0,1))
#abline(v=which(rownames(asd)=="2010-10"))


x11()
#Percentages of abstracts saying certain phrases
asd <- tapply(grepl("statistically significant", df$abstract), format(as.Date(df$date), "%Y"), mean)
plot(asd, type="l")



#### First stilised fact: number of authors per paper has increased steadily over the last 40 years.#### 
num_authors <- sapply(strsplit(df$authors, "-*-", fixed = TRUE), length)
asd <- tapply(num_authors, format(as.Date(df$date), "%Y-%m"), mean)
plot(asd, type="l")

### Do something similar with NBER category. 

df1 <- df[!is.na(df$NBER_cat), ]
asd1 <- strsplit(df1$NBER_cat[], "-*-", fixed = TRUE)
length(unique(unlist(asd1)))

n <- sapply(asd1, length)
published <- rep(1*(df1$published!="Not published"), times = n)
date1 <- format(as.Date( rep(df1$date, times = n) ), "%Y")
categories <- unlist(asd1)
authors <- rep((df1$authors), times = n)
df3 <- data.frame(authors,published, categories, date1, stringsAsFactors = FALSE)

asd1 <- strsplit(df3$authors, "-*-", fixed = TRUE)
n <- sapply(asd1, length)
published <- rep(df3$published, times = n)
categories <- rep(df3$categories, times = n)
date1 <- rep(df3$date1, times = n)
first.name <- gsub("^(.*?),\\s(\\w+).*", "\\2",  unlist(asd1))
temp1 <- gender::gender(first.name)
temp1 <- temp1[!duplicated(temp1),]


temp2 <- data.frame("name"=first.name, date1, categories)
temp3  <- right_join(temp1, temp2)
#####NEED TO UNDERSTAND THE MISMATCH
temp3 <- temp3[!is.na(temp3$gender),]

#Percentage of women by category
df3 <- tapply(temp3$gender!='male', INDEX = list(temp3$date1, temp3$categories), function(x) mean(x, na.rm = TRUE)) 
df3 <- df3[which(rownames(df3)%in%1980:2015),]

df3[is.na(df3)] <- 0
plot(1980:2015,df3[,1], type ="l" ,lwd=0.1, ylim = c(0,1))
for (i in ( 2:(dim(df3)[2]) ) ) lines(1980:2015,df3[,i], type ="l" ,lwd=1)
lines(1980:2015,df3[,"EFG"], type ="l" ,lwd=1, col="red")
lines(1980:2015,df3[,"LS"], type ="l" ,lwd=1, col="blue")
lines(1980:2015,df3[,"PE"], type ="l" ,lwd=1, col="green")



df1 <- df[!is.na(df$NBER_cat), ]
asd1 <- strsplit(df1$NBER_cat[], "-*-", fixed = TRUE)
length(unique(unlist(asd1)))
n <- sapply(asd1, length)
published <- rep(1*(df1$published!="Not published"), times = n)
date1 <- format(as.Date( rep(df1$date, times = n) ), "%Y")
categories <- unlist(asd1)
authors <- rep((df1$authors), times = n)
df3 <- data.frame(authors,published, categories, date1, stringsAsFactors = FALSE)

table1 <- table(date1, categories)
df3 <- tapply(published, INDEX = list(date1, categories), function(x) mean(x, na.rm = TRUE)) 
df3 <- df3[which(rownames(df3)%in%1980:2015),]

df2 <- as.data.frame.matrix(prop.table(table1, 1))
df2 <- df2[which(rownames(df2)%in%1980:2015),]
x11()
plot(1980:2015,df2[,1], type ="l" ,lwd=0.1, ylim = c(0,.4))
for (i in ( 2:(dim(df2)[2]) ) ) lines(1980:2015,df2[,i], type ="l" ,lwd=1)

lines(1980:2015,df2[,"EFG"], type ="l" ,lwd=1, col="red")
lines(1980:2015,df2[,"LS"], type ="l" ,lwd=1, col="blue")
lines(1980:2015,df2[,"PE"], type ="l" ,lwd=1, col="green")

df3[is.na(df3)] <- 0

apply(df3,2, function(x) which(x==0))

x11()
plot(1980:2015,df3[,1], type ="l" ,lwd=0.1, ylim = c(0,1))
for (i in ( 2:(dim(df3)[2]) ) ) lines(1980:2015,df3[,i], type ="l" ,lwd=1)
lines(1980:2015,df3[,"EFG"], type ="l" ,lwd=1, col="red")
lines(1980:2015,df3[,"LS"], type ="l" ,lwd=1, col="blue")
lines(1980:2015,df3[,"PE"], type ="l" ,lwd=1, col="green")


## Any effect of alphabetical order?
## Gender over time
# last name is not working well
library(arm)
library(gender)
library(dplyr)


asd1 <- strsplit(df$authors, "-*-", fixed = TRUE)
#Total number of authors
length(unique(unlist(asd1)))

n <- sapply(asd1, length)
published <- rep(df$published, times = n)

first.name <- gsub("^(.*?),\\s(\\w+).*", "\\2",  unlist(asd1))
last.name <- gsub("^(.*?),\\s(\\w+).*", "\\1",  unlist(asd1))
first.letter <- toupper(substr(last.name, 1, 1))
temp1 <- gender(first.name)
temp1 <- temp1[!duplicated(temp1),]


date1 <- format(as.Date( rep(df$date, times = n) ), "%Y")

temp2 <- data.frame("name"=first.name, date1, published)

temp3  <- right_join(temp1, temp2)

#####NEED TO UNDERSTAND THE MISMATCH
temp3 <- temp3[!is.na(temp3$gender),]

#Percentage of women
asd <-  tapply(temp3$gender!='male', temp3$date1, mean)
plot(x = 1973:2015, y = asd, type="l")

#Publishing rate by gender over time
df2 <- (tapply(temp3$published!="Not published", list(temp3$date1, temp3$gender), mean))
plot(x = 1973:2015, y = df2[,"female"], type="l", lty=3)
lines(x = 1973:2015, y = df2[,"male"], type="l", lty=1)
legend("topright",lty=c(3,1), legend = c("Female", "Male"))

published <- rep(df$published, times = n)
df1 <- data.frame(published, unlist(asd1))

#Overall count of WP by first letter
df2 <- tapply(df1$published, first.letter, length)
barplot(df2[(rownames(df2)%in%LETTERS)])

#Overall percentage published
tapply(df1$published!="Not published", as.factor(first.letter%in%LETTERS[1:13]), mean)


#Regression
#Delete letters with very few authors:
to.delete <- which(LETTERS%in%c("Q", "U", "X", "Y", "Z"))

df3 <- data.frame(y = (df1$published!="Not published"), first.letter)
df3 <- df3[first.letter%in%LETTERS[-to.delete],]
fit1 <- lm(y~first.letter-1, data = df3)
arm::coefplot(fit1)
abline(v = mean(df3$y))

#If last name is between A-H then mean publication rate is 2% higher. 
d1 <- 1*(df3$first.letter%in%LETTERS[1:8])
coef(summary(lm(df3$y~d1)))


#Count of WP over time
df2 <- (tapply(df1$published!="Not published", list(first.letter,format(as.Date(rep(df$date, times = n)), "%Y")), length))
plot(1973:2015,df2[1,], type ="l" ,lwd=0.1, ylim = c(0,300), col="red")
for (i in ( 2:(dim(df2)[1]/2) ) ) lines(1973:2015,df2[i,], type ="l" ,lwd=1, col="red")
for (i in ( (dim(df2)[1]/2+1):(dim(df2)[1]) ) ) lines(1973:2015,df2[i,], type ="l" ,lwd=1, col="Blue")

# Percentage published over time
df2 <- (tapply(df1$published!="Not published", list(first.letter,format(as.Date(rep(df$date, times = n)), "%Y")), mean))
plot(1973:2015,df2[1,], type ="l" ,lwd=0.1, ylim = c(0,1), col="red")
for (i in ( 2:(dim(df2)[1]/2) ) ) lines(1973:2015,df2[i,], type ="l" ,lwd=1, col="red")
for (i in ( (dim(df2)[1]/2+1):(dim(df2)[1]) ) ) lines(1973:2015,df2[i,], type ="l" ,lwd=1, col="Blue")


###Need to get NBER fields

###Do word clouds

### Extract 2-3 word phrases

