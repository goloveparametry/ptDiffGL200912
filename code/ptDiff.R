gl09f <- read.csv("data/gl2009-12.csv", quote="") #full dataset

# "Date"   Date of match played
# "Time" 	Time of match played
# "H"		Home team goals
# "A"		Away team goals
# "Team_Home"	Home team name
# "Team_Away"	Away team name
# "Note"	Match note

str(gl09f)

# rename the column names
# colnames(gl09f) <- c("gh","ga","teamh","teama")

# add round and season
a <- NULL
b <- NULL

for(i in 1:30) a <- c(a,c(rep(i,8)))
gl09f$rnd <- a

for(i in 1:3) b <- c(b,c(rep(i,240)
gl09f$season <- b

gl09f$Date <- as.Date(gl09f$Date, "%d.%m.%Y")
                         
getCurrentTable <- function(s, r) {
  
  # subset season
  d <- subset(gl09f, season == s)  
  
  # get teams
  teams <- levels(droplevels(d)$Team_Home)  
  
  # make empty table
  t <- vector("numeric", length(teams))
  t[1:length(teams)] <- 0
  names(t) <- teams
  
  # subset current round
  d <- subset(gl09f, season == s & rnd <= r)
  
  # point logic
  for(i in 1:nrow(d)) {
    # print(paste("GAME #",i," | ",d[i,]$Team_Home," v. ",d[i,]$Team_Away,d[i,]$H ,":",d[i,]$A))
    if(d[i,]$H == d[i,]$A) {
      # each get 1 point
      t[paste(d[i,]$Team_Home)] <- t[paste(d[i,]$Team_Home)] + 1
      t[paste(d[i,]$Team_Away)] <- t[paste(d[i,]$Team_Away)] + 1
    }
    if(d[i,]$H > d[i,]$A) {
      # home team gets 3 points
      t[paste(d[i,]$Team_Home)] <- t[paste(d[i,]$Team_Home)] + 3
    }    
    if(d[i,]$H < d[i,]$A) {
      # away team gets 3 points
      t[paste(d[i,]$Team_Away)] <- t[paste(d[i,]$Team_Away)] + 3
    }  
  }
  
  # print table
  ts <- sort(t)
  # print(ts)
  
  # return difference between first and last
  c(names(ts[length(ts)]), ts[length(ts)], names(ts[1]), ts[1])
}    
                         
df <- data.frame(num=rep(NA, 30), t1=rep("", 30), t1_p=rep("", 30), tl=rep("", 30), tl_p=rep("", 30),stringsAsFactors=FALSE)
for(i in 1:30) {
  df[i,] <- c(i, getCurrentTable(1,i))
}
             