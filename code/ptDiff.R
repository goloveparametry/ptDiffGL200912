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

for(i in 1:3) b <- c(b,c(rep(i,240)))
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
  c(names(ts[length(ts)]), ts[length(ts)], 
    names(ts[length(ts)-1]), ts[length(ts)-1], 
    names(ts[2]), ts[2],
    names(ts[1]), ts[1])
}    

# final

                         
s <- c("2009-10","2010-11","2011-12")

for (i in 1:3) {                         
  df <- data.frame(num=rep(NA, 30), 
                   t1=rep("", 30), t1_p=rep("", 30),
                   t2=rep("", 30), t2_p=rep("", 30),
                   tr1=rep("", 30), tr1_p=rep("", 30),
                   tr2=rep("", 30), tr2_p=rep("", 30),
                   stringsAsFactors=FALSE)
  for(j in 1:30) {
    df[j,] <- c(j, getCurrentTable(i,j))
  }
  
  # graphics  
  png(filename=paste("gcs/plot",s[i],".png",sep=""))
  plot(df$num, df$t1_p, 
       type="l",
       col="green", 
       main=paste("Gambrinus Liga: Point margin", s[i]), 
       xlab="week", ylab="pts", 
       ylim=c(0,70))
  lines(df$num, df$t2_p, col="orange")
  lines(df$num, df$tr1_p, col="red")
  lines(df$num, df$tr2_p, col="red")
  text(x = 30, y=df[30,c(3,5,7,9)],  labels = df[30,c(2,4,6,8)], cex=0.6, adj=c(1,-.5))
  dev.off()  
  
  # export
  write.csv(df[,c(1,3,5,9)],file=paste("data/df",s[i],".txt",sep=""), row.names=F, quote=F)
                         
}                         
                         
                         
             