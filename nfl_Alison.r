install.packages("fivethirtyeight")

install.packages("ggplot2")

install.packages("plyr")

library(plyr)

library(fivethirtyeight)

library(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {

    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

head(nfl_suspensions)

help(nfl_suspensions)

ggplot(nfl_suspensions)

countYear <- count(nfl_suspensions, 'year')
ggplot(countYear, aes(x=year, y= freq)) + geom_line() + scale_x_continuous(breaks = seq(1940, 2015, by = 5))+
  scale_y_continuous(breaks = seq(0, 50, by = 5))

countTeam <- count(nfl_suspensions, 'team')
countTeam <- arrange(countTeam, desc(freq) )
ggplot(countTeam, aes(x=team, y= freq)) + geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

countTeam2 <- head(countTeam, 4)
count <- 0

for (val in countTeam2$team) {
  tbl <- (as.data.frame(table(nfl_suspensions[which(nfl_suspensions$team==val), ]["year"])))
  assign(paste("tbl", count, sep = ""), ggplot(tbl, aes(x=Var1, y= Freq)) + geom_point())
  count <- count + 1
}

multiplot(tbl0, tbl1, tbl2, tbl3, cols=2)

print(mean(as.numeric(nfl_suspensions$games),na.rm=TRUE))

print(median(as.numeric(nfl_suspensions$games),na.rm=TRUE))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

print(getmode(as.numeric(nfl_suspensions$games)))

print(getmode(as.numeric(nfl_suspensions$year)))

