#analysis of 2016 merged data
### based on output from prelim_R which merges SC data,x's  planting data and Jan's GPS data
#this script will read in data (or a random subset at least), clean and merge it.

# ##notes
# unique GPS for semi.name (i.e. district-site) at the lowest level, pointless to look at group level (full.name)
# awhere data provides, for each GPS, the average weather date PER DAY

library(dplyr)
library(randomForest)
library(lattice)
library(parallel)
library(aod)
library(ggplot2)
library(broom)
library(Rcpp)
library(pscl)
library(caTools)
library(boot)
library(mice)
library(VIM)
library(plyr)
library(reshape)
library(forcats)
library(plotly)
library(car)
library(readr)

library(MKmisc)
library(ResourceSelection)


start.time <- proc.time()




# Functions ---------------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

reg_line <-function(models, m,x,inter){ #model name, slope, x list, intercept
  lobf <- c()
  xshort <-  x[seq(1, length(x), 6)]
  for(i in xshort){
    y= i * coef(models)[m] + coef(models)[inter]
    lobf <- c(lobf, y)
  }
  ls <- list("lobf"=lobf, "newx"=xshort)
  return(ls)
}


###slim down data

slim_my_data <- function(data_to_slim, fac, slimname){
  set.seed(101) #reproducible 
  sample = sample.split(data_to_slim, SplitRatio = fac) #sample contains only boolean indecies
  data.train = subset(data_to_slim, sample == TRUE)
  data.test = subset(data_to_slim, sample == FALSE)
  write.csv(data.train, file= slimname) }


excluded <- function(l1,l2){
  exc <- c()
  exc <- setdiff(l1,l2)
  exc <- c(exc, setdiff(l2,l1))
  exc <- unique(exc)
  return(exc)
}



repay_pc <- function(x){
  mx <- max(x)
  res <- (x/mx)*100.
  return(res)
}

number_ticks <- function(n) {function(limits) pretty(limits, n)}

sems <- function(x) sqrt(var(x)/length(x))

cohen_d <- function(m1,m2,s1,s2) {  
  spo <- sqrt((s1**2 + s2**2)/2)
  d <- (m1 - m2)/spo
  effsi <- d / sqrt((d**2)+4)
  ret <- list("d" = d, "effectsi" = effsi)
  return(ret)
  
}
###################################




sitedat <- read_csv("2016_site_Client_plant_gps_full.csv")

awhr <- read_csv("full_awhere_05-15.csv")

aw16 <- read_csv("full_awhere_16.csv")







######### pasted in -----



site.lm <- lm(formula = rep.rate ~ Border + qualify.pc + pc.new + TotalEnrolledSeasons + TotalCredit + corr.plant + plant.spread, data = supsite)
summary(site.lm)
summary(site.lm)$r.squared

sitef.lm <- lm(formula = rep.rate ~ . ,data = supsite)
summary(sitef.lm)
summary(sitef.lm)$r.squared

#use broom lib to clean up output and save

tidy_sitef <- tidy(sitef.lm)
tidy_sitef
write.csv(tidy_sitef, "site_full_LM.csv")


stop()

p1 <- ggplot(supdat, aes(first.maize.plant)) + geom_histogram() + xlab("First plant") + scale_x_date(date_breaks = "2 weeks" )

p2 <- ggplot(supdat, aes(last.maize.plant)) + geom_histogram() + xlab("Last plant") + scale_x_date(date_breaks = "2 weeks" )

p3 <- ggplot(supdat) + geom_freqpoly(binwidth=8,aes(x=first.maize.plant,colour="First maize planting"),size=2) 

p3 <- p3  + geom_freqpoly(binwidth=8,aes(x=last.maize.plant,colour='Last maize planting'),size=2) + xlab("Date") + scale_x_date(date_breaks = "1 month" )

p1
p2
multiplot(p1, p2, cols=1)
p3


#ggplot(supdat, aes(rep.rate)) + geom_histogram() + xlab("Repayment rate (time/credit) 2016") 

mai.lm4 <- lm(formula = rep.rate ~ plant.spread + District, data = supdat)
summary(mai.lm4)
summary(mai.lm4)$r.squared
coef(mai.lm2)["plant.spread"]
sp.co = 0
sp.co <- c()
for(i in unique(tot$District)){
  subby <- subset(tot, District==i)
  k = lm(formula= rep.pc ~ plant.spread, data=subby)
  summary(k)
  print(paste("plant spread coeff", coef(k)["plant.spread"]))
  sp.co <- c(sp.co, coef(k)["plant.spread"])
}
spreadco <- data.frame(sp.co, unique(tot$District))
spreadco <- spreadco[order(spreadco$sp.co),]
spreadco
set.seed(131)
colnames(tot)








