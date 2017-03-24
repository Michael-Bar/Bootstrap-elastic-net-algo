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



options(width=85)
#full data read in
supdat <- read_csv("2016_Client_plant_gps_full.csv")

#leave out some goods
#supdat <- read_csv("2016_Client_plant_gps.csv")

dim(supdat)

lapply(supdat,summary)

#kill some useless/high na colnames
colnames(supdat)
supdat$X <- NULL
supdat$fu1 <- NULL
supdat$fu2 <- NULL
supdat$SeasonName <- NULL
#redo rep rate
supdat$rep.rate <- NULL
supdat$NationalID <- NULL
supdat$FieldManagerPayrollID <- NULL
supdat$FieldOfficerPayrollID <- NULL
supdat$rep.rate <- (supdat$LastRepayment - supdat$FirstRepayment)/supdat$rep.pc
supdat$rep.rate <- as.numeric(supdat$rep.rate)
colnames(supdat)

#drop rows without repayment pc final
supdat <- supdat[complete.cases(supdat$rep.pc),]
supdat <- supdat[complete.cases(supdat$new.clients),]

#recode logicals as 1 0
supdat$NewMember <- gsub('True', 1.0, supdat$NewMember)
supdat$NewMember <- gsub('False', 0.0, supdat$NewMember)
supdat$NewMember <- as.numeric(supdat$NewMember)

supdat$Border <- gsub('no border', 0.0, supdat$Border)
supdat$Border <- gsub('border', 1.0, supdat$Border)
supdat$Border <- as.numeric(supdat$Border)

#full output, non condensed. Is about 0.5GB
#write.csv(supdat, file="2016_Client_plant_gps_full.csv", row.names = FALSE)



supsite <- supdat %>% group_by(semi.name) %>% summarise_each(funs(mean))

supsite$nom <- rownames(supsite)


#lets subset only non NAs
supsite <- Filter(function(x)!all(is.na(x)), supsite)

supsite$semi.name

supsite$pc.new <- supsite$new.clients/(supsite$new.clients + supsite$ret.clients) *100.
supsite$qualify.pc <- (supsite$total.qualified.clients/supsite$total.enrolled.clients)*100.
supsite$tot.client <- supsite$new.clients + supsite$ret.clients


#output condensed - site only form of DF
write.csv(supsite, file="2016_site_Client_plant_gps_full.csv", row.names = FALSE)

