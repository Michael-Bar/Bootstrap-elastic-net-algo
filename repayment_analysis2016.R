#analysis of 2016 merged data
### based on output from prelim_R which merges SC data,x's  planting data and Jan's GPS data
#this script will read in data (or a random subset at least), clean and merge it.

# ##notes
# unique GPS for semi.name (i.e. district-site) at the lowest level, pointless to look at group level (full.name)
# awhere data provides, for each GPS, the average weather date PER DAY


##devtools::install_github("cran/ggplot2", force = T)

library(dplyr)
library(randomForest)
library(lattice)
library(parallel)
library(aod)
library(zoo)

library(ggplot2)

library(ROCR)

library(corrplot)

library(broom)
library(robustbase)
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



sitedat <- read_csv("C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/2016_site_Client_plant_gps_full.csv")
sitedat <- sitedat[complete.cases(sitedat$Latitude),]

awhr <- read_csv("C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/aWhere/full_awhere_05-15.csv")
awhr16 <- read_csv("C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/aWhere/full_awhere_16.csv")

# awhr$semi.name <- "A"
# awhr16$semi.name <- "B"
# 
# x <- seq(1:dim(sitedat)[1])
# x<- (x*366)-365
# for(i in 1:length(x)   ){
#   
#     loc <- x[i]
#     loc2 <- loc+366
#     nam <- sitedat$semi.name[i]
#     awhr[loc:loc2,]$semi.name <- nam
#     awhr16[loc:loc2,]$semi.name <- nam }
# 
# write.csv(awhr, "C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/aWhere/full_awhere_05-15.csv")
# write.csv(awhr16,"C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/aWhere/full_awhere_16.csv")


histo_pet <-  ddply(awhr, c("day"), summarise,  avpet= mean(pet.average), avsd = mean(pet.stdDev)   )
sixt_pet <-  ddply(awhr16, c("date"), summarise,  pet= mean(precipitation.amount), sdpet = sd(precipitation.amount)   )
sixt_pet <- subset(sixt_pet, date != "2016-02-29")
histo_pet$day <- as.Date(histo_pet$day, format = "%m-%d")
histo_pet <- histo_pet[complete.cases(histo_pet),]
histo_pet$pet16 <- sixt_pet$pet
histo_pet$petsd16 <- sixt_pet$sdpet


histo_pet$pet16mov <- rollmean(histo_pet$pet16,14, na.pad=TRUE)

#+ geom_smooth( aes(x=day,y=pet16, colour="Smoothed 2016"), method="loess", inherit.aes=TRUE) 

p1 <- ggplot(histo_pet) + geom_point(aes(x=day, y=avpet, colour="Historical")    ) + geom_ribbon(aes(x=day, ymax=avpet+avsd,ymin=avpet-avsd),alpha=0.1) + geom_point(aes(x=day,y=pet16, colour="2016"))  +  scale_x_date(date_breaks = "2 month", limits = as.Date(c('2017-01-01','2017-12-30')))
p1 <- p1 + geom_line(data=histo_pet, aes(x=day, y=pet16mov, colour="moving av"),size=1.5 , color="red")                                                                                                                                                                                                                                                                                                                      
p1

p3 <- ggplot(sitedat) + geom_freqpoly(binwidth=8,aes(x=first.maize.plant+365,colour="First planting"),size=2) 

p3 <- p3  + geom_freqpoly(binwidth=8,aes(x=last.maize.plant+365,colour='Last planting'),size=2) + xlab("Date") + scale_x_date(date_breaks = "2 month", 
                                                                                                                              limits = as.Date(c('2017-01-01','2017-12-30')))
#all kenya

multiplot(p1,p3 ,cols=1)
######### pasted in -----

####
#lets make up sums for the growing season
#get mean planting date
awhr$day <- as.Date(awhr$day, format = "%m-%d") - 365 # fake like its 2016
mu_plant <-  mean(sitedat$first.maize.plant) 

#subset awhere to mean first plant + 4 months(growth estimate based on nothing)
growdat <- subset(awhr, day >= mu_plant & day <= mu_plant+120)
growdat16 <- subset(awhr16, date >= mu_plant & date <= mu_plant+120)

#summarise by site
head(growdat)
sum(is.na(match(growdat$semi.name,growdat16$semi.name )))

avwhr <- growdat %>% group_by(semi.name) %>% summarise_each(funs(mean))
avwhr16 <- growdat16 %>% group_by(semi.name) %>% summarise_each(funs(mean))
avwhr <- Filter(function(x)!all(is.na(x)), avwhr)
avwhr16 <- Filter(function(x)!all(is.na(x)), avwhr16)
avwhr16$X1 <- NULL
avwhr16$X1_1 <- NULL
avwhr$X1 <- NULL
avwhr$X1_1 <- NULL
colnames(avwhr)
colnames(avwhr16) <- paste(colnames(avwhr16), "2016", sep="_")
colnames(avwhr16)[1] <- "semi.name"


avdat <- merge(sitedat, avwhr, by="semi.name")
avdat <- merge(avdat, avwhr16, by="semi.name")

avdat$day <- NULL
avdat$date_2016 <- NULL
colnames(avdat)
#slim it down a bit
slimdat <- avdat[c(1:18,581:584,603:dim(avdat)[2])]
slimdat <- Filter(function(x)!all(is.na(x)), slimdat)
colnames(slimdat)
slimdat$OAFID <- NULL
slimdat$SeasonID <- NULL
slimdat$RegionID <- NULL
slimdat$DistrictID <- NULL
slimdat$SectorID<- NULL
slimdat$SiteID<- NULL
slimdat$GroupID <- NULL
slimdat$nom <- NULL

##try a simple linear reg
lapply(slimdat,class)

#make default flag
slimdat$default <- slimdat$X..Repaid
slimdat$default[slimdat$default < 100] <- 1
slimdat$default[slimdat$default == 100] <- 0


slimdat$semi.name

slimdat$TotalRepaid <- NULL
slimdat$TotalRepaid_IncludingOverpayments <- NULL
slimdat$RemainingCredit <- NULL
slimdat$pc.new <- NULL
slimdat$first.maize.plant <- NULL
slimdat$last.maize.plant <- NULL
summary(slimdat$X..Repaid)

slimdat$logit_repaid <- logit(slimdat$X..Repaid, percents=TRUE)
slimdat$logit_repaid <- NULL

#site.lm <- lmrob(formula = default ~ . -semi.name -X..Repaid, data = slimdat)

#select features for logit via RF

slimdat$default <- as.factor(slimdat$default)
set.seed(123123)
sample = sample.split(slimdat, SplitRatio = 0.75) #sample contains only boolean indecies
train = subset(slimdat, sample == TRUE)
hold = subset(slimdat, sample == FALSE)
train_y <- train$default
hold_y <- hold$default

#look at autocorrelation
x <- grep("Date", lapply(train,class) )
train[x] <- as.numeric(unlist(train[x]))
x<- c("semi.name","default")
t <- train
t[x] <- NULL

lapply(t,class)
grep("factor", lapply(train,class) )
corrplot(cor(t) ,tl.cex=0.5, tl.col='blue', tl.pos='lower'  ) 
sum(is.na(train))




#

rf.m <- randomForest(as.factor(default) ~ .  -semi.name -X..Repaid - location.latitude_2016 - location.longitude_2016 - location.fieldId_2016 , ntree=300, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=train,cutoff=c(0.5,0.5))
rf.m


#eval RF
pred <- predict(rf.m, newdata = hold , type="response", norm.votes=TRUE) 
table(observed = hold[, "default"], predicted = pred)
tpr <- table(observed = hold[, "default"], predicted = pred)[4] / (table(observed = hold[, "default"], predicted = pred)[4]   +  table(observed = hold[, "default"], predicted = pred)[2]  )
tnr <- table(observed = hold[, "default"], predicted = pred)[1] / (table(observed = hold[, "default"], predicted = pred)[1] + table(observed = hold[, "default"], predicted = pred)[3]   )

print("##################") 
print(paste("sensitivity % , i.e. true positives = ", round(tpr*100,2)))
print(paste("specificity %, i.e. true negatives = ", round(tnr*100,2)))
print(paste("false positives %", round((1-tnr)*100,2)))
print(paste("false negatives %", round((1-tpr)*100,2)))
print("##################") 

##Eval RF --------
#cross validation first
pred <- predict(rf.m, newdata = hold , type="response", norm.votes=TRUE) 
x <- as.numeric(pred) -1
cv <- x + (as.numeric(hold_y))
cv
correct_predictions <- (sum(cv==0) + sum(cv==2)) /  length(cv) *100


print(paste("Total correct prediction % ==", round(correct_predictions,2 )))
print(paste("Total error  % ==",  100 - round(correct_predictions,2)   )    )

#####look at RF

rf.m$importance
varImpPlot(rf.m, type=1)
as.data.frame( sort(rf.m$importance[,3],decreasing = TRUE)  )

#run logit

default_logit <- glm(as.factor(default) ~ . -semi.name -X..Repaid -location.latitude_2016 -location.longitude_2016 ,
               data = train, family = "binomial")
summary(mylogit)

plot(site.lm)

tidy_ <- tidy(site.lm)
tidy_[ order(-tidy_[,2]), ]
outp <- stargazer::stargazer(site.lm, type="text")

write.table(x, "./regression_out/reg.txt")
write.csv(tidy_, "./regression_out/site_full_LM.csv")


summary(site.lm)
summary(site.lm)$r.squared



#RF

rf.m <- randomForest(logit_repaid ~ .  -semi.name -X..Repaid - location.latitude_2016 - location.longitude_2016 , ntree=300, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=slimdat,cutoff=c(0.5,0.5))
rf.m$importance

as.data.frame( sort(rf.m$importance[,1],decreasing = TRUE)  )

varImpPlot(rf.m, type=1, main="RF first pass - group level default classifier") 



stop()

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











##### 

stop()

supdat <- sitedat

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








