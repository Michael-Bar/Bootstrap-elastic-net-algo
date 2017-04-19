#this script will read in data (or a random subset at least), clean and merge it.

library(dplyr)
library(randomForest)
library(lattice)
library(parallel)
library(aod)
library(ggplot2)
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

library(MKmisc)
library(ResourceSelection)


start.time <- proc.time()

#functions
#lapply(1:3, function(x) c(x, x^2, x^3))
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
#full data and slim
data <- read.csv("Season Clients Master_20170203-091149.csv",strip.white=TRUE) # [ ,1:57] #2015 data

data[data==""] <- NA   #replace blank with NA# replace whitespace with NAN
#length(row.names(data))
#slim_my_data(data, 0.1, "slimmed_1.csv")

data2 <- read.csv("Season Clients Master_20170203-091037.csv",strip.white=TRUE) #[ ,1:55] #2016 data
data2[data2==""] <- NA   #replace blank with NA# replace whitespace with NAN
#length(row.names(data2))
#slim_my_data(data2, 0.1, "slimmed_2.csv")

#read slim data
#data <- read.csv("slimmed_1.csv",strip.white=TRUE) #2015 data 100rep
#data[data==""] <- NA   #replace blank with NA# replace whitespace with NAN
#length(row.names(data))

#data2 <- read.csv("slimmed_2.csv",strip.white=TRUE) #2016 data 98rep
#data2[data2==""] <- NA   #replace blank with NA# replace whitespace with NAN
#length(row.names(data2))


#data <- data[,1:57]
#data2 <- data2[,1:55]
summary(data)
summary(data2)

#lets give full anmes

#Q: are IDs conserved across years? i think not, therefore block out the following
#hmm looks like mostly unique IDS
#length( unique(data$OAFID)) - length(data$OAFID)
# so lets just work with them to start
#data <- data[!duplicated(data$OAFID),]
#length( unique(data$OAFID)) - length(data$OAFID)

#likewise
#length( unique(data2$OAFID)) - length(data2$OAFID)
#data2 <- data2[!duplicated(data2$OAFID),]
#length( unique(data$OAFID)) - length(data$OAFID)

colnames(data)
data$rep.pc<- (data$TotalRepaid/ data$TotalCredit)*100.
data2$rep.pc<- (data2$TotalRepaid/ data2$TotalCredit)*100.


#COMMENTED OUT FOR SPEED#############
# #fullnames
# sitedata2
# grpdata
# summary(data2$rep.pc)
# 
# #breakdown data to look at vars
# sitedata <- ddply(data, c("SiteName"), summarise,  avrep = mean(rep.pc), sd = sd(rep.pc),  semss = sems(rep.pc))
# grpdata <- ddply(data, c("GroupName"), summarise, avrep    = mean(rep.pc), sd = sd(rep.pc), semss = sems(rep.pc))
# 
# sitedata2 <- ddply(data2, c("SiteName"), summarise,  avrep = mean(rep.pc), sd = sd(rep.pc),  semss = sems(rep.pc))
# grpdata2 <- ddply(data2, c("GroupName"), summarise, avrep    = mean(rep.pc), sd = sd(rep.pc), semss = sems(rep.pc))
# 
# 
# 
# 
# exc <- excluded(sitedata$SiteName, sitedata2$SiteName)
# 
# #exc the identified list from dataframes
# sitedata = subset(sitedata, !(sitedata$SiteName %in% exc))
# sitedata2 = subset(sitedata2, !(sitedata2$SiteName %in% exc))
# #"%ni%" <- Negate("%in%")
# summary(sitedata)
# #order alphabet to make sure
# sitedata <- sitedata[order(sitedata$SiteName),]
# sitedata2 <- sitedata2[order(sitedata2$SiteName),] 
# 
# ggplot() + geom_point(aes(x=sitedata$avrep, y=sitedata2$avrep))
# 
# #2015 is 100% repayment, which causes issues for R2 calc and predictive modelling - use 2013/2014 instead?
#-#-#-#-#-#-#-##-#-#-#-#-#-#-#-#-#




# bring in planting dates -------------------------------------------------


### bring in planting data 2016
#lets try looking at the SPAN of planting by doing final date/first date
#lets looks at the lateness (we will add in seasons later) - this will just be data$col - min(data$col) - this is corr.plant - no effective difference to mean plant

planted <- read.csv("LR16_planted.csv",strip.white=TRUE) #2016 date
planted[planted==""] <- NA   #replace blank with NA# replace whitespace with NAN
planted[planted=="#N/A"] <- NA 
colnames(planted)
summary(planted)

head(planted,n=20)




#exclude group names not in lists
#length(grpdata2$GroupName)
#length(planted$Group.Name)
#length(excluded(grpdata2$GroupName, planted$Group.Name))
#exc2 <- excluded(grpdata2$GroupName, planted$Group.Name)
#grpdata2 = subset(grpdata2, !(grpdata2$GroupName %in% exc2))
#length(grpdata2$GroupName)


#Try to get mean dates by splitting column into two and taking mean
class(da$Maize..First.Farmer.Plant.Date)
foo <- data.frame(do.call('rbind', strsplit(as.character(planted$Maize..First.Farmer.Plant.Date),'-',fixed=TRUE)))
foo2 <- data.frame(do.call('rbind', strsplit(as.character(planted$Maize.Last.Farmer.Plant.Date),'-',fixed=TRUE)))
foo2$X1 <- as.Date(paste("2016 ", foo2$X1,sep=""), format="%Y %d %b")
foo2$X2 <- as.Date(paste("2016 ", foo2$X2,sep=""), format="%Y %d %b")
foo$X1 <- as.Date(paste("2016 ", foo$X1,sep=""), format="%Y %d %b")
foo$X2 <- as.Date(paste("2016 ", foo$X2,sep=""), format="%Y %d %b")
#mean.Date(as.Date(c("01-01-2014", "01-07-2014"), format=c("%m-%d-%Y")))
foo$mudate <- (foo$X1 + ((foo$X2 - foo$X1) / 2))
foo2$mudate <- (foo2$X1 + ((foo2$X2 - foo2$X1) / 2))
planted$first.maize.plant <- foo$mudate
planted$last.maize.plant <- foo2$mudate
planted$full.name <- paste(planted$District,planted$Site,planted$Group.Name)
planted$full.name
#this full name attribute is unique to every GROUP. therefore for site data you will want to summarise by site
#
##############

#make unique names (at least to group level)
data$full.name <- paste(data$DistrictName,data$SiteName,data$GroupName)
data2$full.name <- paste(data2$DistrictName,data2$SiteName,data2$GroupName)

z <- is.na(match(planted$full.name,data2$full.name))
duplicated(planted$full.name)
sum(z, na.rm=TRUE) 

length(data2$full.name)
length(planted$full.name)
planted$full.name

#histo planting
#limits=as.Date(c("2015-11-01","2017-02-01"

p1 <- ggplot(planted, aes(first.maize.plant)) + geom_histogram() + xlab("First plant") + scale_x_date(date_breaks = "2 weeks" )

p2 <- ggplot(planted, aes(last.maize.plant)) + geom_histogram() + xlab("Last plant") + scale_x_date(date_breaks = "2 weeks" )

p3 <- ggplot(planted) + geom_freqpoly(binwidth=8,aes(x=first.maize.plant,colour="First maize planting"),size=2) 

p3 <- p3  + geom_freqpoly(binwidth=8,aes(x=last.maize.plant,colour='Last maize planting'),size=2) + xlab("Date") + scale_x_date(date_breaks = "1 month" )

p1
p2
multiplot(p1, p2, cols=1)
p3


#now lets get mean date
planted$mean.plant <- (planted$first.maize.plant + ((planted$last.maize.plant - planted$first.maize.plant) / 2))
t <- c("first.maize.plant","last.maize.plant","mean.plant","full.name")
tail(planted[t],n=50)

#ets try a regression against planting date for fun
duplicated(planted$full.name)
duplicated(data$full.name)
#data <-  ddply(data, c("full.name"), summarise,  avrep = mean(rep.pc), sd = sd(rep.pc),  semss = sems(rep.pc))
df <- data2
summary(df$rep.pc)
#drop un-summarisables
#df <- df[sapply(df,is.numeric)]
#df$full.name <- data$full.name
#colnames(df)
#d <- !is.na(match(colnames(df),colnames(data)))
#colnames(df[d])

#df <- ddply(data, c("full.name"), summarise)
#df <- df %>% group_by(full.name) %>% summarise_each(funs(mean))
df <- as.data.frame(df)


##################
#Rthonic way to merge
#sanity check
df$fu1 <- df$full.name
planted$fu2 <- planted$full.name
tot <- merge(df,planted,by="full.name")
tot$plant.spread <- tot$last.maize.plant - tot$first.maize.plant
tot$plant.spread <- as.numeric(tot$plant.spread)
summary(tot)
colnames(tot)
summary(tot$Dropped)

#######
tot <-subset(tot, !is.na(plant.spread))
#things to think
#rep.pc is ueless metric
#repyment rate as last-first rep / total credit? or / 

#calc time for rep
tot$LastRepayment <- strptime(tot$LastRepayment,format = '%m/%d/%Y %H:%M:%S')
tot$FirstRepayment <- strptime(tot$FirstRepayment,format = '%m/%d/%Y %H:%M:%S')

tot$LastRepayment <- as.Date(tot$LastRepayment, format="%m/%d/%Y")
tot$FirstRepayment <- as.Date(tot$FirstRepayment, format="%m/%d/%Y")

#look at spread between first and last repayment dates
tot$rep.spread <- tot$LastRepayment - tot$FirstRepayment
tot$rep.spread <- as.numeric(tot$rep.spread)

#corrected mean plant
tot$corr.plant <- tot$mean.plant - min(tot$mean.plant)
summary(tot$corr.plant)
#calc plant spread
tot$rep.rate <- tot$TotalCredit /  tot$rep.spread
tot$rep.rate #sh per day av
#add in vert repayment 2016 data?
#drop rows without a planting date
dim(tot)

dim(tot)
class(tot$plant.spread)
anyNA(tot$plant.spread)
#turn off prompts for next plot

lapply(tot,summary)
dim(tot)
dim(data2)
dim(df) 
dim(planted)

data <- NA
data2 <- NA
df <- NA
planted <- NA

#ADD in lat long from Jan's data set
gps <- read.csv("sitedensity16.csv")
lapply(gps,summary)

colnames(gps)
gps$semi.name <- paste(gps$X2016.district.name,gps$X2016.site.name,sep="_")

tot$semi.name <- paste(tot$District, tot$SiteName)
#match em up, see atrition
z <- match(tot$semi.name, gps$semi.name)
sum(!is.na(z))

#try merge?
#supdat<- merge(totdat,gps,sort=FALSE)
supdat <- merge(tot, gps, by = "semi.name", all = TRUE, sort = FALSE)
head(supdat, n=10)
dim(supdat)

#output a small amount for manual inspection
slim_my_data(supdat,0.005, "testy_sup.csv")

#looks good! that ends the merging of data for 2016
write.csv(supdat, file="2016_Client_plant_gps_full.csv", row.names = FALSE)

#do we want to merge 2015 or 17, do we have the data for that?
stop()




mai.lm1 <- lm(formula = rep.pc ~ mean.plant + District, data = tot)
summary(mai.lm1)
summary(mai.lm1)$r.squared
xyplot(rep.pc ~ mean.plant, data = tot,
       xlab = "mean.plant",
       ylab = "rep pc",
       main = "reg plot"  )


mai.lm2 <- lm(formula = rep.pc ~ plant.spread + District, data = tot)
summary(mai.lm2)
summary(mai.lm2)$r.squared

#mai.lmsite <- lm(formula = rep.pc ~ plant.spread + Site, data = tot)
#summary(mai.lmsite)
#summary(mai.lmsite)$r.squared


xyplot(rep.pc ~ plant.spread, data = tot,
       xlab = "plant spread",
       ylab = "rep pc",
       main = "reg plot"  )
#av.plots(mai.lm2, data=tot) par(ask=F)

confint(mai.lm2,level = 0.95)

anyNA(tot$rep.rate)

mai.lm3 <- lm(formula = rep.rate ~ mean.plant + District, data = tot)
summary(mai.lm3)
summary(mai.lm3)$r.squared

mai.lm4 <- lm(formula = rep.rate ~ plant.spread + District, data = tot)
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

#av.plots(mai.lm1, data=tot) par(ask=F)

#av.plots(glm(partic != "not.work" ~ hincome + children, data=Womenlf, family=binomial)) #for logits

tot$mean.plant

print(paste("time elapsed",proc.time() - ptm))
stop()
