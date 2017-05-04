
library(e1071)
library(neuralnet)

rm(list=ls())
## Read in data
stats <- read.csv("NCAA_2016_Team_Stats.csv", header=TRUE)
t.2016 <- read.csv("NCAA_2016_Tournament.csv", header=TRUE)
stats <- data.frame(lapply(stats, as.character), stringsAsFactors=FALSE)
t.2016 <- data.frame(lapply(t.2016, as.character), stringsAsFactors=FALSE)

stats.2015 <- read.csv("NCAA_2015_Team_Stats.csv", header=TRUE)
t.2015 <- read.csv("NCAA_2015_Tournament.csv", header=TRUE)
stats.2015 <- data.frame(lapply(stats.2015, as.character), stringsAsFactors = FALSE)
t.2015 <- data.frame(lapply(t.2015, as.character), stringsAsFactors = FALSE)

stats.2014 <- read.csv("NCAA_2014_Team_Stats.csv", header=TRUE)
t.2014 <- read.csv("NCAA_2014_Tournament.csv", header=TRUE)
stats.2014 <- data.frame(lapply(stats.2014, as.character), stringsAsFactors = FALSE)
t.2014 <- data.frame(lapply(t.2014, as.character), stringsAsFactors = FALSE)
stats.2014 <- stats.2014[,-c(27,28)]


#############################################################################################
#############################################################################################
#############################################################################################
## Read in new data
stats.2017 <- read.csv("NCAA_2017_Team_Stats.csv", header=TRUE)
t.2017 <- read.csv("NCAA_2017_Tournament.csv", header=TRUE)
stats.2017 <- data.frame(lapply(stats.2017, as.character), stringsAsFactors=FALSE)
t.2017 <- data.frame(lapply(t.2017, as.character), stringsAsFactors=FALSE)

## Fix stats.2017.2017 record
# for(i in 1:dim(stats.2017)[1]){
#   stats.2017$Record[i] <- as.numeric(strsplit(stats.2017$Record, "-")[[i]][1])/
#     (as.numeric(strsplit(stats.2017$Record, "-")[[i]][2]) +
#        as.numeric(strsplit(stats.2017$Record, "-")[[i]][1]))
# }

## Create new matrix of data
# t.stats.2017 <- matrix(NA, nrow = dim(t.2017)[1], ncol=(dim(stats.2017)[2]*2+1))
# t.stats.2017[,1] <- t.2017[,2] == t.2017[,4]
# for(i in 1:dim(t.2017)[1]){
#   home.place <- which(stats.2017$Team.Name == t.2017$Team.1[i])
#   opp.place <- which(stats.2017$Team.Name == t.2017$Team.2[i])
#   home.stats <- stats.2017[home.place,-c(2)]
#   opp.stats <- stats.2017[opp.place,-c(2)]
#   t.stats.2017[i,2:51] <- as.character(c(home.stats, opp.stats))
#   t.stats.2017[i,52] <- as.character(t.2017$Team.1[i])
#   t.stats.2017[i,53] <- as.character(t.2017$Team.2[i])
# }
# new.names <- c("Win",paste("Home", names(stats.2017)[-c(2)],sep=" "),
#                paste("Opp", names(stats.2017)[-c(2)],sep=" "),"HomeTeam", "AwayTeam")
# t.stats.2017 <- data.frame(t.stats.2017, stringsAsFactors=FALSE)
# names(t.stats.2017) <- new.names
# t.stats.2017$Win <- as.factor(t.stats.2017$Win)

## Convert columns to numeric, not character
# for(i in 2:(dim(t.stats.2017)[2]-2)){
#   if(!identical(t.stats.2017[1,i], NA)){
#     t.stats.2017[,i] <- as.numeric(t.stats.2017[,i])
#   }
# }
## Convert columns to factor, not character
# t.stats.2017$`Home Conference` <- as.factor(t.stats.2017$`Home Conference`)
# t.stats.2017$`Opp Conference` <- as.factor(t.stats.2017$`Opp Conference`)
# t.stats.2017$`Home Berth.type` <- as.factor(t.stats.2017$`Home Berth.type`)
# t.stats.2017$`Opp Berth.type` <- as.factor(t.stats.2017$`Opp Berth.type`)
# 
# ## Remove conference columns
# drops <- c("Home Conference", "Opp Conference", "Home Berth.type", "Opp Berth.type",
#            "HomeTeam","AwayTeam")
# t.stats.2017 <- t.stats.2017[,!(names(t.stats.2017) %in% drops)]
#############################################################################################
#############################################################################################
#############################################################################################


## Fix stats record
for(i in 1:dim(stats)[1]){
  stats$Record[i] <- as.numeric(strsplit(stats$Record, "-")[[i]][1])/
              (as.numeric(strsplit(stats$Record, "-")[[i]][2]) + 
                 as.numeric(strsplit(stats$Record, "-")[[i]][1]))
  stats.2015$Record[i] <- as.numeric(strsplit(stats.2015$Record, "-")[[i]][1])/
              (as.numeric(strsplit(stats.2015$Record, "-")[[i]][2]) + 
                  as.numeric(strsplit(stats.2015$Record, "-")[[i]][1]))
  stats.2014$Record[i] <- as.numeric(strsplit(stats.2014$Record, "-")[[i]][1])/
              (as.numeric(strsplit(stats.2014$Record, "-")[[i]][2]) + 
                  as.numeric(strsplit(stats.2014$Record, "-")[[i]][1]))
  stats.2017$Record[i] <- as.numeric(strsplit(stats.2017$Record, "-")[[i]][1])/
              (as.numeric(strsplit(stats.2017$Record, "-")[[i]][2]) + 
                   as.numeric(strsplit(stats.2017$Record, "-")[[i]][1]))
}

## Create new matrix of data
t.stats <- matrix(NA, nrow = dim(t.2014)[1]*4, ncol=(dim(stats)[2]*2+1))
#t.stats[,1] <- t.2016[,2] == t.2016[,4]
j <- 1
k <- 1
l <- 1
for(i in 1:(dim(t.2016)[1]*4)){
  if(i <= dim(t.2016)[1]){
    home.place <- which(stats$Team.Name == t.2016$Team.1[i])
    opp.place <- which(stats$Team.Name == t.2016$Team.2[i])
    home.stats <- stats[home.place,-c(2)]
    opp.stats <- stats[opp.place,-c(2)]
    t.stats[i,1] <- t.2016[i,2] == t.2016[i,4]
    t.stats[i,2:51] <- as.character(c(home.stats, opp.stats))
    t.stats[i,52] <- as.character(t.2016$Team.1[i])
    t.stats[i,53] <- as.character(t.2016$Team.2[i])
  }else if(i <= (dim(t.2016)[1]*2)){
    home.place <- which(stats.2015$Team.Name == t.2015$Team.1[j])
    opp.place <- which(stats.2015$Team.Name == t.2015$Team.2[j])
    home.stats <- stats.2015[home.place,-c(2)]
    opp.stats <- stats.2015[opp.place,-c(2)]
    t.stats[i,1] <- t.2015[j,2] == t.2015[j,4]
    t.stats[i,2:51] <- as.character(c(home.stats, opp.stats))
    t.stats[i,52] <- as.character(t.2015$Team.1[j])
    t.stats[i,53] <- as.character(t.2015$Team.2[j])
    j <- j + 1
  }else if(i <= (dim(t.2016)[1]*3)){
    home.place <- which(stats.2014$Team.Name == t.2014$Team.1[k])
    opp.place <- which(stats.2014$Team.Name == t.2014$Team.2[k])
    home.stats <- stats.2014[home.place,-c(2)]
    opp.stats <- stats.2014[opp.place,-c(2)]
    t.stats[i,1] <- t.2014[k,2] == t.2014[k,4]
    t.stats[i,2:51] <- as.character(c(home.stats, opp.stats))
    t.stats[i,52] <- as.character(t.2014$Team.1[k])
    t.stats[i,53] <- as.character(t.2014$Team.2[k])
    k <- k + 1
  }else{
    home.place <- which(stats.2017$Team.Name == t.2017$Team.1[l])
    opp.place <- which(stats.2017$Team.Name == t.2017$Team.2[l])
    home.stats <- stats.2017[home.place,-c(2)]
    opp.stats <- stats.2017[opp.place,-c(2)]
    t.stats[i,1] <- t.2017[l,2] == t.2017[l,4]
    t.stats[i,2:51] <- as.character(c(home.stats, opp.stats))
    t.stats[i,52] <- as.character(t.2017$Team.1[l])
    t.stats[i,53] <- as.character(t.2017$Team.2[l])
    l <- l + 1
  }
}
new.names <- c("Win",paste("Home", names(stats)[-c(2)],sep=" "),paste("Opp", names(stats)[-c(2)],sep=" "),"HomeTeam", "AwayTeam")
t.stats <- data.frame(t.stats, stringsAsFactors=FALSE)
names(t.stats) <- new.names
t.stats$Win <- as.factor(t.stats$Win)


## Convert columns to numeric, not character
for(i in 2:(dim(t.stats)[2]-2)){
  if(!identical(t.stats[1,i], NA)){
    t.stats[,i] <- as.numeric(t.stats[,i])
  }
}

## Convert columns to factor, not character
# t.stats$`Home Conference` <- as.factor(t.stats$`Home Conference`)
# t.stats$`Opp Conference` <- as.factor(t.stats$`Opp Conference`)
t.stats$`Home Berth.type` <- as.factor(t.stats$`Home Berth.type`)
t.stats$`Opp Berth.type` <- as.factor(t.stats$`Opp Berth.type`)

## Remove conference columns
drops <- c("Home Conference", "Opp Conference", "Home Berth.type", "Opp Berth.type", "HomeTeam", "AwayTeam")
t.stats <- t.stats[,!(names(t.stats) %in% drops)]


## Look at colinearity
corrplot.mixed(cor(t.stats[c(2:24)]), upper = "ellipse")


## Standardize data
t.stats <- scale(t.stats) # Some columns are not numeric and thus can't be scaled.



## Look at wins against Field goal pct

win.vec <- t.stats$Win == TRUE
plot(t.stats$`Opp Field.Goals.Pct`~t.stats$`Home Field.Goals.Pct`, col="white",
      xlab="Home Team Field Goal Percent", ylab="Opponent Field Goal Percent",
      main="Tournament Results")
points(t.stats$`Opp Field.Goals.Pct`[win.vec]~t.stats$`Home Field.Goals.Pct`[win.vec],
       pch=16, col="red")
points(t.stats$`Opp Field.Goals.Pct`[!win.vec]~t.stats$`Home Field.Goals.Pct`[!win.vec],
       pch=18, col="blue")
legend("topright", c("Win","Loss"), pch=c(16,18), col=c("red","blue"))
# 
# ###########################################################################
# ## PCA
# #################
# covariates <- t.stats[,-1]
# 
# Z = scale(covariates)
# 
# eg = eigen(1/(dim(Z)-1)*t(Z)%*%Z) # squares of sv$d
# eg$values
# #condition number:
# cn.alt = sqrt(max(eg$values)/abs(min(eg$values)))
# cn.alt
# 
# 
# ######################################################################################
# # PCA/PCR
# 
# pca.calc = prcomp(Z, scale = TRUE)
# 
# summary(pca.calc)
# PoV <- pca.calc$sdev^2/sum(pca.calc$sdev^2)
# 
# barplot(PoV, space=0, names.arg=seq(1:length(PoV)), main="Proportion of Variance", xlab="Principle Component",
#         ylab="Proportion of Variance")
# abline(v=11, col="red", lwd=2, lty=2)
# V = pca.calc$rotation #eigenvectors
# 
# C = Z%*%V #matrix of principal components
# 
# 
# #Note that the variance the third PC (C_3) is close to zero (lambda_3 = 0.0026).
# #Very low variance means that C_3 is approximately constant.
# #So C_3 = v_{1,3}*Z_1 + v_{2,3}Z_2 + v_{3,3}*Z_3 \approx 0.
# #Noting that v_{2,3} \approx 0, we get that Z_1 \approx Z_3, i.e.,
# #Z_1 is highly correlated with Z_3. Of course, in other situations,
# #similar reasoning would yield more complicated relationships (MULTIcollinearity).
# 
# ### Reduce the C
# C <- C[,c(1:11)]
# 
# stats.pc = as.data.frame(cbind(t.stats$Win, C)) #construct new "data" matrix with PCs
# 
# # theta = V%*%french2pc.lm$coeff[-1] #yields coefficients with respect to centered model
# # theta
# # coefficients(lm(scale(french2$IMPORT) ~ as.matrix(X))) #check computation of theta...
# # 
# # beta = sd(french2$IMPORT)/sd(french2$DOPROD)*theta[1] #yields DPROD coefficient in original units
# # beta
# # summary(french2.lm) #check...
# # 
# # #Now, since C_3 \approx 0 (equivalently, Z_1 \approx Z_3), we could drop it from our regression...
# # 
# # french2pc2.lm = lm(V1 ~ PC1 + PC2, data = french2pc)
# # summary(french2pc2.lm)
# # 
# # theta = V[,-3]%*%french2pc2.lm$coeff[-1] #yields coefficients with respect to centered model
# # theta
# # coefficients(lm(scale(french2$IMPORT) ~ as.matrix(X))) #check...
# 




############################################################################
## superpc (supervised principal component analysis)
## http://statweb.stanford.edu/~tibs/superpc/tutorial.html
#########################

## install.packages("superpc")
library(superpc)
y <- vector(length=length(t.stats$Win))
for(i in 1:length(t.stats$Win)){
  if(t.stats$Win[i] == "TRUE"){
    y[i] <- 1
  }else{
    y[i] <- 0
  }
}

split.spot <- dim(t.2014)[1]*3
mean.vec <- vector(length=dim(t.stats)[2])
sd.vec <- vector(length=dim(t.stats)[2])
train.dat <- matrix(nrow=split.spot,ncol=dim(t.stats)[2]-1)
test.dat <- matrix(nrow=dim(t.stats)[1]-split.spot,ncol=dim(t.stats)[2]-1)
for(i in 2:(dim(t.stats)[2])){
  mean.vec[i] <- mean(t.stats[c(1:split.spot),i])
  sd.vec[i] <- sd(t.stats[c(1:split.spot),i])
  train.dat[,i-1] <- (t.stats[c(1:split.spot),i]-mean.vec[i])/sd.vec[i]
}
for(i in 2:(dim(t.stats)[2])){
  test.dat[,i-1] <- (t.stats[-c(1:split.spot),i]-mean.vec[i])/sd.vec[i]
}
# scaled.stats <- scale(t.stats[,-1])
# train.dat <- scaled.stats[c(1:split.spot),]
# test.dat <- scaled.stats[-c(1:split.spot),]

y.train <- y[c(1:split.spot)]
y.test <- y[-c(1:split.spot)]
#featurenames <- paste("feature", as.character(1:(dim(t.stats)[2]-1)),sep="")
featurenames <- names(t.stats)[-1]
dat <- list(x=scale(t(data.matrix(train.dat))),y=y.train, featurenames=featurenames)


train.obj <- superpc.train(dat, type="regression")

max.scores <- vector(length=100)
for(i in 1:100){
  cv.obj <- superpc.cv(train.obj, dat)
  col.means <- colMeans(cv.obj$scor)
  max.scores[i] <- cv.obj$thresholds[which.max(cv.obj$scor[1,])]
}

superpc.plotcv(cv.obj)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
thres.use <- Mode(max.scores)

## create testing data
dat.test <- list(x=t(data.matrix(test.dat)),y=y.test, featurenames=featurenames)

lrtest.obj <- superpc.lrtest.curv(train.obj, dat, dat.test)
superpc.plot.lrtest(lrtest.obj)

fit.cts <- superpc.predict(train.obj, dat, dat.test, threshold=thres.use,
                           n.components=1, prediction.type="continuous")
superpc.fit.to.outcome(train.obj, dat.test, fit.cts$v.pred)


fit.red <- superpc.predict.red(train.obj, dat, dat.test, threshold=thres.use)

fit.redcv <- superpc.predict.red.cv(fit.red, cv.obj, dat, threshold=thres.use)

superpc.plotred.lrtest(fit.redcv)

output.features <- superpc.listfeatures(dat.test, train.obj, fit.red)
output.features
#names <- c("Opp Field Goals Made", "Opp Number of Points", "Opp Field Goals Atts", "Opp Number of Rebounds",
#           "Home Field Goals Made","Number of Points", "Home Field Goal Atts")
names <- output.features[,3]
df <- data.frame(Variable=names, Importance=as.numeric(output.features[,1]))
df$Variable <- factor(df$Variable, levels=names) # Still need to fix order
ggplot(df, aes(x=Variable,y=Importance)) + labs(title="Supervised PC",x="Most Important Variables",y="Importance Score") +
    theme(axis.text.x=element_text(angle=45,hjust=1)) + geom_col()
#barchart(Score~test, data=df, origin=0)


############################################################################
############################################################################


colnames(train.dat) <- names(t.stats)[-1]
colnames(test.dat) <- names(t.stats)[-1]
train.stats <- data.frame(train.dat[,output.features[,3]])
train.stats$Win <- t.stats$Win[1:split.spot]
test.stats <- data.frame(test.dat[,output.features[,3]])
test.stats$Win <- t.stats$Win[-c(1:split.spot)]


train.dat <- data.frame(train.dat)
train.dat$Win <- t.stats$Win[1:split.spot]
test.dat <- data.frame(test.dat)
test.dat$Win <- t.stats$Win[-c(1:split.spot)]

###############################################################################

##################################
## Look at different SVMs
##################################

## Linear
lin.svm <- svm(Win~.,data=train.stats,kernel="linear",cost=1)
tune.lin <- tune(svm,Win~.,data=train.stats,kernel="linear",
                 ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svc <- tune.lin$best.model
fit.svc

## Polynomial deg 2
tune.poly2 <- tune(svm,Win~.,data=train.stats,kernel="polynomial",degree=2,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly2 <- tune.poly2$best.model
fit.poly2

## Polynomial deg 3
tune.poly3 <- tune(svm,Win~.,data=train.stats,kernel="polynomial",degree=3,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly3 <- tune.poly3$best.model
fit.poly3

## Polynomial deg 4
tune.poly4 <- tune(svm,Win~.,data=train.stats,kernel="polynomial",degree=4,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly4 <- tune.poly4$best.model
fit.poly4


## Polynomial 5
tune.poly5 <- tune(svm,Win~.,data=train.stats,kernel="polynomial",degree=5,
                 ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly5 <- tune.poly5$best.model
fit.poly5

## Radial
rad.svm <- svm(Win~.,data=train.stats,kernel="radial",cost=1)
tune.rad <- tune(svm,Win~.,data=train.stats,kernel="radial",
                 ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.rad <- tune.rad$best.model

fit.rad


## Sigmoid
tune.sig <- tune(svm,Win~.,data=train.stats,kernel="sigmoid",
                 ranges=list(cost=seq(0.001,5,length.out=100)))
fit.sig <- tune.sig$best.model
fit.sig

## k-nn

knn.scores <- vector(length=100)
for(i in 1:100){
  self.knn <- knn(train=train.stats[,!names(train.stats) %in% "Win"],
                  test=train.stats[,!names(train.stats) %in% "Win"], cl=train.stats$Win, k=i)
  tab <- table(train.stats$Win, self.knn)
  tab
  knn.scores[i] <- (tab[1,1]+tab[2,2])/dim(train.stats)[1]
}
knn.use <- which(max(knn.scores[-1])==knn.scores[-1])+1
self.knn <- knn(train=train.stats[,!names(train.stats) %in% "Win"],
                test=train.stats[,!names(train.stats) %in% "Win"], cl=train.stats$Win, k=3)

test.knn <- knn(train=train.stats[,!names(train.stats) %in% "Win"],
                test=test.stats[,!names(train.stats) %in% "Win"], cl=train.stats$Win, k=3)
tab <- table(test.stats$Win, test.knn)
tab

# knn.scores <- vector(length=100)
# for(i in 1:100){
#   self.knn <- knn(train=train.stats[,!names(train.stats) %in% "Win"],
#                   test=test.stats[,!names(test.stats) %in% "Win"], cl=train.stats$Win, k=i)
#   tab <- table(test.stats$Win, self.knn)
#   tab
#   knn.scores[i] <- (tab[1,1]+tab[2,2])/dim(test.stats)[1]
# }
# tab <- table(train.stats$Win, self.knn)
# tab
# (tab[1,1]+tab[2,2])/dim(train.stats)[1]
# 
# fit.knn <- knn(train=train.stats[,!names(train.stats) %in% "Win"],
#                test=test.stats[,!names(test.stats) %in% "Win"], cl=train.stats$Win, k=3)

## Neural Net

# m <- model.matrix(
#     ~Win + `Home Overall.rank` + `Home Record` + `Opp Overall.rank`, data=t.stats
# )
# svm.nn <- neuralnet(WinTRUE ~., data=m, hidden=3)
# #svm.nn <- neuralnet(Win~t.stats$`Home Overall.rank`, data=t.stats,hidden=3)



#############################################################
## Look at different SVMs with ALL COVARIATES
#############################################################

## Linear
lin.svm <- svm(Win~.,data=train.dat,kernel="linear",cost=1)
tune.lin <- tune(svm,Win~.,data=train.dat,kernel="linear",
                 ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svc <- tune.lin$best.model
fit.svc

## Polynomial deg 2
tune.poly2 <- tune(svm,Win~.,data=train.dat,kernel="polynomial",degree=2,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly2 <- tune.poly2$best.model
fit.poly2

## Polynomial deg 3
tune.poly3 <- tune(svm,Win~.,data=train.dat,kernel="polynomial",degree=3,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly3 <- tune.poly3$best.model
fit.poly3

## Polynomial deg 4
tune.poly4 <- tune(svm,Win~.,data=train.dat,kernel="polynomial",degree=4,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly4 <- tune.poly4$best.model
fit.poly4


## Polynomial 5
tune.poly5 <- tune(svm,Win~.,data=train.dat,kernel="polynomial",degree=5,
                   ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.poly5 <- tune.poly5$best.model
fit.poly5

## Radial
rad.svm <- svm(Win~.,data=train.dat,kernel="radial",cost=1)
tune.rad <- tune(svm,Win~.,data=train.dat,kernel="radial",
                 ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
fit.rad <- tune.rad$best.model

fit.rad


## Sigmoid
tune.sig <- tune(svm,Win~.,data=train.dat,kernel="sigmoid",
                 ranges=list(cost=seq(0.001,5,length.out=100)))
fit.sig <- tune.sig$best.model
fit.sig

## k-nn

knn.scores <- vector(length=100)
for(i in 1:100){
  self.knn <- knn(train=train.dat[,!names(train.dat) %in% "Win"],
                  test=train.dat[,!names(train.dat) %in% "Win"], cl=train.dat$Win, k=i)
  tab <- table(train.dat$Win, self.knn)
  tab
  knn.scores[i] <- (tab[1,1]+tab[2,2])/dim(train.dat)[1]
}
self.knn <- knn(train=train.dat[,!names(train.dat) %in% "Win"],
                test=train.dat[,!names(train.dat) %in% "Win"], cl=train.dat$Win, k=3)


test.knn <- knn(train=train.dat[,!names(train.dat) %in% "Win"],
                test=test.dat[,!names(test.dat) %in% "Win"], cl=train.stats$Win, k=3)
tab <- table(test.stats$Win, test.knn)
tab


# 
# ################################################
# ## SVM with PCA
# ################################################
# 
# ## Linear
# lin.svm <- svm(Win~.,data=stats.pc,kernel="linear",cost=1)
# tune.lin <- tune(svm,V1~.,data=stats.pc,kernel="linear",
#                  ranges=list(cost=seq(0.01,5,length.out=100)))
# fit.svc <- tune.lin$best.model
# fit.svc
# 
# ## Polynomial deg 2
# tune.poly2 <- tune(svm,V1~.,data=stats.pc,kernel="polynomial",degree=2,
#                    ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
# fit.poly2 <- tune.poly2$best.model
# fit.poly2
# 
# ## Polynomial deg 3
# tune.poly3 <- tune(svm,V1~.,data=stats.pc,kernel="polynomial",degree=3,
#                    ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
# fit.poly3 <- tune.poly3$best.model
# fit.poly3
# 
# ## Polynomial deg 4
# tune.poly4 <- tune(svm,V1~.,data=stats.pc,kernel="polynomial",degree=4,
#                    ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
# fit.poly4 <- tune.poly4$best.model
# fit.poly4
# 
# 
# ## Polynomial 5
# tune.poly5 <- tune(svm,V1~.,data=stats.pc,kernel="polynomial",degree=5,
#                    ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
# fit.poly5 <- tune.poly5$best.model
# fit.poly5
# 
# ## Radial
# rad.svm <- svm(V1~.,data=stats.pc,kernel="radial",cost=1)
# tune.rad <- tune(svm,V1~.,data=stats.pc,kernel="radial",
#                  ranges=list(cost=seq(0.01,5,length.out=100)),probability=TRUE)
# fit.rad <- tune.rad$best.model
# 
# fit.rad
# 
# 
# ## Sigmoid
# tune.sig <- tune(svm,V1~.,data=stats.pc,kernel="sigmoid",
#                  ranges=list(cost=seq(0.001,5,length.out=100)))
# fit.sig <- tune.sig$best.model
# fit.sig
# 
# 
# 
# 







##############################################################
## Predict using 2017 Data
##############################################################





# new.data <- predict(pca.calc, newdata=scale(t.stats.2017[-1]))
# new.data2 <- scale(t.stats.2017[,-1], pca$center, pca$scale) %*% pca$rotation ## Same thing as above,
    # but without the scale()
# new.data.trim <- new.data[,1:11]

#################################
## Choose which svm
#################################

# Linear
tab <- table(true=train.stats$Win,pred=predict(fit.svc,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]

# Poly 2
tab <- table(true=train.stats$Win,pred=predict(fit.poly2,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]

# Poly 3
tab <- table(true=train.stats$Win,pred=predict(fit.poly3,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]

# Poly 4
tab <- table(true=train.stats$Win,pred=predict(fit.poly4,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]

# Poly 5
tab <- table(true=train.stats$Win,pred=predict(fit.poly5,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]

# rad
tab <- table(true=train.stats$Win,pred=predict(fit.rad,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]

# sig
tab <- table(true=train.stats$Win,pred=predict(fit.sig,newdata=train.stats))
tab
(tab[1,1]+tab[2,2])/dim(train.stats)[1]






##
## Testing set performance
##

# Linear
tab <- table(true=test.stats$Win,pred=predict(fit.svc,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# Poly 2
tab <- table(true=test.stats$Win,pred=predict(fit.poly2,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# Poly 3
tab <- table(true=test.stats$Win,pred=predict(fit.poly3,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# Poly 4
tab <- table(true=test.stats$Win,pred=predict(fit.poly4,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# Poly 5
tab <- table(true=test.stats$Win,pred=predict(fit.poly5,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# rad
tab <- table(true=test.stats$Win,pred=predict(fit.rad,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# sig
tab <- table(true=test.stats$Win,pred=predict(fit.sig,newdata=test.stats))
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]

# k-nn
tab <- table(test.stats$Win, fit.knn)
tab
(tab[1,1]+tab[2,2])/dim(test.stats)[1]



predictions <- predict(fit.svc,newdata=test.stats)
winner.vec <- vector(length=length(predictions))
loser.vec <- vector(length=length(predictions))
for(i in 1:length(t.2016$Team.1)){
  if(predictions[i] == FALSE){
    winner.vec[i] <- t.2017$Team.2[i]
    loser.vec[i] <- t.2017$Team.1[i]
  }else{
    winner.vec[i] <- t.2017$Team.1[i]
    loser.vec[i] <- t.2017$Team.2[i]
  }
}
# cbind(predict(fit.svc,newdata=test.stats),t.2017$Team.1,t.2017$Team.2)
cbind(winner.vec, loser.vec)



#################################
## Simulate a bracket
#################################

# upset.score = 0
# for(i in 1:length(test.stats$Win)){
#   if((test.stats$Home.Overall.rank[i] > test.stats$Opp.Overall.rank[i]) &(test.stats$Win[i] == "FALSE")){
#     print("-------------------------")
#     print(t.2017$Team.1[i])
#     print(t.2017$Team.2[i])
#     print("-------------------------")
#     upset.score <- upset.score + 1
#   }
# }
# upset.score
# (length(test.stats$Win)-upset.score)/length(test.stats$Win)





