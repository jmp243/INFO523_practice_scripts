# Jung Mee Park
# INFO 523
# HW 6 - Part B and C
# 2021-12-08

# DETECTING FRAUDULENT TRANSACTIONS
###################################################
### Loading the data into R 
###################################################
# load('sales.Rdata')
# BASED ON 2010 AND 2016 UPDATES
# library(DMwR) # not available for this version of R
library(dplyr)
data(sales, package = "DMwR2")

head(sales)

###################################################
### Exploring the data set
###################################################
summary(sales)

nlevels(sales$ID)
nlevels(sales$Prod)

length(which(is.na(sales$Quant) & is.na(sales$Val)))

filter(sales,is.na(Quant),is.na(Val)) # updated in 2016, page 297

sum(is.na(sales$Quant) & is.na(sales$Val))

table(sales$Insp)/nrow(sales)*100

totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS,main='Transactions per salespeople',names.arg='',xlab='Salespeople',ylab='Amount')
barplot(totP,main='Transactions per product',names.arg='',xlab='Products',ylab='Amount')

### updated plot from 2016 book, page 298
library(ggplot2)
ggplot(group_by(sales,ID) %>% summarize(nTrans=n()),aes(x=ID,y=nTrans)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
  xlab("Salesmen") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Salesman")
ggplot(group_by(sales,Prod) %>% summarize(nTrans=n()),aes(x=Prod,y=nTrans)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
  xlab("Product") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Product")
#############
ggplot(group_by(sales,ID) %>% summarize(nTrans=n()),aes(x=ID,y=nTrans)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
  xlab("Product") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Salesman")

ggplot(group_by(sales,Prod) %>% summarize(nTrans=n()),aes(x=Prod,y=nTrans)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
  xlab("Salesmen") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Product")
#######
sales$Uprice <- sales$Val/sales$Quant
### updated in 2016
sales <- mutate(sales, Uprice=Val/Quant) # page 298
##
summary(sales$Uprice)


####################
# code from 2016
####################
prods <- group_by(sales,Prod)
mpProds <- summarize(prods, medianPrice = median(Uprice,na.rm=TRUE))
# bind_cols(mpProds %>% arrange(medianPrice) %>% slice(1:5),
#               mpProds %>% arrange(desc(medianPrice)) %>% slice(1:5))

# Jung Mee's exploration, try a different slice
bind_cols(mpProds %>% arrange(medianPrice) %>% slice(1:6),
          mpProds %>% arrange(desc(medianPrice)) %>% slice(1:6))
# or see what this does
bind_cols(mpProds %>% arrange(desc(medianPrice)) %>% slice(1:6))


# page 300
library(forcats) # also needed ggplot2 but was loaded in earlier
ggplot(filter(sales,Prod %in% c("p3689","p560")),aes(x=fct_drop(Prod),y=Uprice)) +
  geom_boxplot() + scale_y_log10() +
  xlab("") + ylab("log10(UnitPrice)")

# page 301
ids <- group_by(sales,ID)
tvIDs <- summarize(ids,totalVal=sum(Val,na.rm=TRUE))
# bind_cols(tvIDs %>% arrange(totalVal) %>% slice(1:5),
#               tvIDs %>% arrange(desc(totalVal)) %>% slice(1:5))
bind_cols(tvIDs %>% arrange(totalVal) %>% slice(1:6),
          tvIDs %>% arrange(desc(totalVal)) %>% slice(1:6))
# page 302
arrange(tvIDs,desc(totalVal)) %>% slice(1:100) %>%
  summarize(t100=sum(totalVal)) /
  (summarize(tvIDs,sum(totalVal))) * 100
arrange(tvIDs,totalVal) %>% slice(1:2000) %>%
  summarize(b2000=sum(totalVal)) /
  (summarize(tvIDs,sum(totalVal))) * 100

prods <- group_by(sales,Prod)
qtProds <- summarize(prods,totalQty=sum(Quant,na.rm=TRUE))
bind_cols(qtProds %>% arrange(desc(totalQty)) %>% slice(1:5),
              qtProds %>% arrange(totalQty) %>% slice(1:5))

arrange(qtProds,desc(totalQty)) %>% slice(1:100) %>%
  summarize(t100=sum(as.numeric(totalQty))) /
  (summarize(qtProds,sum(as.numeric(totalQty)))) * 100

arrange(qtProds,totalQty) %>% slice(1:4000) %>%
  summarize(b4000=sum(as.numeric(totalQty))) /
  (summarize(qtProds,sum(as.numeric(totalQty)))) * 100

nouts <- function(x) length(boxplot.stats(x)$out)
noutsProds <- summarise(prods,nOut=nouts(Uprice))

arrange(noutsProds,desc(nOut))

# page 304
summarize(noutsProds,totalOuts=sum(nOut))
summarize(noutsProds,totalOuts=sum(nOut))/nrow(sales)*100
###################################################
### Data problems
###################################################
# http://ltorgo.github.io/DMwR2/Rfraud.html
### page 305 in 2016
prop.naQandV <- function(q,v) 100*sum(is.na(q) & is.na(v))/length(q)
summarise(ids,nProbs=prop.naQandV(Quant,Val)) %>% arrange(desc(nProbs))
summarise(prods,nProbs=prop.naQandV(Quant,Val)) %>% arrange(desc(nProbs))

sales <- filter(sales,!(is.na(Quant) & is.na(Val)))
prop.nas <- function(x) 100*sum(is.na(x))/length(x)
summarise(prods,propNA.Q=prop.nas(Quant)) %>% arrange(desc(propNA.Q))

filter(sales, Prod %in% c("p2442","p2443")) %>%
  group_by(Insp) %>% count()

sales <- droplevels(filter(sales,!(Prod %in% c("p2442", "p2443"))))

# page 307
summarise(ids,propNA.Q=prop.nas(Quant)) %>% arrange(desc(propNA.Q)) 
summarise(prods,propNA.V=prop.nas(Val)) %>% arrange(desc(propNA.V))
summarise(ids,propNA.V=prop.nas(Val)) %>% arrange(desc(propNA.V))

# Jung Mee's addition to the code
filter_sales <- sales %>% 
  filter(Insp != 'fraud') 

# from updated Torgo 2016 
tPrice <- filter(sales, Insp != "fraud") %>%
  group_by(Prod) %>%
  summarise(medianPrice = median(Uprice,na.rm=TRUE))

noQuantMedPrices <- filter(sales, is.na(Quant)) %>%
  inner_join(tPrice) %>%
  select(medianPrice)
noValMedPrices <- filter(sales, is.na(Val)) %>%
  inner_join(tPrice) %>%
  select(medianPrice)

noQuant <- which(is.na(sales$Quant))
noVal <- which(is.na(sales$Val))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /noQuantMedPrices)
sales[noVal,'Val'] <- sales[noVal,'Quant'] * noValMedPrices

sales$Uprice <- sales$Val/sales$Quant

# save data
save(sales, file = "salesClean.Rdata") # Torgo 2016

### 6.2.3.2 Few transactions of some products, page 309
### Defining the data mining tasks
ms <- filter(sales,Insp != "fraud") %>%
  group_by(Prod) %>%
  summarize(median=median(Uprice,na.rm=TRUE),
              iqr=IQR(Uprice,na.rm=TRUE),
              nTrans=n(),
              fewTrans=ifelse(nTrans>20,FALSE,TRUE))
ms

## ggplot of transactions, page 310
ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) +
  geom_point() +
  xlab("Median") + ylab("IQR")
ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) +
  geom_point() +
  scale_y_log10() + scale_x_log10() +
  xlab("log(Median)") + ylab("log(IQR)")

# Jung Mee's modification of the graph 
plot_trans <- ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) +
  geom_point() +
  scale_y_log10() + scale_x_log10() +
  xlab("log(Median)") + ylab("log(IQR)")

plot_trans <- plot_trans + labs(title = "Transactions based on median values", 
                                subtitle = "log transformed")
print(plot_trans)

#### Kolmogorov-Smirov test, page 311 ####
# code for normalizing data
ms <- mutate(ms,smedian=scale(median),siqr=scale(iqr))
smalls <- which(ms$fewTrans)
nsmalls <- as.character(ms$Prod[smalls])
similar <- matrix(NA,length(smalls),7,
                      dimnames=list(nsmalls,
                      c("RowSimProd", "ks.stat", "ks.p", "medP", "iqrP", "medS","iqrS")))
xprods <- tapply(sales$Uprice, sales$Prod, list)
for(i in seq_along(smalls)) {
    d <- scale(ms[,c("smedian","siqr")],
                 c(ms$smedian[smalls[i]],ms$siqr[smalls[i]]),
                 FALSE)
    d <- sqrt(drop(d^2 %*% rep(1, ncol(d))))
    stat <- ks.test(xprods[[nsmalls[i]]], xprods[[order(d)[2]]])
    similar[i, ] <- c(order(d)[2], stat$statistic, stat$p.value,
                              ms$median[smalls[i]],ms$iqr[smalls[i]],
                              ms$median[order(d)[2]],ms$iqr[order(d)[2]])
} # There were 50 or more warnings (use warnings() to see the first 50)

#### page 312 
bind_rows(filter(ms,Prod==rownames(similar)[1]),
            ms[similar[1,1],])

nrow(similar[similar[, "ks.p"] >= 0.9, ])
sum(similar[, "ks.p"] >= 0.9)
save(similar, file = "similarProducts.Rdata")

#### 6.3.1.1. Unsupervised Techniques, page 313
#### 6.3.1.2. Supervised Techniques, page 314
#### 6.3.1.3. Semi-Supervised Techniques, page 315

#### 6.3.2.2 Lift Charts and Precision/Recall Curves
# Evaluation Criteria 
library(ROCR)
data("ROCR.simple")
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "prec", "rec")
plot(perf)

PRcurve <- function(preds, trues, ...) {
    require(ROCR, quietly = TRUE)
    pd <- prediction(preds, trues)
    pf <- performance(pd, "prec", "rec")
    pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x))))
    plot(pf, ...)
}


PRcurve(ROCR.simple$predictions, ROCR.simple$labels)
# 
# library(ROCR)
# data(ROCR.simple)
# pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
# perf <- performance(pred, "prec", "rec")
par( mfrow=c(1,2) )
plot(perf)
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)
par( mfrow=c(1,1) )

### page 319 
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift Chart")

CRchart <- function(preds, trues, ...) {
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "rec", "rpp")
  plot(pf, ...)
}

### page 320
CRchart(ROCR.simple$predictions, ROCR.simple$labels,
      main='Cumulative Recall Chart')

### 6.3.2.3 Normalized distance to typical price, page 321
avgNDTP <- function(toInsp,train,stats) {
    if (missing(train) && missing(stats))
      stop('Provide either the training data or the product stats')
    if (missing(stats)) {
      stats <- as.matrix(filter(train,Insp != 'fraud') %>%
                           group_by(Prod) %>%
                           summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                           select(median,iqr))
      rownames(stats) <- levels(train$Prod)
      stats[which(stats[,'iqr']==0),'iqr'] <- stats[which(stats[,'iqr']==0),'median']

  }
  
    return(mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
                  stats[toInsp$Prod,'iqr']))
}

# experimental methodology
evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds,...) # I need to review how ... works
{
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[1:nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}

# obtaining outlier rankings
# unsupervised approaches
BPrule.wf <- function(form,train,test,...) {
    require(dplyr, quietly=TRUE)
    ms <- as.matrix(filter(train,Insp != 'fraud') %>%
                      group_by(Prod) %>%
                      summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                      select(median,iqr))
          rownames(ms) <- levels(train$Prod)
          ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
          ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
          ms[test$Prod,'iqr']
          rankOrder <- order(ORscore,decreasing=TRUE)
          res <- list(testSet=test,rankOrder=rankOrder,
                      probs=matrix(c(ORscore,ifelse(test$Insp=='fraud',1,0)),
                                           ncol=2))
                res
}

#### page 324
globalStats <- as.matrix(filter(sales,Insp != 'fraud') %>%
                             group_by(Prod) %>%
                             summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                             select(median,iqr))
rownames(globalStats) <- levels(sales$Prod)
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- globalStats[which(globalStats[,'iqr']==0),'median']
head(globalStats,3)

#### page 325
library(performanceEstimation) 
bp.res <- performanceEstimation(
    PredTask(Insp ~ ., sales),
    Workflow("BPrule.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                     method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                     evaluator="evalOutlierRanking",
                     evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

#### page 326 
ps.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,1])
ts.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",avg="vertical")
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',avg='vertical')

#### 6.4.1.2 Local Outlier Factors (LOF), page 327
LOF.wf <- function(form, train, test, k, ...) {
  require(DMwR2, quietly=TRUE)
  ntr <- nrow(train)
  all <- as.data.frame(rbind(train,test))
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  
  res <- list(testSet=test,
              rankOrder=order(all[(ntr+1):N,'lof'],decreasing=TRUE),
              probs=as.matrix(cbind(all[(ntr+1):N,'lof'],
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}
### page 328
lof.res <- performanceEstimation(
    PredTask(Insp ~ . , sales),
    Workflow("LOF.wf", k=7),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

# # edit some parameters
lof.res <- performanceEstimation(
  PredTask(Insp ~ . , sales),
  Workflow("LOF.wf", k=7),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=4, hldSz=0.3, strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.2, statsProds=globalStats))
)

summary(lof.res)

### page 329
# fix the function here. 
# ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])  # error
ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])
ts.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
            xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical') # code does not work. ps.lof not found
legend('topright',c('BPrule','LOF'),lty=c(1,2))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
          lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))

# from 2010 base R
ho.ORh <- function(form, train, test, ...) {
  require(dprep,quietly=T)
  ntr <- nrow(train)
  all <- rbind(train,test)
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      outliers.ranking(ups[[u]])$prob.outliers 
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],
                                          decreasing=T),...),
            itInfo=list(preds=all[(ntr+1):N,'lof'],
                        trues=ifelse(test$Insp=='fraud',1,0))
  )
}




### 6.4.1.3 Clustering-Based Outlier Ranks (OR h), page 331
ORh.wf <- function(form, train, test, ...) {
    require(DMwR2, quietly=TRUE)
    ntr <- nrow(train)
      all <- as.data.frame(rbind(train,test))
        N <- nrow(all)
          ups <- split(all$Uprice,all$Prod)
            r <- list(length=ups)
              for(u in seq(along=ups))
              r[[u]] <- if (NROW(ups[[u]]) > 3)
                outliers.ranking(ups[[u]])$prob.outliers
                else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]]))
                else NULL
                all$orh <- vector(length=N)
                split(all$orh,all$Prod) <- r
                all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))] <-
                     SoftMax(all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))])
                res <- list(testSet=test,
                                    rankOrder=order(all[(ntr+1):N,'orh'],decreasing=TRUE),
                                    probs=as.matrix(cbind(all[(ntr+1):N,'orh'],
                                                            ifelse(test$Insp=='fraud',1,0))))
                res
 }

#### page 331
orh.res <- performanceEstimation(
    PredTask(Insp ~ . , sales),
    Workflow("ORh.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                     method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                     evaluator="evalOutlierRanking",
                     evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

# summary(orh.res)

#### page 332
ps.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,1]) # What is this function for?
ts.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1,col='grey', avg='vertical')
legend('topright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,col='grey',avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey')) 
# For some reason the BPrule line is the only one that shows ups. Something is wrong 
# internally with the functions written for the package. 

### 6.4.2 Supervised Approaches
#### 6.4.2.1 The class Imbalance Problem, page 334
library(UBL) # detours in the iris dataset

#### 6.4.2.2 Naive Bayes, page 336
NB.wf <- function(form,train,test,...) {
    require(e1071,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
      data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
        data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
          model <- naiveBayes(Insp ~ .,data, ...)
            preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')], type='raw')
              rankOrder <- order(preds[,'fraud'], decreasing=TRUE)
                rankScore <- preds[,'fraud']
                  res <- list(testSet=test,
                                rankOrder=rankOrder,
                                probs=as.matrix(cbind(rankScore,
                               ifelse(test$Insp=='fraud',1,0))))
                    res
 }

nb.res <- performanceEstimation(
  PredTask(Insp ~ . , sales),
  Workflow("NB.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1,
                                     statsProds=globalStats))
)

summary(nb.res)

### page 337
ps.nb <- sapply(getIterationsInfo(nb.res), function(i) i$probs[,1])
ts.nb <- sapply(getIterationsInfo(nb.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
            xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))
CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
          lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))

#### page 338
NBsm.wf <- function(form,train,test,C.perc="balance",dist="HEOM",...) {
    require(e1071,quietly=TRUE)
    require(UBL,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
      data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
        data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
          newData <- SmoteClassif(Insp ~ .,data,C.perc=C.perc,dist=dist,...)
            model <- naiveBayes(Insp ~ .,newData)
              preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
                rankOrder <- order(preds[,'fraud'],decreasing=T)
                  rankScore <- preds[,'fraud']
                    res <- list(testSet=test,
                                  rankOrder=rankOrder,
                                  probs=as.matrix(cbind(rankScore,
                                                          ifelse(test$Insp=='fraud',1,0))))
                      res
}

####
nbs.res <- performanceEstimation(
    PredTask(Insp ~ ., sales),
    Workflow("NBsm.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                     method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                     evaluator="evalOutlierRanking",
                     evaluator.pars=list(Threshold=0.1,
                                           statsProds=globalStats))
)

summary(nbs.res)

#### page 339
ps.nbs <- sapply(getIterationsInfo(nbs.res), function(i) i$probs[,1])
ts.nbs <- sapply(getIterationsInfo(nbs.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
            xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=2, avg='vertical')
PRcurve(ps.nbs,ts.nbs,add=TRUE,lty=1, col='grey',avg='vertical')
legend('topright',c('NaiveBayes','ORh','smoteNaiveBayes'),lty=c(1,2,1),
           col=c('black','black','grey'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
          lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=2,avg='vertical')
CRchart(ps.nbs,ts.nbs,add=TRUE,lty=1,col='grey',avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','smoteNaiveBayes'),lty=c(1,2,1),
           col=c('black','black','grey'))

#### 6.4.2.3 AdaBoost, page 342
library(RWeka) # I could not load this package in my version of R. 
WOW(AdaBoostM1) # function is part of RWeka

ab.wf <- function(form,train,test,ntrees=100,...) {
    require(RWeka,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
      data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
        data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
          model <- AdaBoostM1(Insp ~ .,data,
                                control=Weka_control(I=ntrees))
            preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                               type='probability')
              rankOrder <- order(preds[,"fraud"],decreasing=TRUE)
                rankScore <- preds[,"fraud"]
                  res <- list(testSet=test,
                                rankOrder=rankOrder,
                                probs=as.matrix(cbind(rankScore,
                                                ifelse(test$Insp=='fraud',1,0))))
                    res
}

### debug ab.res
ab.res <- performanceEstimation(
    PredTask(Insp ~ .,sales),
    Workflow("ab.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                     method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                     evaluator="evalOutlierRanking",
                     evaluator.pars=list(Threshold=0.1,
                                           statsProds=globalStats))
)

#### page 343
ps.ab <- sapply(getIterationsInfo(ab.res), function(i) i$probs[,1])
ts.ab <- sapply(getIterationsInfo(ab.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
            xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1, color='grey', avg='vertical')
PRcurve(ps.ab,ts.ab,add=TRUE,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','AdaBoostM1'),
           lty=c(1,1,2),col=c('black','grey','black'))
CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
          lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.ab,ts.ab,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','AdaBoostM1'),
           lty=c(1,1,2),col=c('black','grey','grey'))

### 6.4.3 Semi-Supervised Approaches, page 345
library(DMwR2)
library(e1071) # another look at the iris dataset

#### page 346
pred.nb <- function(m,d) {
    p <- predict(m,d,type='raw')
      data.frame(cl=colnames(p)[apply(p,1,which.max)],
                   p=apply(p,1,max)
      )
}
nb.st.wf <- function(form,train,test,...) {
  require(e1071,quietly=TRUE)
  require(DMwR2, quietly=TRUE)
  train <- as.data.frame(train[,c('ID','Prod','Uprice','Insp')])
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(form,train,
                     learner='naiveBayes', learner.pars=list(),
                     pred='pred.nb')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='raw')
  
  rankOrder <- order(preds[,'fraud'],decreasing=TRUE)
  rankScore <- preds[,"fraud"]
  
  res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}

### page 347 
nb.st.res <- performanceEstimation(
    PredTask(Insp ~ .,sales),
    Workflow("nb.st.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                     method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                     evaluator="evalOutlierRanking",
                     evaluator.pars=list(Threshold=0.1,
                                        statsProds=globalStats))
)

ps.nb.st <- sapply(getIterationsInfo(nb.st.res), function(i) i$probs[,1])
ts.nb.st <- sapply(getIterationsInfo(nb.st.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1, color='grey', avg='vertical')
PRcurve(ps.nb.st,ts.nb.st,add=TRUE,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.nb.st,ts.nb.st,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','grey'))

### page 348
CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
          lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.nb.st,ts.nb.st,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),
           lty=c(1,1,2),col=c('black','grey','grey'))

### 
pred.ada <- function(m,d) {
    p <- predict(m,d,type='probability')
      data.frame(cl=colnames(p)[apply(p,1,which.max)],
                   p=apply(p,1,max)
      )
}

ab.st.wf <- function(form,train,test,ntrees=100,...) {
    require(RWeka,quietly=TRUE)
    require(DMwR2,quietly=TRUE)
    train <- as.data.frame(train[,c('ID','Prod','Uprice','Insp')])
      train[which(train$Insp == 'unkn'),'Insp'] <- NA
        train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
          model <- SelfTrain(form,train,
                               learner='AdaBoostM1',
                               learner.pars=list(control=Weka_control(I=ntrees)),
                               pred='pred.ada')
            preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                               type='probability')
            rankOrder <- order(preds[,'fraud'],decreasing=T)
              rankScore <- preds[,"fraud"]
              res <- list(testSet=test,
                            rankOrder=rankOrder,
                            probs=as.matrix(cbind(rankScore,
                                            ifelse(test$Insp=='fraud',1,0))))
              res
     }

summary(ab.st.wf)

ab.st.res <- performanceEstimation(
  PredTask(Insp ~ .,sales),
  Workflow("ab.st.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1,
                                     statsProds=globalStats))
)

ps.ab.st <- sapply(getIterationsInfo(ab.st.res), function(i) i$probs[,1]) 
ts.ab.st <- sapply(getIterationsInfo(ab.st.res), function(i) i$probs[,2])

PRcurve(ps.orh,ts.orh,main="PR curve",lty=1,
          xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.ab,ts.ab,add=TRUE,lty=1, color='grey', avg='vertical')
PRcurve(ps.ab.st,ts.ab.st,add=TRUE,lty=2,avg='vertical')
legend('topright',c('ORh','AdaBoostM1','AdaBoostM1-ST'),
           lty=c(1,1,2),col=c('black','grey','black'))
CRchart(ps.orh,ts.orh,main='Cumulative Recall curve',
          lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.ab,ts.ab,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.ab.st,ts.ab.st,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('ORh','AdaBoostM1','AdaBoostM1-ST'),
           lty=c(1,1,2),col=c('black','grey','grey'))
        
# There were a lot of things I could not fix because I didn't understand how these complicated 
# functions were written. If I had more knowledge, I could have fixed the bug in the function(i) i$probs[,1], etc.
# My biggest struggle for getting the code to work was the fact that some packages from 2016
# were not compatible with my current version of R. I am running R version 4.1.1 (2021-08-10). 
# I could not make much headway after line 404. For my paid work, I am currently trying to improve 
# someone else's code in R and having to create a Tableau dashboard with the info. So this was a 
# helpful exercise. 

### Part 3
# 1. Did you find any flaws with their approach? Explain. 
# 

# 2. What aspects were you able to improve? Explain. 

# 3. Did the conclusions change in any way? Please explain. 

# 4. Create a table summarizing the main changes between the original chapter and 
# your new script. 

# 5. Re-assess your position in Part A-5. For instance, were you able to accomplish 
# the changes/improvements you had in mind? 