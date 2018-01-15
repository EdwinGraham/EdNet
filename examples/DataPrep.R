library(rsai)

train <- fread("../data/train.csv")
test <- fread("../data/test.csv")

dt <- rbindlist(list(train, test), fill=TRUE)
dt[, ':='( AgeUnknown = 1*is.na(Age),
           PClassNorm = (Pclass-mean(dt$Pclass))/sd(dt$Pclass),
           FareUnknown = 1*is.na(Fare),
           SibSpNorm = (SibSp-mean(dt$SibSp))/sd(dt$SibSp),
           ParchNorm = (Parch-mean(dt$Parch))/sd(dt$Parch),
           Female = 1*(Sex=="female"),
           Cherbourg = 1*(Embarked=="C"),
           Queenstown = 1*(Embarked=="Q"),
           AgeEstimated = ifelse(is.na(Age), 0, 1*(Age==floor(Age)+0.5)),
           HadCabin = 1*(Cabin != ""),
           CabinA = 1*(substr(Cabin, 1, 1)=="A"),
           CabinB = 1*(substr(Cabin, 1, 1)=="B"),
           CabinC = 1*(substr(Cabin, 1, 1)=="C"),
           CabinD = 1*(substr(Cabin, 1, 1)=="D"),
           CabinE = 1*(substr(Cabin, 1, 1)=="E"),
           CabinF = 1*(substr(Cabin, 1, 1)=="F"))]

dt[is.na(Age), Age := median(dt$Age, na.rm = TRUE)]
dt[, AgeNorm := (Age-mean(dt$Age))/sd(dt$Age)]

fareByClass <- dt[!is.na(Fare), .(medFare=median(Fare)), by=Pclass]

setkey(dt, Pclass)
setkey(fareByClass, Pclass)

dt <- fareByClass[dt]

dt[is.na(Fare), Fare := medFare]
dt[, FareNorm := (log1p(Fare)-mean(log1p(dt$Fare)))/sd(log1p(dt$Fare))]
setkey(dt, PassengerId)

modelVars <- c("AgeEstimated", "AgeNorm", "AgeUnknown", "CabinA", "CabinB", "CabinC", "CabinD", "CabinE", "CabinF", "Cherbourg",
               "FareNorm", "FareUnknown", "Female", "HadCabin", "ParchNorm", "PClassNorm", "Queenstown", "SibSpNorm")

X <- t(as.matrix(dt[!is.na(Survived), modelVars, with=FALSE]))
X_test <- t(as.matrix(dt[is.na(Survived), modelVars, with=FALSE]))
Y <- t(as.matrix(dt[!is.na(Survived), .(Survived)]))

save(list=c("X", "X_test", "Y"), file="../data/titanicData.RDA")
