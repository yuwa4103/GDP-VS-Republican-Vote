getwd()
setwd("D:/R project")
getwd()

case <- read.csv('case01.csv')
case$percentvotes <- case$candidatevotes/case$totalvotes
head(case)
Michigan <- case[case[2]=='Michigan',]
head(Michigan)
Minnesota <- case[case[2]=='Minnesota',]
head(Minnesota)
#select year
Mic12 <-Michigan[Michigan[1]==2012,]
Min12 <- Minnesota[Minnesota[1]==2012,]
Mic16 <- Michigan[Michigan[1]==2016,]
Min16 <- Minnesota[Minnesota[1]==2016,]

#pecent change
Mic16$percentchange <- (Mic16$percentvotes-Mic12$percentvotes)/Mic12$percentvotes
Min16$percentchange <- (Min16$percentvotes-Min12$percentvotes)/Min12$percentvotes
Mic16$percentchange

#Michigan gdb
gdp <- read.csv("lagdp1218.csv", stringsAsFactors = FALSE)
head(gdp)
Micgdp <- (gdp[1278:1360,])
head(Micgdp)
str(Micgdp)
as.numeric(gsub(',','',Micgdp[,2]))
as.numeric(gsub(',','',Micgdp[,5]))

Mic16rep <- Mic16[seq(2,nrow(Mic16),3),]
nrow(Mic16rep)
Mic16rep$gdpchange <- (as.numeric(gsub(',','',Micgdp[,5]))-as.numeric(gsub(',','',Micgdp[,2])))/as.numeric(gsub(',','',Micgdp[,2]))
(as.numeric(gsub(',','',Micgdp[,5]))-as.numeric(gsub(',','',Micgdp[,2])))/as.numeric(gsub(',','',Micgdp[,2]))

Mic16rep

summary(lm(percentchange ~ gdpchange, data=Mic16rep))


# Minnesota gdb

Mingdp <- (gdp[1363:1449,])
head(Mingdp)
Min16rep <- Min16[seq(2,nrow(Min16),3),]
nrow(Min16rep)
Min16rep$gdpchange <- (as.numeric(gsub(',','', Mingdp[,5]))-as.numeric(gsub(',','', Mingdp[,2])))/as.numeric(gsub(',','', Mingdp[,2]))

summary(lm(percentchange ~ gdpchange, data=Min16rep))



Mic16dem <- Mic16[seq(1,nrow(Mic16),3),]
Min16dem <- Min16[seq(1,nrow(Min16),3),]
Mic16dem$gdpchange <- (as.numeric(gsub(',','',Micgdp[,5]))-as.numeric(gsub(',','',Micgdp[,2])))/as.numeric(gsub(',','',Micgdp[,2]))
Min16dem$gdpchange <- (as.numeric(gsub(',','', Mingdp[,5]))-as.numeric(gsub(',','', Mingdp[,2])))/as.numeric(gsub(',','', Mingdp[,2]))


summary(lm(percentchange ~ gdpchange, data=Mic16dem))
summary(lm(percentchange ~ gdpchange, data=Min16dem))


censusone <- read.csv('acs2015_county_data.csv')
censustwo <- read.csv('acs2017_county_data.csv')

#############################################
forreg <- cbind(censusone[2],censusone[3],(censustwo[37]-censusone[37])/censusone[37],(censustwo[8]-censusone[8])/censusone[8],(censustwo[34]-censusone[34])/censusone[34])
Micforreg <- forreg[forreg[1]=='Michigan',]
nrow(Micforreg)
Minforreg <- forreg[forreg[1]=='Minnesota',]


Micfinal <- cbind(Mic16rep,Micforreg)
Minfinal <- cbind(Min16rep,Minforreg)


summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=Micfinal))
summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=Minfinal))

#level, change, list contry for ecah state by size
need <- (censustwo[4:length(censustwo)] - censusone[4:length(censusone)])
censusdiff <- cbind(censustwo[1:3],need)
head(censusdiff)

Micdiff <- censusdiff[censusdiff[2]=='Michigan',]
Mindiff <- censusdiff[censusdiff[2]=='Minnesota',]

Micllvars <- cbind(Mic16rep,Micdiff)
Minllvars <- cbind(Min16rep,Mindiff)
head(Micllvars)
head(Minllvars)

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=Micllvars))
summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=Minllvars))

