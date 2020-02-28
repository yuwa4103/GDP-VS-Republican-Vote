getwd()
setwd("D:/R project")
getwd()
case <- read.csv('case01.csv')
gdp <- read.csv("lagdp1218.csv", stringsAsFactors = FALSE)
censusone <- read.csv('acs2015_county_data.csv')
censustwo <- read.csv('acs2017_county_data.csv')

#Colorado,Florida,Iowa
case$percentvotes <- case$candidatevotes/case$totalvotes

head(case)
colorado <- case[case[2]=='Colorado',]
head(colorado)
florida <- case[case[2]=='Florida',]
iowa <- case[case[2]=='Iowa',]
colorado12 <- colorado[colorado[1]==2012,]
florida12 <- florida[florida[1]==2012,]
iowa12 <- iowa[iowa[1]==2012,]
colorado16 <- colorado[colorado[1]==2016,]
florida16 <- florida[florida[1]==2016,]
iowa16 <- iowa[iowa[1]==2016,]
colorado16$percentchange <- (colorado16$percentvotes-colorado12$percentvotes)/colorado12$percentvotes
iowa16$percentchange <- (iowa16$percentvotes-iowa12$percentvotes)/iowa12$percentvotes
florida16$percentchange <- (florida16$percentvotes-florida12$percentvotes)/florida12$percentvotes
florida16rep <- florida16[seq(2,nrow(florida16),3),]
nrow(florida16rep)
head(gdp)
flgdp <- (gdp[342:408,])
str(flgdp)
as.numeric(gsub(',','',flgdp[,2]))
as.numeric(gsub(',','',flgdp[,5]))

florida16rep$gdpchange <- (as.numeric(gsub(',','',flgdp[,5]))-as.numeric(gsub(',','',flgdp[,2])))/as.numeric(gsub(',','',flgdp[,2]))
(as.numeric(gsub(',','',flgdp[,5]))-as.numeric(gsub(',','',flgdp[,2])))/as.numeric(gsub(',','',flgdp[,2]))

florida16rep

summary(lm(percentchange ~ gdpchange, data=florida16rep))

iowagdp <- (gdp[822:920,])
iowagdp
iowa16rep <- iowa16[seq(2,nrow(iowa16),3),]
iowa16rep$gdpchange <- (as.numeric(gsub(',','', iowagdp[,5]))-as.numeric(gsub(',','', iowagdp[,2])))/as.numeric(gsub(',','', iowagdp[,2]))

summary(lm(percentchange ~ gdpchange, data=iowa16rep))

cologdp <- (gdp[259:322,])

colo16rep <- colorado16[seq(2,nrow(colorado16),3),]
colo16rep$gdpchange <- (as.numeric(gsub(',','', cologdp[,5]))-as.numeric(gsub(',','', cologdp[,2])))/as.numeric(gsub(',','', cologdp[,2]))

summary(lm(percentchange ~ gdpchange, data=colo16rep))

florida16dem <- florida16[seq(1,nrow(florida16),3),]
iowa16dem <- iowa16[seq(1,nrow(iowa16),3),]
colo16dem <- colorado16[seq(1,nrow(colorado16),3),]
florida16dem$gdpchange <- (as.numeric(gsub(',','',flgdp[,5]))-as.numeric(gsub(',','',flgdp[,2])))/as.numeric(gsub(',','',flgdp[,2]))
iowa16dem$gdpchange <- (as.numeric(gsub(',','', iowagdp[,5]))-as.numeric(gsub(',','', iowagdp[,2])))/as.numeric(gsub(',','', iowagdp[,2]))
colo16dem$gdpchange <- (as.numeric(gsub(',','', cologdp[,5]))-as.numeric(gsub(',','', cologdp[,2])))/as.numeric(gsub(',','', cologdp[,2]))

summary(lm(percentchange ~ gdpchange, data=florida16dem))
summary(lm(percentchange ~ gdpchange, data=iowa16dem))
summary(lm(percentchange ~ gdpchange, data=colo16dem))


forreg <- cbind(censusone[2],censusone[3],(censustwo[37]-censusone[37])/censusone[37],(censustwo[8]-censusone[8])/censusone[8],(censustwo[34]-censusone[34])/censusone[34])
head(forreg)
coloforreg <- forreg[forreg[1]=='Colorado',]
iowaforreg <- forreg[forreg[1]=='Iowa',]
floridaforreg <- forreg[forreg[1]=='Florida',]

colofinal <- cbind(colo16rep,coloforreg)
iowafinal <- cbind(iowa16rep,iowaforreg)
floridafinal <- cbind(florida16rep,floridaforreg)

head(colofinal)

summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=colofinal))

summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=iowafinal))

summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=floridafinal))


need <- (censustwo[5:length(censustwo)] - censusone[5:length(censusone)])
need$TotalPoppercentchange <- (censustwo$TotalPop-censusone$TotalPop)/censusone$TotalPop

censusdiff <- cbind(censustwo[1:4],need)
head(censusdiff)

colodiff <- censusdiff[censusdiff[2]=='Colorado',]
iowadiff <- censusdiff[censusdiff[2]=='Iowa',]
floridadiff <- censusdiff[censusdiff[2]=='Florida',]

coloallvars <- cbind(colo16rep,colodiff)
iowaallvars <- cbind(iowa16rep,iowadiff)
floridaallvars <- cbind(florida16rep,floridadiff)

head(coloallvars)
summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=coloallvars))

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=iowaallvars))

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=floridaallvars))


#Michigan,Minnesota
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



#############################################
forreg <- cbind(censusone[2],censusone[3],(censustwo[37]-censusone[37])/censusone[37],(censustwo[8]-censusone[8])/censusone[8],(censustwo[34]-censusone[34])/censusone[34])
Micforreg <- forreg[forreg[1]=='Michigan',]
nrow(Micforreg)
Minforreg <- forreg[forreg[1]=='Minnesota',]


Micfinal <- cbind(Mic16rep,Micforreg)
Minfinal <- cbind(Min16rep,Minforreg)


summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=Micfinal))
summary(lm(percentchange ~ gdpchange + Unemployment + White + PublicWork, data=Minfinal))


Micdiff <- censusdiff[censusdiff[2]=='Michigan',]
Mindiff <- censusdiff[censusdiff[2]=='Minnesota',]

Micallvars <- cbind(Mic16rep,Micdiff)
Minallvars <- cbind(Min16rep,Mindiff)


summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=Micallvars))

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=Minallvars))



#ohio,pennsyLvania,Nevada
case$percentvotes <- case$candidatevotes/case$totalvotes
head(case)
ohio <- case[case[2]=='Ohio',]
head(ohio)
pennsylvania <- case[case[2]=='Pennsylvania',]
nevada <- case[case[2]=='Nevada',]
ohio12 <- ohio[ohio[1]==2012,]
pennsylvania12 <- pennsylvania[pennsylvania[1]==2012,]
nevada12 <- nevada[nevada[1]==2012,]
ohio16 <- ohio[ohio[1]==2016,]
pennsylvania16 <- pennsylvania[pennsylvania[1]==2016,]
nevada16 <- nevada[nevada[1]==2016,]
ohio16$percentchange <- (ohio16$percentvotes-ohio12$percentvotes)/ohio12$percentvotes
pennsylvania16$percentchange <- (pennsylvania16$percentvotes-pennsylvania12$percentvotes)/pennsylvania12$percentvotes
nevada16$percentchange <- (nevada16$percentvotes-nevada12$percentvotes)/nevada12$percentvotes
nevada16rep <- nevada16[seq(2,nrow(nevada16),3),]
nrow(nevada16rep)

head(gdp)
nevadagdp <- (gdp[1806:1822,])
str(nevadagdp)
as.numeric(gsub(',','',nevadagdp[,2]))
as.numeric(gsub(',','',nevadagdp[,5]))

nevada16rep$gdpchange <- (as.numeric(gsub(',','',nevadagdp[,5]))-as.numeric(gsub(',','',nevadagdp[,2])))/as.numeric(gsub(',','',nevadagdp[,2]))
(as.numeric(gsub(',','',nevadagdp[,5]))-as.numeric(gsub(',','',nevadagdp[,2])))/as.numeric(gsub(',','',nevadagdp[,2]))
nevada16rep

pennsylvaniagdp <- (gdp[2323:2389,])
pennsylvaniagdp
pennsylvania16rep <- pennsylvania16[seq(2,nrow(pennsylvania16),3),]
pennsylvania16rep$gdpchange <- (as.numeric(gsub(',','', pennsylvaniagdp[,5]))-as.numeric(gsub(',','', pennsylvaniagdp[,2])))/as.numeric(gsub(',','', pennsylvaniagdp[,2]))

ohiogdp <- (gdp[2116:2203,])
nrow(ohio16rep)
ohio16rep <- ohio16[seq(2,nrow(ohio16),3),]
ohio16rep$gdpchange <- (as.numeric(gsub(',','', ohiogdp[,5]))-as.numeric(gsub(',','', ohiogdp[,2])))/as.numeric(gsub(',','', ohiogdp[,2]))

nevadadiff <- censusdiff[censusdiff[2]=='Nevada',]
penndiff <- censusdiff[censusdiff[2]=='Pennsylvania',]
ohiodiff <- censusdiff[censusdiff[2]=='Ohio',]

nevadaallvars <- cbind(nevada16rep, nevadadiff)
pennallvars <- cbind(pennsylvania16rep, penndiff)
ohioallvars <- cbind(ohio16rep, ohiodiff)

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=nevadaallvars))

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=pennallvars))

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=ohioallvars))







#New Hampshire, North Carolina
newhampshire <-case[case[2]=='New Hampshire',]
head(newhampshire)

northcarolina <- case[case[2]=='North Carolina',]
head(northcarolina)

newhampshire12 <- newhampshire[newhampshire[1]==2012,]
northcarolina12 <- northcarolina[northcarolina[1]==2012,]

newhampshire16 <- newhampshire[newhampshire[1]==2016,]
northcarolina16 <- northcarolina[northcarolina[1]==2016,]

newhampshire16$percentchange <- (newhampshire16$percentvotes-newhampshire12$percentvotes)/newhampshire12$percentvotes

northcarolina16$percentchange <- (northcarolina16$percentvotes-northcarolina12$percentvotes)/northcarolina12$percentvotes


head(gdp)

nhgdp <- (gdp[1825:1834,])
str(nhgdp)
as.numeric(gsub(',','',nhgdp[,2]))
as.numeric(gsub(',','',nhgdp[,5]))

newhampshire16rep <- newhampshire16[seq(2,nrow(newhampshire16),3),]
nrow(newhampshire16rep)
newhampshire16rep$gdpchange <- (as.numeric(gsub(',','',nhgdp[,5]))-as.numeric(gsub(',','',nhgdp[,2])))/as.numeric(gsub(',','',nhgdp[,2]))
(as.numeric(gsub(',','',nhgdp[,5]))-as.numeric(gsub(',','',nhgdp[,2])))/as.numeric(gsub(',','',nhgdp[,2]))
newhampshire16rep
summary(lm(percentchange ~ gdpchange, data=newhampshire16rep))

ncgdp <- (gdp[1959:2058,])
str(ncgdp)
northcarolina16rep <- northcarolina16[seq(2,nrow(northcarolina16),3),]
nrow(northcarolina16rep)
northcarolina16rep$gdpchange <- (as.numeric(gsub(',','', ncgdp[,5]))-as.numeric(gsub(',','', ncgdp[,2])))/as.numeric(gsub(',','', ncgdp[,2]))
northcarolina16rep
summary(lm(percentchange ~ gdpchange, data=northcarolina16rep))

ncdiff <- censusdiff[censusdiff[2]=='North Carolina',]
nhampdiff <- censusdiff[censusdiff[2]=='New Hampshire',]


ncallvars <- cbind(northcarolina16rep, ncdiff)
nhampallvars <- cbind(newhampshire16rep, nhampdiff)

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=ncallvars))

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=nhampallvars))


#Wisconsin
wisconsin <- case[case[2]=='Wisconsin',]
wisconsin12 <- wisconsin[wisconsin[1]==2012,]
wisconsin16 <- wisconsin[wisconsin[1]==2016,]
head(wisconsin12)
head(wisconsin16)
wisconsin16$percentchange <- (wisconsin16$percentvotes-wisconsin12$percentvotes)/wisconsin12$percentvotes
wisconsin16rep <- wisconsin16[seq(2,nrow(wisconsin16),3),]
nrow(wisconsin16rep)
wisgdp <- (gdp[3122:3193,])
wisgdp
nrow(wisgdp)

wisconsin16rep <- wisconsin16[seq(2,nrow(wisconsin16),3),]
head(wisconsin16rep)
nrow(wisconsin16rep)
wisconsin16rep$gdpchange <- (as.numeric(gsub(',','', wisgdp[,5]))-as.numeric(gsub(',','', wisgdp[,2])))/as.numeric(gsub(',','', wisgdp[,2]))

wiscdiff <- censusdiff[censusdiff[2]=='Wisconsin',]
wiscaallvars <- cbind(wisconsin16rep, wiscdiff)
summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=wiscaallvars))



#Virginia, not required
virginia <- case[case[2]=='Virginia',]
virginia12 <- virginia[virginia[1]==2012,]
virginia16 <- virginia[virginia[1]==2016,]
head(virginia12)
head(virginia16)
virginia16$percentchange <- (virginia16$percentvotes-virginia12$percentvotes)/virginia12$percentvotes
virginia16rep <- virginia16[seq(2,nrow(virginia16),3),]
virgdp <- (gdp[2915:3022,])

#colnames(virgdp)[colnames(virgdp)=="Table.1..Prototype.Real.Gross.Domestic.Product.by.County..2012...2015"] <- "county"

colnames(virgdp)[1] <- "county"

newOne <- merge(virginia16rep,virgdp,by = "county")
newOne$gdpchange <- (as.numeric(gsub(',','', newOne[,17]))-as.numeric(gsub(',','', newOne[,14])))/as.numeric(gsub(',','', newOne[,14]))
summary(lm(percentchange ~ gdpchange, data=newOne))

virgdiff <- censusdiff[censusdiff[2]=='Virginia',]
virgdiff <- virgdiff[c(1,4:7,9:15,17,19:26,28,30:33,35:39,41:43,45:46,48:59,61:68,70,71,74:77,80:83,86:90,92,94),]
virgdiff[3][1:72,]
newOne[1][1:87,]
newOne
newOne <- newOne[c(1,3:17,19:29,31:36,38:55,58:63,65:68,70,74:79,81,82,83,85:87),]
newOne <- newOne[c(1:7,9:nrow(newOne)),]
nrow(virgdiff)
nrow(newOne)

newOne <- newOne[,c(1:13,23)]

virgallvars <- cbind(newOne, virgdiff)

dim(virgallvars)
dim(coloallvars)

swingstates <- rbind(coloallvars,virgallvars,nhampallvars, wiscaallvars, ncallvars, nevadaallvars, pennallvars, ohioallvars, Micallvars, Minallvars, iowaallvars, floridaallvars)



str(swingstates)


quantile(swingstates$TotalPop,c(.33,.67))


quantile(swingstates$gdpchange,c(.33,.67))


quantile(swingstates$gdpchange,c(.25,.50 ,.75))

quantile(swingstates$TotalPop,c(.25,.50 ,.75))

head(swingstates)

for(x in 1:nrow(swingstates)){
	if(swingstates$TotalPop[x]<20400){
		swingstates$PopCat[x] <- 'Small'
	}else if(swingstates$TotalPop[x]>=20400 & swingstates$TotalPop[x]<=61750){
		swingstates$PopCat[x] <- 'Medium'
	}else{
		swingstates$PopCat[x] <- 'Large'
	}
}


for(x in 1:nrow(swingstates)){
	if(swingstates$gdpchange[x]<0.009699052){
		swingstates$gdpCat[x] <- 'gdpQ1'
	}else if(swingstates$gdpchange[x]>= 0.009699052 & swingstates$gdpchange[x]<= 0.038438953){
		swingstates$gdpCat[x] <- 'gdpQ2'
	}else if(swingstates$gdpchange[x]> 0.038438953 & swingstates$gdpchange[x]<= 0.098552162){
		swingstates$gdpCat <- 'gdpQ3'
	}else{
		swingstates$gdpCat[x] <- 'gdpQ4'
	}
}


head(swingstates)
swingstates$PopCat <- as.factor(swingstates$PopCat)

summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment, data=swingstates))

##Added State variable
summary(lm(percentchange ~ gdpchange + TotalPop + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment + state + TotalPoppercentchange, data=swingstates))

##Same as above but Total Pop is a categorical variable instead of states
summary(lm(percentchange ~ gdpchange + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment + TotalPoppercentchange + PopCat,  data=swingstates))

## State fixed and total pop category together
summary(lm(percentchange ~ gdpchange + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment + TotalPoppercentchange + PopCat + state,  data=swingstates))



##GDP Cat instead of GDP Change
summary(lm(percentchange ~ gdpCat + Men + Hispanic + White + Black + Native + Asian + Pacific + VotingAgeCitizen + Income + IncomePerCap + Poverty + ChildPoverty + Professional + Service + Office + Construction + Production + Drive + Carpool + Transit + Walk + OtherTransp + WorkAtHome + MeanCommute + Employed + PrivateWork + PublicWork + SelfEmployed + FamilyWork + Unemployment + TotalPoppercentchange + PopCat,  data=swingstates))

## GDP cat (category) seems to be a worse predictive variable than gdpchange (continuous)  we should use gdp change for our regression.  The state variable covers state fixed effects and is generally good - let's keep that.  The Total Population category seems to be better than general total population so let's keep that category as well.  

##best i can get with Population cagtegory
summary(lm(percentchange ~ gdpchange + Black + VotingAgeCitizen + IncomePerCap + Poverty + ChildPoverty + Walk + TotalPoppercentchange + PopCat,  data=swingstates))


##It seems like these groupings don't give us ant great regressions.  Definitely play around with these and other groups as well.  Also you can work on individual states as well.  Use swingstates (or a subset of this data frame) for all further calculations

