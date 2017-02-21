#Exploring Mortality Data Beta - Data manipulation
#24/02/16
#Written by Alexander Stuteley

setwd('C:/Users/astut/Desktop')
#setwd('H:/')
#library(reshape)

### reading in the mortality data
data = read.csv("mos3371.csv")

### Selecting only the important variables
### colnames(dd)

datareduce = data[,c(13,14,20,15:17,66,9,31,68:70,32,34)]

### Getting the year our of date of death.
dmy = as.Date(datareduce[,1],"%d/%m/%Y")
datareduce[,1] = dmy
datareduce$year = as.numeric(format(dmy,"%Y")) 

#### Looking at the data, lets take it after 88.
startingDate = 1996
dd = datareduce[datareduce$year>1995,]

### we only have post 1991 pop estimates so we only want post 1991 deaths
#post1991 = dd88[dd88$year>=1991,]

#post1991$YOB = post1991$year- post1991$AGE_AT_DEATH_YRS
##creating a year of birth variable

#### What we now want is to create an age group variable
froms = function(){
  froms = seq(from=0,to=80,by=5)
  #froms = paste(seq(from = start,to = 75,by = jump),seq(from = end,to = 80,by = jump),sep='.')
  #froms = c(froms,"80+","Total")
  #froms = c(froms,max(froms)+jump)
  FROMS <<- froms
  return(froms)
}

#### jump is the level of increase
yearCreator = function(froms=FROMS){
  ### creates a vector of cut offs
  Names = character(length(froms)-1)
  for(i in 1:(length(froms)-1)){
    Names[i] = paste(as.character(froms[i]),as.character((froms[i]+4)),sep=".")
  }
  ### creates a vector of names for each cut off group
  #Names[length(froms)] = paste(Names[length(froms)],"+",sep='')  
  Names = c(Names,"80 and over","Total")
  ### adds a pre group in.
  NAMES <<- Names
  return(Names)
}

### formats the mortality data set to give us the cohorts that we want.
variableCreator = function(data,existingVariable,froms=FROMS,names){
  location = grep(existingVariable,colnames(data))
  newLocation  = ncol(data) + 1
  ## finds where the date of birth is in the data set.
  ## and where we are putting the ageCohort variable in.  
  #data[(data[,location]<froms[2]),newLocation] = names[1]
  froms = c(froms,1000)
  for(i in 2:(length(names))){
    data[(data[,location]<froms[i] & data[,location]>=froms[i-1]),newLocation] = names[i-1]
  }
  colnames(data)[newLocation] = "ageband"
  return(data)
}

wrappedMortality = function(mortData = dd,Variable = "AGE_AT_DEATH_YRS"){
  FROMS = froms()
  NAMES = yearCreator(FROMS)
  returnedData = variableCreator(mortData,Variable,FROMS,NAMES)
  #returnedData$Maori = "Non Maori"
  #returnedData[(grepl("Maori",returnedData$BDM_ETHNIC_DESC1)|grepl("Maori",returnedData$BDM_ETHNIC_DESC2)|grepl("Maori",returnedData$BDM_ETHNIC_DESC3)),]$Maori="Maori"
  #returnedData[(grepl("Maori" ,returnedData$ETHNICITY_1)|grepl("Maori" ,returnedData$ETHNICITY_2)|grepl("Maori" ,returnedData$ETHNICITY_3)),]$Maori= "Maori"
  return(returnedData)
}

pp = wrappedMortality()
head(pp)

##trying to get counts asap

pcode = c(30,31,32,33,34,35,36,37)
acode = c(40,41,42,43,44)

mdf = pp[which(pp$ETHNICG1 == 21|#pp$ETHNICG1 == 32|
                 pp$ETHNICG2 == 21|#pp$ETHNICG2 == 32|
                 pp$ETHNICG3 == 21),#|pp$ETHNICG3 == 32,
         c(1,15,2,16,14,12)]
nmdf = pp[which(!(pp$ETHNICG1 == 21|#pp$ETHNICG1 == 32|
                    pp$ETHNICG2 == 21|#pp$ETHNICG2 == 32|
                    pp$ETHNICG3 == 21)),
          c(1,15,2,16,14,12)]
#is.element(issue,mort)
pdf = pp[which(is.element(pp$ETHNICG1,pcode)|#pp$ETHNICG1 == 32|
                 is.element(pp$ETHNICG2,pcode)|#pp$ETHNICG2 == 32|
                 is.element(pp$ETHNICG3,pcode)),#|pp$ETHNICG3 == 32,
         c(1,15,2,16,14,12)]

npdf = pp[which(!(is.element(pp$ETHNICG1,pcode)|#pp$ETHNICG1 == 32|
                    is.element(pp$ETHNICG2,pcode)|#pp$ETHNICG2 == 32|
                    is.element(pp$ETHNICG3,pcode))),#|pp$ETHNICG3 == 32,
          c(1,15,2,16,14,12)]
asdf = pp[which(is.element(pp$ETHNICG1,acode)|#pp$ETHNICG1 == 32|
                  is.element(pp$ETHNICG2,acode)|#pp$ETHNICG2 == 32|
                  is.element(pp$ETHNICG3,acode)),#|pp$ETHNICG3 == 32,
          c(1,15,2,16,14,12)]
nasdf = pp[which(!(is.element(pp$ETHNICG1,acode)|#pp$ETHNICG1 == 32|
                     is.element(pp$ETHNICG2,acode)|#pp$ETHNICG2 == 32|
                     is.element(pp$ETHNICG3,acode))),#|pp$ETHNICG3 == 32,
           c(1,15,2,16,14,12)]
npnmdf = pp[which(!(pp$ETHNICG1 == 21|#pp$ETHNICG1 == 32|
                      pp$ETHNICG2 == 21|#pp$ETHNICG2 == 32|
                      pp$ETHNICG3 == 21|
                      is.element(pp$ETHNICG1,pcode)|#pp$ETHNICG1 == 32|
                      is.element(pp$ETHNICG2,pcode)|#pp$ETHNICG2 == 32|
                      is.element(pp$ETHNICG3,pcode))),#|pp$ETHNICG3 == 32,
            c(1,15,2,16,14,12)]

adf = pp[,c(1,15,2,16,14,12)]

daagg = aggregate(DOD~GENDER+year+ageband,data = adf, FUN = function(x){NROW(x)})
csvcreator("","37total",daagg,0)
aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = adf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","37total",aagg,i)
}
csvcreator("dep06","37total",aagg,12)
csvcreator("dep06","37total",aagg,34)
csvcreator("dep06","37total",aagg,56)
csvcreator("dep06","37total",aagg,78)
csvcreator("dep06","37total",aagg,910)

dmagg = aggregate(DOD~GENDER+year+ageband,data = mdf, FUN = function(x){NROW(x)})
csvcreator("","37maori",dmagg,0)
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mdf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","37maori",magg,i)
}
csvcreator("dep06","37maori",magg,12)
csvcreator("dep06","37maori",magg,34)
csvcreator("dep06","37maori",magg,56)
csvcreator("dep06","37maori",magg,78)
csvcreator("dep06","37maori",magg,910)

dnmagg = aggregate(DOD~GENDER+year+ageband,data = nmdf, FUN = function(x){NROW(x)})
csvcreator("","37nonmaori",dnmagg,0)
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmdf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","37nonmaori",nmagg,i)
}
csvcreator("dep06","37nonmaori",nmagg,12)
csvcreator("dep06","37nonmaori",nmagg,34)
csvcreator("dep06","37nonmaori",nmagg,56)
csvcreator("dep06","37nonmaori",nmagg,78)
csvcreator("dep06","37nonmaori",nmagg,910)

#aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = adf, FUN = function(x){NROW(x)})
#maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mdf, FUN = function(x){NROW(x)})
#nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmdf, FUN = function(x){NROW(x)})

pagg = aggregate(DOD~GENDER+year+ageband,data = pdf, FUN = function(x){NROW(x)})
csvcreator("","37pacific",pagg,0)

npagg = aggregate(DOD~GENDER+year+ageband,data = npdf, FUN = function(x){NROW(x)})
csvcreator("","37nonpacific",npagg,0)

asagg = aggregate(DOD~GENDER+year+ageband,data = asdf, FUN = function(x){NROW(x)})
csvcreator("","37asian",asagg,0)

nasagg = aggregate(DOD~GENDER+year+ageband,data = nasdf, FUN = function(x){NROW(x)})
csvcreator("","37nonasian",nasagg,0)

npnmagg = aggregate(DOD~GENDER+year+ageband,data = npnmdf, FUN = function(x){NROW(x)})
csvcreator("","37nonpacificnonmaori",npnmagg,0)

{
pearce = c(paste("I",10:15,sep=""),
           paste("A",15:19,sep=""),"J45",paste("I0",5:9,sep=""),
           paste("K",35:38,sep=""),paste("J0",0:9,sep=""),paste("J",20:22,sep=""),
           "A03","B45",paste("H",65:70,sep=""),paste("I0",0:2,sep=""),"K12",
           paste("L0",1:8,sep=""),"L88","M00","M86","C81",paste("K",40:46,sep=""),
           "K81",paste("D",50:53,sep=""),paste("J",12:19,sep=""),
           #icd 9 codes
           paste("40",1:5,sep=""),paste("1",0:8,sep=""),"137","493",paste("39",3:8,sep=""),
           paste("54",0:3,sep=""),paste("46",0:6,sep=""),"487","4","34","321",paste("38",1:3,sep=""),
           paste("39",0:2,sep=""),paste("68",0:6,sep=""),"711","730","201",paste("55",0:3,sep=""),
           "574","575","280","281",paste("48",0:6,sep=""),"490")

pearcedf = adf[which(is.element(substring(adf$icdd,1,3),substring(pearce,1,3))),]
pearcendf = adf[which(!is.element(substring(adf$icdd,1,3),substring(pearce,1,3))),]
mpearcedf = mdf[which(is.element(substring(mdf$icdd,1,3),substring(pearce,1,3))),]
mpearcendf = mdf[which(!is.element(substring(mdf$icdd,1,3),substring(pearce,1,3))),]
nmpearcedf = nmdf[which(is.element(substring(nmdf$icdd,1,3),substring(pearce,1,3))),]
nmpearcendf = nmdf[which(!is.element(substring(nmdf$icdd,1,3),substring(pearce,1,3))),]

ppearcedf = pdf[which(is.element(substring(pdf$icdd,1,3),substring(pearce,1,3))),]
ppearcendf = pdf[which(!is.element(substring(pdf$icdd,1,3),substring(pearce,1,3))),]
nppearcedf = npdf[which(is.element(substring(npdf$icdd,1,3),substring(pearce,1,3))),]
nppearcendf = npdf[which(!is.element(substring(npdf$icdd,1,3),substring(pearce,1,3))),]
aspearcedf = asdf[which(is.element(substring(asdf$icdd,1,3),substring(pearce,1,3))),]
aspearcendf = asdf[which(!is.element(substring(asdf$icdd,1,3),substring(pearce,1,3))),]
naspearcedf = nasdf[which(is.element(substring(nasdf$icdd,1,3),substring(pearce,1,3))),]
naspearcendf = nasdf[which(!is.element(substring(nasdf$icdd,1,3),substring(pearce,1,3))),]
npnmpearcedf = npnmdf[which(is.element(substring(npnmdf$icdd,1,3),substring(pearce,1,3))),]
npnmpearcendf = npnmdf[which(!is.element(substring(npnmdf$icdd,1,3),substring(pearce,1,3))),]

daagg = aggregate(DOD~GENDER+year+ageband,data = pearcedf, FUN = function(x){NROW(x)})
csvcreator("","pearcetotal",daagg,0)
aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = pearcedf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","pearcetotal",aagg,i)
}
csvcreator("dep06","pearcetotal",aagg,12)
csvcreator("dep06","pearcetotal",aagg,34)
csvcreator("dep06","pearcetotal",aagg,56)
csvcreator("dep06","pearcetotal",aagg,78)
csvcreator("dep06","pearcetotal",aagg,910)

dmagg = aggregate(DOD~GENDER+year+ageband,data = mpearcedf, FUN = function(x){NROW(x)})
csvcreator("","pearcemaori",dmagg,0)
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mpearcedf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","pearcemaori",magg,i)
}
csvcreator("dep06","pearcemaori",magg,12)
csvcreator("dep06","pearcemaori",magg,34)
csvcreator("dep06","pearcemaori",magg,56)
csvcreator("dep06","pearcemaori",magg,78)
csvcreator("dep06","pearcemaori",magg,910)
#magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mpearcedf, FUN = function(x){NROW(x)})

dnmagg = aggregate(DOD~GENDER+year+ageband,data = nmpearcedf, FUN = function(x){NROW(x)})
csvcreator("","pearcenonmaori",dnmagg,0)
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmpearcedf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","pearcenonmaori",nmagg,i)
}
csvcreator("dep06","pearcenonmaori",nmagg,12)
csvcreator("dep06","pearcenonmaori",nmagg,34)
csvcreator("dep06","pearcenonmaori",nmagg,56)
csvcreator("dep06","pearcenonmaori",nmagg,78)
csvcreator("dep06","pearcenonmaori",nmagg,910)
#nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmpearcedf, FUN = function(x){NROW(x)})


#aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = pearcendf, FUN = function(x){NROW(x)})
#maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mpearcendf, FUN = function(x){NROW(x)})
#nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmpearcendf, FUN = function(x){NROW(x)})

daaggn = aggregate(DOD~GENDER+year+ageband,data = pearcendf, FUN = function(x){NROW(x)})
csvcreator("","notpearcetotal",daaggn,0)
aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = pearcendf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","notpearcetotal",aaggn,i)
}
csvcreator("dep06","notpearcetotal",aaggn,12)
csvcreator("dep06","notpearcetotal",aaggn,34)
csvcreator("dep06","notpearcetotal",aaggn,56)
csvcreator("dep06","notpearcetotal",aaggn,78)
csvcreator("dep06","notpearcetotal",aaggn,910)

dmaggn = aggregate(DOD~GENDER+year+ageband,data = mpearcendf, FUN = function(x){NROW(x)})
csvcreator("","notpearcemaori",dmaggn,0)
maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mpearcendf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","notpearcemaori",maggn,i)
}
csvcreator("dep06","notpearcemaori",maggn,12)
csvcreator("dep06","notpearcemaori",maggn,34)
csvcreator("dep06","notpearcemaori",maggn,56)
csvcreator("dep06","notpearcemaori",maggn,78)
csvcreator("dep06","notpearcemaori",maggn,910)
#magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mpearcedf, FUN = function(x){NROW(x)})

dnmaggn = aggregate(DOD~GENDER+year+ageband,data = nmpearcendf, FUN = function(x){NROW(x)})
csvcreator("","notpearcenonmaori",dnmaggn,0)
nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmpearcendf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","notpearcenonmaori",nmaggn,i)
}
csvcreator("dep06","notpearcenonmaori",nmaggn,12)
csvcreator("dep06","notpearcenonmaori",nmaggn,34)
csvcreator("dep06","notpearcenonmaori",nmaggn,56)
csvcreator("dep06","notpearcenonmaori",nmaggn,78)
csvcreator("dep06","notpearcenonmaori",nmaggn,910)
}
{
  
  raymontav = c(paste("A",15:19,sep=""),"B90",paste("A",38:41,sep=""),"A46","A481",paste("B",50:54,sep=""),
                "G00","G03","J020",paste("J",13:15,sep=""),"J18","L03",paste("B",15:24,sep=""),"J10","J12",
                "J171","J21",paste("E0",0:7,sep=""),"I26","I802",paste("K",35:38,sep=""),
                paste("K",40:46,sep=""),paste("K",80:83,sep=""),paste("K",85:86,sep=""),"K915",
                paste("O0",0:9,sep=""),paste("O",10:99,sep=""),paste("C0",0:9,sep=""),paste("C",10:16,sep=""),
                paste("C",18:22,sep=""),paste("C",33:34,sep=""),paste("C",43:44,sep=""),"C50",
                paste("C",53:55,sep=""),"C67","C73","C81",paste("C91",0:1,sep=""),paste("D",10:36,sep=""),
                paste("E",10:14,sep=""),paste("G",40:41,sep=""),paste("I0",0:9,sep=""),paste("I",11:13,sep=""),
                paste("I",20:25,sep=""),paste("I",60:69,sep=""),"I71",paste("N0",0:9,sep=""),"N13",
                paste("N",17:21,sep=""),"N35","N40","N991",paste("J",40:46,sep=""),paste("K",25:28,sep=""),
                paste("K",73:74,sep=""),"H311","P00",paste("P0",3:9,sep=""),paste("P",10:95,sep=""),
                paste("Q0",0:9,sep=""),paste("Q",10:99,sep=""),paste("F",10:16,sep=""),paste("F",18:19,sep=""),
                "I426","K292","K70",paste("V0",1:4,sep=""),"V06","V09",paste("V",10:80,sep=""),"V87","V89",
                "V99",paste("W0",0:9,sep=""),paste("W",10:19,sep=""),paste("W",65:74,sep=""),
                paste("X0",0:9,sep=""),paste("X",40:49,sep=""),paste("X",60:99,sep=""),paste("Y0",0:9,sep=""),
                paste("Y87",0:1,sep="")
                #icd 9
                ,"1123"  ,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,
                "1196"	,"1304"	,"1390"	,"1500"	,"1501"	,"1502"	,"1503"	,"1504"	,"1505"	,"1510"	,"1511"	,"1512"	,
                "1513"	,"1514"	,"1515"	,"1516"	,"1520"	,"1521"	,"1522"	,"1523"	,"1550"	,"1551"	,"1552"	,"1570"	,
                "1571"	,"1572"	,"1573"	,"1574"	,"1580"	,"1590"	,"1591"	,"1600"	,"1601"	,"1602"	,"1603"	,"1610"	,
                "1611"	,"1612"	,"1620"	,"1622"	,"1623"	,"1624"	,"1625"	,"1630"	,"1631"	,"1720"	,"1721"	,"1722"	,
                "1723"	,"1724"	,"1725"	,"1726"	,"1401"	,"1404"	,"1701"	,"1702"	,"1703"	,"1704"	,"1706"	,"1710"	,
                "1712"	,"1713"	,"1714"	,"1715"	,"1716"	,"1730"	,"1731"	,"1732"	,"1733"	,"1734"	,"1735"	,"1736"	,
                "1740"	,"1741"	,"1742"	,"1743"	,"1744"	,"1745"	,"1746"	,"1765"	,"1800"	,"1801"	,"1880"	,"1881"	,
                "1882"	,"1883"	,"1884"	,"1885"	,"1886"	,"1890"	,"1891"	,"1892"	,"1893"	,"1894"	,"1895"	,"360"	,
                "363"	,"362"	,"382"	,"380"	,"381"	,"3841"	,"383"	,"3842"	,"3840"	,"3849"	,"388"	,"389"	,
                "461"	,"462"	,"463"	,"460"	,"1372"	,"1373"	,"1370"	,"3200"	,"3201"	,"3202"	,"3203"	,"3209"	,
                "32082"	,"3222"	,"3229"	,"340"	,"481"	,"4822"	,"4820"	,"4821"	,"4824"	,"48230"	,"48231"	,
                "48239"	,"48282"	,"48283"	,"48289"	,"4829"	,"485"	,"486"	,"68100"	,"68111"	,"6823"	,"6824"	,
                "6826"	,"6827"	,"6822"	,"6828"	,"6829"	,"701"	,"7031"	,"7020"	,"7030"	,"7041"	,"7051"	,"7049"	,
                "7033"	,"7022"	,"7032"	,"7044"	,"7054"	,"709"	,"4870"	,"4871"	,"4878"	,"4800"	,"4801"	,"4808"	,
                "4809"	,"4661"	,"2449"	,"2410"	,"2411"	,"2409"	,"24200"	,"24220"	,"24221"	,"24230"	,"24290"	,
                "24291"	,"2452"	,"41519"	,"45119"	,"5400"	,"5401"	,"5409"	,"541"	,"55012"	,"55092"	,"55010"	,
                "55000"	,"55001"	,"55200"	,"55100"	,"55300"	,"5521"	,"5511"	,"55220"	,"55221"	,"55321"	,
                "5523"	,"5533"	,"5528"	,"55229"	,"5518"	,"5538"	,"5529"	,"57400"	,"57401"	,"57410"	,"57411"	,
                "57420"	,"57421"	,"57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,"5750"	,"5751"	,
                "5761"	,"5762"	,"5764"	,"5768"	,"5769"	,"5770"	,"5771"	,"5772"	,"5778"	,"5779"	,"64123"	,"66511"	,
                "67002"	,"64671"	,"67131"	,"67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,
                "64863"	,"64893"	,"1409"	,"1410"	,"1411"	,"1412"	,"1413"	,"1414"	,"1418"	,"1419"	,"1430"	,"1431"	,
                "1440"	,"1441"	,"1448"	,"1449"	,"1452"	,"1453"	,"1454"	,"1455"	,"1450"	,"1451"	,"1456"	,"1458"	,
                "1459"	,"1420"	,"1421"	,"1429"	,"1461"	,"1462"	,"1460"	,"1468"	,"1469"	,"1471"	,"1478"	,"1479"	,
                "1488"	,"1489"	,"1490"	,"1498"	,"1509"	,"1518"	,"1519"	,"1534"	,"1535"	,"1536"	,"1530"	,"1531"	,
                "1537"	,"1532"	,"1533"	,"1538"	,"1539"	,"1540"	,"1541"	,"1543"	,"1542"	,"1548"	,"1628"	,"1629"	,
                "1727"	,"1728"	,"1729"	,"1737"	,"1738"	,"1739"	,"1748"	,"1759"	,"1749"	,"1808"	,"1809"	,"1821"	,
                "1820"	,"179"	,"1887"	,"1888"	,"1889"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,"20150"	,
                "20151"	,"20152"	,"20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,"20190"	,
                "20195"	,"20198"	,"20400"	,"20401"	,"2106"	,"2113"	,"2114"	,"2112"	,"2115"	,"2116"	,"2117"	,
                "2123"	,"2126"	,"2127"	,"2136"	,"2148"	,"22809"	,"22801"	,"22802"	,"22804"	,"2281"	,"2150"	,
                "2189"	,"2230"	,"2231"	,"2252"	,"2254"	,"2250"	,"2251"	,"2253"	,"2270"	,"2273"	,"25021"	,"25031"	,
                "25033"	,"25011"	,"25013"	,"25041"	,"25043"	,"25061"	,"25071"	,"25073"	,"25081"	,"25083"	,
                "25091"	,"25001"	,"25003"	,"25020"	,"25030"	,"25010"	,"25012"	,"25040"	,"25042"	,"25050"	,
                "25060"	,"25070"	,"25072"	,"25080"	,"25082"	,"25000"	,"25002"	,"34550"	,"34500"	,"34510"	,
                "34511"	,"34560"	,"34570"	,"34580"	,"34590"	,"34591"	,"3453"	,"3452"	,"34540"	,"3910"	,"3912"	,
                "3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,"3959"	,"3970"	,"3960"	,
                "3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"40201"	,"40211"	,"40291"	,
                "40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,"40492"	,"40413"	,"40493"	,
                "40490"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,"41012"	,"41021"	,"41031"	,
                "41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,"41091"	,"41092"	,"41071"	,
                "41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,"41410"	,"41419"	,"4148"	,
                "4149"	,"430"	,"431"	,"4321"	,"4320"	,"4329"	,"43301"	,"43311"	,"43321"	,"43331"	,"43391"	,
                "43401"	,"43411"	,"43491"	,"436"	,"43320"	,"43300"	,"43310"	,"43330"	,"43380"	,"43400"	,
                "43410"	,"43490"	,"4373"	,"4370"	,"4372"	,"4375"	,"4371"	,"4378"	,"4379"	,"438"	,"44100"	,
                "44101"	,"44102"	,"44103"	,"4411"	,"4412"	,"4413"	,"4414"	,"4416"	,"4417"	,"4415"	,"4419"	,
                "58089"	,"5809"	,"5804"	,"5834"	,"5821"	,"5822"	,"58289"	,"5829"	,"5813"	,"5811"	,"5812"	,"5819"	,
                "5831"	,"5832"	,"5830"	,"5836"	,"58389"	,"5839"	,"591"	,"59372"	,"59373"	,"5996"	,"5845"	,
                "5849"	,"5859"	,"5851"	,"586"	,"5920"	,"5921"	,"5929"	,"5941"	,"5989"	,"4918"	,"4919"	,"4920"	,
                "4928"	,"49121"	,"49120"	,"49320"	,"49321"	,"496"	,"49300"	,"49310"	,"49390"	,"49301"	,
                "49391"	,"53100"	,"53110"	,"53120"	,"53140"	,"53150"	,"53151"	,"53160"	,"53170"	,"53190"	,
                "53191"	,"53200"	,"53210"	,"53220"	,"53240"	,"53241"	,"53250"	,"53251"	,"53260"	,"53271"	,
                "53290"	,"53300"	,"53310"	,"53340"	,"53341"	,"53350"	,"53360"	,"53390"	,"53410"	,"53450"	,
                "57149"	,"57140"	,"5716"	,"5715"	,"76401"	,"76402"	,"76404"	,"76407"	,"76429"	,"76490"	,
                "76491"	,"76492"	,"76493"	,"76494"	,"76495"	,"76496"	,"76497"	,"76498"	,"76499"	,"76500"	,
                "76501"	,"76502"	,"76503"	,"76511"	,"76512"	,"76514"	,"76516"	,"76518"	,"7660"	,"7661"	,
                "7662"	,"7670"	,"7671"	,"7682"	,"7683"	,"7684"	,"7685"	,"7689"	,"7700"	,"7701"	,"7702"	,"7703"	,
                "7707"	,"7704"	,"7705"	,"7708"	,"7709"	,"7711"	,"7717"	,"7712"	,"7714"	,"7718"	,"7720"	,"7721"	,
                "7722"	,"7760"	,"7725"	,"7726"	,"7728"	,"7730"	,"7731"	,"7732"	,"7780"	,"7747"	,"7744"	,"7762"	,
                "7761"	,"7764"	,"7765"	,"7750"	,"7756"	,"7758"	,"7757"	,"7775"	,"7776"	,"7792"	,"7680"	,"7681"	,
                "7400"	,"7402"	,"7424"	,"74101"	,"74102"	,"74103"	,"74100"	,"74192"	,"74193"	,"74190"	,
                "74253"	,"74259"	,"7428"	,"7429"	,"74483"	,"74489"	,"7450"	,"74511"	,"74510"	,"74519"	,
                "74512"	,"7458"	,"7454"	,"7455"	,"74569"	,"7452"	,"74601"	,"74602"	,"74609"	,"7461"	,"7462"	,
                "7463"	,"7464"	,"7465"	,"7466"	,"7467"	,"74687"	,"74685"	,"74686"	,"74684"	,"74689"	,"7469"	,
                "7470"	,"74710"	,"74722"	,"74729"	,"7473"	,"74741"	,"74742"	,"74740"	,"7475"	,"74761"	,
                "74769"	,"74781"	,"74782"	,"7483"	,"7484"	,"7485"	,"74869"	,"74860"	,"74900"	,"74910"	,
                "75016"	,"75029"	,"7503"	,"7504"	,"7511"	,"7512"	,"7510"	,"7513"	,"7514"	,"7515"	,"75161"	,
                "75169"	,"75162"	,"7518"	,"7530"	,"75314"	,"75313"	,"75312"	,"75315"	,"75317"	,"75319"	,
                "75310"	,"7532"	,"7534"	,"7533"	,"7536"	,"7538"	,"75563"	,"75470"	,"7540"	,"7542"	,"75481"	,
                "75489"	,"75500"	,"75529"	,"75534"	,"75560"	,"75569"	,"75600"	,"75608"	,"75609"	,"75615"	,
                "75619"	,"75656"	,"75651"	,"75659"	,"7564"	,"75650"	,"7566"	,"7567"	,"75683"	,"7569"	,"7571"	,
                "75739"	,"23770"	,"23771"	,"23772"	,"7595"	,"7596"	,"75987"	,"75981"	,"75986"	,"75985"	,
                "75982"	,"75984"	,"7590"	,"7591"	,"7593"	,"7594"	,"7597"	,"75989"	,"7599"	,"7580"	,"7582"	,"7581"	,
                "7583"	,"7585"	,"7586"	,"7588"	,"7589"	,"30300"	,"30301"	,"30302"	,"30500"	,"30501"	,"30502"	,
                "30390"	,"30393"	,"2911"	,"2912"	,"30550"	,"30552"	,"30553"	,"30400"	,"30401"	,"30470"	,
                "30471"	,"30540"	,"30410"	,"30570"	,"30531"	,"30460"	,"30461"	,"30462"	,"30480"	,"30481"	,
                "4255"	,"5710"	,"5711"	,"5712"	,"5713"	,"82270"	,"82271"	,"82273"	,"82276"	,"82278"	,"81475"	,
                "82804"	,"82903"	,"82570"	,"82573"	,"82574"	,"82579"	,"81875"	,"81365"	,"80138"	,"81265"	,
                "82613"	,"82615"	,"82618"	,"81025"	,"81035"	,"81525"	,"81535"	,"81625"	,"81825"	,"81635"	,
                "82128"	,"82220"	,"82221"	,"82528"	,"81225"	,"81235"	,"81925"	,"81005"	,"81015"	,"82406"	,
                "82304"	,"82308"	,"82314"	,"81505"	,"81515"	,"81715"	,"81605"	,"81805"	,"81615"	,"81815"	,
                "82104"	,"82108"	,"82114"	,"82211"	,"82502"	,"82503"	,"82504"	,"82508"	,"81205"	,"81115"	,
                "81215"	,"81915"	,"82724"	,"82820"	,"82824"	,"82825"	,"82828"	,"81885"	,"81895"	,"82483"	,
                "82590"	,"82598"	,"81995"	,"82891"	,"88500"	,"88507"	,"88506"	,"88504"	,"88505"	,"88508"	,
                "88509"	,"88697"	,"88604"	,"88699"	,"88460"	,"88467"	,"88466"	,"88469"	,"88440"	,"88447"	,
                "88449"	,"88420"	,"88427"	,"88429"	,"88090"	,"88097"	,"88096"	,"88094"	,"88015"	,"88098"	,
                "88099"	,"88105"	,"88103"	,"88108"	,"88109"	,"88113"	,"88200"	,"88206"	,"88205"	,"88203"	,
                "88208"	,"88209"	,"88430"	,"88410"	,"88414"	,"88411"	,"88418"	,"88300"	,"88308"	,"88310"	,
                "88390"	,"88397"	,"88315"	,"88391"	,"88398"	,"88470"	,"88477"	,"88490"	,"88800"	,"88497"	,
                "88807"	,"88806"	,"88494"	,"88804"	,"88495"	,"88805"	,"88491"	,"88801"	,"88498"	,"88808"	,
                "88499"	,"88809"	,"91040"	,"91050"	,"91056"	,"91054"	,"91058"	,"91098"	,"91060"	,"91064"	,
                "91024"	,"91018"	,"91028"	,"91038"	,"91070"	,"91074"	,"91071"	,"91078"	,"91080"	,"91084"	,
                "91081"	,"91088"	,"89020"	,"89030"	,"89080"	,"89090"	,"89127"	,"89137"	,"89136"	,"89131"	,
                "89200"	,"89205"	,"89208"	,"89500"	,"89400"	,"89300"	,"89380"	,"89810"	,"85080"	,"85046"	,
                "85086"	,"85049"	,"85320"	,"85420"	,"85500"	,"85010"	,"85020"	,"85017"	,"85027"	,"85029"	,
                "85510"	,"85700"	,"85830"	,"85880"	,"85890"	,"85709"	,"85809"	,"85829"	,"86010"	,"86020"	,
                "86210"	,"86240"	,"86700"	,"86820"	,"86830"	,"86880"	,"86805"	,"86833"	,"86983"	,"86808"	,
                "86818"	,"86828"	,"86988"	,"86338"	,"86407"	,"95000"	,"95007"	,"95008"	,"95009"	,"95010"	,
                "95020"	,"95030"	,"95037"	,"95036"	,"95028"	,"95038"	,"95039"	,"95040"	,"95050"	,"95047"	,
                "95046"	,"95045"	,"95041"	,"95049"	,"95110"	,"95113"	,"95290"	,"95060"	,"95065"	,"95061"	,
                "95070"	,"95090"	,"95097"	,"95099"	,"95300"	,"95310"	,"95307"	,"95317"	,"95387"	,"95306"	,
                "95316"	,"95304"	,"95314"	,"95305"	,"95385"	,"95302"	,"95303"	,"95301"	,"95308"	,"95309"	,
                "95400"	,"95407"	,"95406"	,"95401"	,"95408"	,"95500"	,"95504"	,"95510"	,"95520"	,"95516"	,
                "95514"	,"95524"	,"95515"	,"95525"	,"95522"	,"95511"	,"95521"	,"95518"	,"95528"	,"95540"	,
                "95547"	,"95544"	,"95545"	,"95541"	,"95548"	,"95549"	,"95550"	,"95810"	,"95817"	,"95815"	,
                "95818"	,"95819"	,"95824"	,"95600"	,"95607"	,"95605"	,"95603"	,"95609"	,"95700"	,"95710"	,
                "95707"	,"95717"	,"95716"	,"95715"	,"95713"	,"95718"	,"95728"	,"95806"	,"95805"	,"95803"	,
                "95808"	,"95855"	,"95858"	,"95840"	,"95847"	,"96220"	,"96228"	,"96300"	,"96307"	,"96306"	,
                "96308"	,"96408"	,"96510"	,"96520"	,"96516"	,"96514"	,"96515"	,"96525"	,"96531"	,"96518"	,
                "96528"	,"96540"	,"96546"	,"96543"	,"96548"	,"96800"	,"96807"	,"96808"	,"96600"	,"96607"	,
                "96606"	,"96604"	,"96605"	,"96603"	,"96608"	,"96609"	,"96820"	,"96826"	,"96824"	,"96825"	,
                "96823"	,"96828"	,"96829"	,"96855"	,"96000"	,"96007"	,"96006"	,"96005"	,"96008"	,"96009"	,
                "96720"	,"96725"	,"96700"	,"96704"	,"96709"	,"96730"	,"96710"	,"96790"	,"96799"	,"96889"	,
                "96890"	,"96897"	,"96895"	,"96898"	,"96899"	,"95909"	,"96909")

raymontavdf = adf[which(is.element(substring(adf$icdd,1,3),substring(raymontav,1,3))&(!is.element(adf$ageband,c("75.79","80 and over")))),]
raymontavndf = adf[which(!is.element(substring(adf$icdd,1,3),substring(raymontav,1,3))&(!is.element(adf$ageband,c("75.79","80 and over")))),]
mraymontavdf = mdf[which(is.element(substring(mdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
mraymontavndf = mdf[which(!is.element(substring(mdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
nmraymontavdf = nmdf[which(is.element(substring(nmdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
nmraymontavndf = nmdf[which(!is.element(substring(nmdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]

praymontavdf = pdf[which(is.element(substring(pdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
praymontavndf = pdf[which(!is.element(substring(pdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
npraymontavdf = npdf[which(is.element(substring(npdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
npraymontavndf = npdf[which(!is.element(substring(npdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
asraymontavdf = asdf[which(is.element(substring(asdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
asraymontavndf = asdf[which(!is.element(substring(asdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
nasraymontavdf = nasdf[which(is.element(substring(nasdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
nasraymontavndf = nasdf[which(!is.element(substring(nasdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
npnmraymontavdf = npnmdf[which(is.element(substring(npnmdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
npnmraymontavndf = npnmdf[which(!is.element(substring(npnmdf$icdd,1,3),substring(raymontav,1,3))&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]

daagg = aggregate(DOD~GENDER+year+ageband,data = raymontavdf, FUN = function(x){NROW(x)})
csvcreator("","raymontavtotal",daagg,0)
aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontavdf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","raymontavtotal",aagg,i)
}
csvcreator("dep06","raymontavtotal",aagg,12)
csvcreator("dep06","raymontavtotal",aagg,34)
csvcreator("dep06","raymontavtotal",aagg,56)
csvcreator("dep06","raymontavtotal",aagg,78)
csvcreator("dep06","raymontavtotal",aagg,910)

dmagg = aggregate(DOD~GENDER+year+ageband,data = mraymontavdf, FUN = function(x){NROW(x)})
csvcreator("","raymontavmaori",dmagg,0)
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavdf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","raymontavmaori",magg,i)
}
csvcreator("dep06","raymontavmaori",magg,12)
csvcreator("dep06","raymontavmaori",magg,34)
csvcreator("dep06","raymontavmaori",magg,56)
csvcreator("dep06","raymontavmaori",magg,78)
csvcreator("dep06","raymontavmaori",magg,910)
#magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavdf, FUN = function(x){NROW(x)})

dnmagg = aggregate(DOD~GENDER+year+ageband,data = nmraymontavdf, FUN = function(x){NROW(x)})
csvcreator("","raymontavnonmaori",dnmagg,0)
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontavdf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","raymontavnonmaori",nmagg,i)
}
csvcreator("dep06","raymontavnonmaori",nmagg,12)
csvcreator("dep06","raymontavnonmaori",nmagg,34)
csvcreator("dep06","raymontavnonmaori",nmagg,56)
csvcreator("dep06","raymontavnonmaori",nmagg,78)
csvcreator("dep06","raymontavnonmaori",nmagg,910)
#nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontavdf, FUN = function(x){NROW(x)})


#aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontavndf, FUN = function(x){NROW(x)})
#maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavndf, FUN = function(x){NROW(x)})
#nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontavndf, FUN = function(x){NROW(x)})

daaggn = aggregate(DOD~GENDER+year+ageband,data = raymontavndf, FUN = function(x){NROW(x)})
csvcreator("","notraymontavtotal",daaggn,0)
aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontavndf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","notraymontavtotal",aaggn,i)
}
csvcreator("dep06","notraymontavtotal",aaggn,12)
csvcreator("dep06","notraymontavtotal",aaggn,34)
csvcreator("dep06","notraymontavtotal",aaggn,56)
csvcreator("dep06","notraymontavtotal",aaggn,78)
csvcreator("dep06","notraymontavtotal",aaggn,910)

dmaggn = aggregate(DOD~GENDER+year+ageband,data = mraymontavndf, FUN = function(x){NROW(x)})
csvcreator("","notraymontavmaori",dmaggn,0)
maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavndf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","notraymontavmaori",maggn,i)
}
csvcreator("dep06","notraymontavmaori",maggn,12)
csvcreator("dep06","notraymontavmaori",maggn,34)
csvcreator("dep06","notraymontavmaori",maggn,56)
csvcreator("dep06","notraymontavmaori",maggn,78)
csvcreator("dep06","notraymontavmaori",maggn,910)
#magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavdf, FUN = function(x){NROW(x)})

dnmaggn = aggregate(DOD~GENDER+year+ageband,data = nmraymontavndf, FUN = function(x){NROW(x)})
csvcreator("","notraymontavnonmaori",dnmaggn,0)
nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontavndf, FUN = function(x){NROW(x)})
for(i in 1:10){
  csvcreator("dep06","notraymontavnonmaori",nmaggn,i)
}
csvcreator("dep06","notraymontavnonmaori",nmaggn,12)
csvcreator("dep06","notraymontavnonmaori",nmaggn,34)
csvcreator("dep06","notraymontavnonmaori",nmaggn,56)
csvcreator("dep06","notraymontavnonmaori",nmaggn,78)
csvcreator("dep06","notraymontavnonmaori",nmaggn,910)
}
{
  raymontam = c(paste("A",15:19,sep=""),paste("A",38:41,sep=""),"A46","A481",paste("B",50:54,sep=""),"B90",
                "G00","G03","J020",paste("J",13:15,sep=""),"J18","L03",paste("B",20:24,sep=""),"C16",
                paste("C",18:21,sep=""),paste("C",40:41,sep=""),paste("C",43:44,sep=""),"C50",
                paste("C",53:55,sep=""),paste("C",61:62,sep=""),"C67","C73","C81","C910",paste("D",10:36,sep=""),
                paste("E0",0:7,sep=""),paste("G",40:41,sep=""),paste("K",35:38,sep=""),paste("K",40:46,sep=""),
                paste("K",80:83,sep=""),"K85","K915",paste("E",10:14,sep=""),"H311",paste("O0",0:9,sep=""),
                paste("O",10:96,sep=""),paste("O",98:99,sep=""),paste("P0",0:9,sep=""),paste("P",10:95,sep=""),
                paste("Q0",0:9,sep=""),paste("Q",10:99,sep=""),paste("I0",1:9,sep=""),paste("I",10:13,sep=""),
                paste("I",20:26,sep=""),paste("I",33:37,sep=""),"I50",paste("I",60:69,sep=""),
                paste("J",40:46,sep=""),paste("K",25:28,sep=""),"N13",paste("N",17:21,sep=""),"N35","N40","N991"
                #icd 9
                ,"1123"  ,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,
                "1196"	,"1304"	,"1390"	,"1500"	,"1501"	,"1502"	,"1503"	,"1504"	,"1505"	,"1510"	,"1511"	,"1512"	,
                "1513"	,"1514"	,"1515"	,"1516"	,"1520"	,"1521"	,"1522"	,"1523"	,"1550"	,"1551"	,"1552"	,"1570"	,
                "1571"	,"1572"	,"1573"	,"1574"	,"1580"	,"1590"	,"1591"	,"1600"	,"1601"	,"1602"	,"1603"	,"1610"	,
                "1611"	,"1612"	,"1620"	,"1622"	,"1623"	,"1624"	,"1625"	,"1630"	,"1631"	,"1720"	,"1721"	,"1722"	,
                "1723"	,"1724"	,"1725"	,"1726"	,"1401"	,"1404"	,"1701"	,"1702"	,"1703"	,"1704"	,"1706"	,"1710"	,
                "1712"	,"1713"	,"1714"	,"1715"	,"1716"	,"1730"	,"1731"	,"1732"	,"1733"	,"1734"	,"1735"	,"1736"	,
                "1740"	,"1741"	,"1742"	,"1743"	,"1744"	,"1745"	,"1746"	,"1765"	,"1800"	,"1801"	,"1880"	,"1881"	,
                "1882"	,"1883"	,"1884"	,"1885"	,"1886"	,"1890"	,"1891"	,"1892"	,"1893"	,"1894"	,"1895"	,"360"	,
                "363"	,"362"	,"382"	,"380"	,"381"	,"3841"	,"383"	,"3842"	,"3840"	,"3849"	,"388"	,"389"	,
                "461"	,"462"	,"463"	,"460"	,"1372"	,"1373"	,"1370"	,"3200"	,"3201"	,"3202"	,"3203"	,"3209"	,
                "32082"	,"3222"	,"3229"	,"340"	,"481"	,"4822"	,"4820"	,"4821"	,"4824"	,"48230"	,"48231"	,
                "48239"	,"48282"	,"48283"	,"48289"	,"4829"	,"485"	,"486"	,"68100"	,"68111"	,"6823"	,"6824"	,
                "6826"	,"6827"	,"6822"	,"6828"	,"6829"	,"2449"	,"2410"	,"2411"	,"2409"	,"24200"	,"24220"	,
                "24221"	,"24230"	,"24290"	,"24291"	,"2452"	,"41519"	,"5400"	,"5401"	,"5409"	,"541"	,"55012"	,
                "55092"	,"55010"	,"55000"	,"55001"	,"55200"	,"55100"	,"55300"	,"5521"	,"5511"	,"55220"	,
                "55221"	,"55321"	,"5523"	,"5533"	,"5528"	,"55229"	,"5518"	,"5538"	,"5529"	,"57400"	,"57401"	,
                "57410"	,"57411"	,"57420"	,"57421"	,"57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,
                "5750"	,"5751"	,"5761"	,"5762"	,"5764"	,"5768"	,"5769"	,"5770"	,"64123"	,"66511"	,"67002"	,
                "64671"	,"67131"	,"67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,"64863"	,
                "64893"	,"1518"	,"1519"	,"1534"	,"1535"	,"1536"	,"1530"	,"1531"	,"1537"	,"1532"	,"1533"	,"1538"	,
                "1539"	,"1540"	,"1541"	,"1543"	,"1542"	,"1548"	,"1707"	,"1708"	,"1709"	,"17001"	,"17002"	,"1727"	,
                "1728"	,"1729"	,"1737"	,"1738"	,"1739"	,"1748"	,"1759"	,"1749"	,"1808"	,"1809"	,"1821"	,"1820"	,
                "179"	,"185"	,"1869"	,"1887"	,"1888"	,"1889"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,
                "20150"	,"20151"	,"20152"	,"20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,
                "20190"	,"20195"	,"20198"	,"20400"	,"20401"	,"2106"	,"2113"	,"2114"	,"2112"	,"2115"	,"2116"	,
                "2117"	,"2123"	,"2126"	,"2127"	,"2136"	,"2148"	,"22809"	,"22801"	,"22802"	,"22804"	,"2281"	,
                "2150"	,"2189"	,"2230"	,"2231"	,"2252"	,"2254"	,"2250"	,"2251"	,"2253"	,"2270"	,"2273"	,"25021"	,
                "25031"	,"25033"	,"25011"	,"25013"	,"25041"	,"25043"	,"25061"	,"25071"	,"25073"	,"25081"	,
                "25083"	,"25091"	,"25001"	,"25003"	,"25020"	,"25030"	,"25010"	,"25012"	,"25040"	,"25042"	,
                "25050"	,"25060"	,"25070"	,"25072"	,"25080"	,"25082"	,"25000"	,"25002"	,"34550"	,"34500"	,
                "34510"	,"34511"	,"34560"	,"34570"	,"34580"	,"34590"	,"34591"	,"3453"	,"3452"	,"34540"	,
                "3910"	,"3912"	,"3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,"3959"	,
                "3970"	,"3960"	,"3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"40201"	,
                "40211"	,"40291"	,"40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,"40492"	,
                "40413"	,"40493"	,"40490"	,"4019"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,
                "41012"	,"41021"	,"41031"	,"41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,
                "41091"	,"41092"	,"41071"	,"41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,
                "41410"	,"41419"	,"4148"	,"4149"	,"430"	,"431"	,"4321"	,"4320"	,"4329"	,"43301"	,"43311"	,
                "43321"	,"43331"	,"43391"	,"43401"	,"43411"	,"43491"	,"436"	,"43320"	,"43300"	,"43310"	,
                "43330"	,"43380"	,"43400"	,"43410"	,"43490"	,"4373"	,"4370"	,"4372"	,"4375"	,"4371"	,"4378"	,
                "4379"	,"438"	,"4280"	,"4281"	,"4289"	,"4210"	,"4219"	,"4240"	,"4241"	,"4242"	,"591"	,"59372"	,
                "59373"	,"5996"	,"5845"	,"5849"	,"5859"	,"5851"	,"586"	,"5920"	,"5921"	,"5929"	,"5941"	,"5989"	,
                "4918"	,"4919"	,"4920"	,"4928"	,"49121"	,"49120"	,"49320"	,"49321"	,"496"	,"49300"	,"49310"	,
                "49390"	,"49301"	,"49391"	,"53100"	,"53110"	,"53120"	,"53140"	,"53150"	,"53151"	,"53160"	,
                "53170"	,"53190"	,"53191"	,"53200"	,"53210"	,"53220"	,"53240"	,"53241"	,"53250"	,"53251"	,
                "53260"	,"53271"	,"53290"	,"53300"	,"53310"	,"53340"	,"53341"	,"53350"	,"53360"	,"53390"	,
                "53410"	,"53450"	,"76401"	,"76402"	,"76404"	,"76407"	,"76429"	,"76490"	,"76491"	,"76492"	,
                "76493"	,"76494"	,"76495"	,"76496"	,"76497"	,"76498"	,"76499"	,"76500"	,"76501"	,"76502"	,
                "76503"	,"76511"	,"76512"	,"76514"	,"76516"	,"76518"	,"7660"	,"7661"	,"7662"	,"7670"	,"7671"	,
                "7682"	,"7683"	,"7684"	,"7685"	,"7689"	,"7700"	,"7701"	,"7702"	,"7703"	,"7707"	,"7704"	,"7705"	,
                "7708"	,"7709"	,"7711"	,"7717"	,"7712"	,"7714"	,"7718"	,"7720"	,"7721"	,"7722"	,"7760"	,"7725"	,
                "7726"	,"7728"	,"7730"	,"7731"	,"7732"	,"7780"	,"7747"	,"7744"	,"7762"	,"7761"	,"7764"	,"7765"	,
                "7750"	,"7756"	,"7758"	,"7757"	,"7775"	,"7776"	,"7792"	,"7680"	,"7681"	,"7400"	,"7402"	,"7424"	,
                "74101"	,"74102"	,"74103"	,"74100"	,"74192"	,"74193"	,"74190"	,"74253"	,"74259"	,"7428"	,
                "7429"	,"74483"	,"74489"	,"7450"	,"74511"	,"74510"	,"74519"	,"74512"	,"7458"	,"7454"	,"7455"	,
                "74569"	,"7452"	,"74601"	,"74602"	,"74609"	,"7461"	,"7462"	,"7463"	,"7464"	,"7465"	,"7466"	,
                "7467"	,"74687"	,"74685"	,"74686"	,"74684"	,"74689"	,"7469"	,"7470"	,"74710"	,"74722"	,
                "74729"	,"7473"	,"74741"	,"74742"	,"74740"	,"7475"	,"74761"	,"74769"	,"74781"	,"74782"	,
                "7483"	,"7484"	,"7485"	,"74869"	,"74860"	,"74900"	,"74910"	,"75016"	,"75029"	,"7503"	,"7504"	,
                "7511"	,"7512"	,"7510"	,"7513"	,"7514"	,"7515"	,"75161"	,"75169"	,"75162"	,"7518"	,"7530"	,
                "75314"	,"75313"	,"75312"	,"75315"	,"75317"	,"75319"	,"75310"	,"7532"	,"7534"	,"7533"	,"7536"	,
                "7538"	,"75563"	,"75470"	,"7540"	,"7542"	,"75481"	,"75489"	,"75500"	,"75529"	,"75534"	,
                "75560"	,"75569"	,"75600"	,"75608"	,"75609"	,"75615"	,"75619"	,"75656"	,"75651"	,"75659"	,
                "7564"	,"75650"	,"7566"	,"7567"	,"75683"	,"7569"	,"7571"	,"75739"	,"23770"	,"23771"	,"23772"	,
                "7595"	,"7596"	,"75987"	,"75981"	,"75986"	,"75985"	,"75982"	,"75984"	,"7590"	,"7591"	,"7593"	,
                "7594"	,"7597"	,"75989"	,"7599"	,"7580"	,"7582"	,"7581"	,"7583"	,"7585"	,"7586"	,"7588"	,"7589")
  
  raymontamdf = adf[which(is.element(adf$icdd,raymontam)&(!is.element(adf$ageband,c("75.79","80 and over")))),]
  raymontamndf = adf[which(!is.element(adf$icdd,raymontam)&(!is.element(adf$ageband,c("75.79","80 and over")))),]
  mraymontamdf = mdf[which(is.element(mdf$icdd,raymontam)&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
  mraymontamndf = mdf[which(!is.element(mdf$icdd,raymontam)&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
  nmraymontamdf = nmdf[which(is.element(nmdf$icdd,raymontam)&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
  nmraymontamndf = nmdf[which(!is.element(nmdf$icdd,raymontam)&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
  
  praymontamdf = pdf[which(is.element(pdf$icdd,raymontam)&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
  praymontamndf = pdf[which(!is.element(pdf$icdd,raymontam)&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
  npraymontamdf = npdf[which(is.element(npdf$icdd,raymontam)&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
  npraymontamndf = npdf[which(!is.element(npdf$icdd,raymontam)&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
  asraymontamdf = asdf[which(is.element(asdf$icdd,raymontam)&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
  asraymontamndf = asdf[which(!is.element(asdf$icdd,raymontam)&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
  nasraymontamdf = nasdf[which(is.element(nasdf$icdd,raymontam)&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
  nasraymontamndf = nasdf[which(!is.element(nasdf$icdd,raymontam)&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
  npnmraymontamdf = npnmdf[which(is.element(npnmdf$icdd,raymontam)&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
  npnmraymontamndf = npnmdf[which(!is.element(npnmdf$icdd,raymontam)&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
  
  daagg = aggregate(DOD~GENDER+year+ageband,data = raymontamdf, FUN = function(x){NROW(x)})
  csvcreator("","raymontamtotal",daagg,0)
  aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontamdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","raymontamtotal",aagg,i)
  }
  csvcreator("dep06","raymontamtotal",aagg,12)
  csvcreator("dep06","raymontamtotal",aagg,34)
  csvcreator("dep06","raymontamtotal",aagg,56)
  csvcreator("dep06","raymontamtotal",aagg,78)
  csvcreator("dep06","raymontamtotal",aagg,910)
  
  dmagg = aggregate(DOD~GENDER+year+ageband,data = mraymontamdf, FUN = function(x){NROW(x)})
  csvcreator("","raymontammaori",dmagg,0)
  magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","raymontammaori",magg,i)
  }
  csvcreator("dep06","raymontammaori",magg,12)
  csvcreator("dep06","raymontammaori",magg,34)
  csvcreator("dep06","raymontammaori",magg,56)
  csvcreator("dep06","raymontammaori",magg,78)
  csvcreator("dep06","raymontammaori",magg,910)
  #magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamdf, FUN = function(x){NROW(x)})
  
  dnmagg = aggregate(DOD~GENDER+year+ageband,data = nmraymontamdf, FUN = function(x){NROW(x)})
  csvcreator("","raymontamnonmaori",dnmagg,0)
  nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontamdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","raymontamnonmaori",nmagg,i)
  }
  csvcreator("dep06","raymontamnonmaori",nmagg,12)
  csvcreator("dep06","raymontamnonmaori",nmagg,34)
  csvcreator("dep06","raymontamnonmaori",nmagg,56)
  csvcreator("dep06","raymontamnonmaori",nmagg,78)
  csvcreator("dep06","raymontamnonmaori",nmagg,910)
  #nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontamdf, FUN = function(x){NROW(x)})
  
  
  #aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontamndf, FUN = function(x){NROW(x)})
  #maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamndf, FUN = function(x){NROW(x)})
  #nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontamndf, FUN = function(x){NROW(x)})
  
  daaggn = aggregate(DOD~GENDER+year+ageband,data = raymontamndf, FUN = function(x){NROW(x)})
  csvcreator("","notraymontamtotal",daaggn,0)
  aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontamndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","notraymontamtotal",aaggn,i)
  }
  csvcreator("dep06","notraymontamtotal",aaggn,12)
  csvcreator("dep06","notraymontamtotal",aaggn,34)
  csvcreator("dep06","notraymontamtotal",aaggn,56)
  csvcreator("dep06","notraymontamtotal",aaggn,78)
  csvcreator("dep06","notraymontamtotal",aaggn,910)
  
  dmaggn = aggregate(DOD~GENDER+year+ageband,data = mraymontamndf, FUN = function(x){NROW(x)})
  csvcreator("","notraymontammaori",dmaggn,0)
  maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","notraymontammaori",maggn,i)
  }
  csvcreator("dep06","notraymontammaori",maggn,12)
  csvcreator("dep06","notraymontammaori",maggn,34)
  csvcreator("dep06","notraymontammaori",maggn,56)
  csvcreator("dep06","notraymontammaori",maggn,78)
  csvcreator("dep06","notraymontammaori",maggn,910)
  #magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamdf, FUN = function(x){NROW(x)})
  
  dnmaggn = aggregate(DOD~GENDER+year+ageband,data = nmraymontamndf, FUN = function(x){NROW(x)})
  csvcreator("","notraymontamnonmaori",dnmaggn,0)
  nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontamndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","notraymontamnonmaori",nmaggn,i)
  }
  csvcreator("dep06","notraymontamnonmaori",nmaggn,12)
  csvcreator("dep06","notraymontamnonmaori",nmaggn,34)
  csvcreator("dep06","notraymontamnonmaori",nmaggn,56)
  csvcreator("dep06","notraymontamnonmaori",nmaggn,78)
  csvcreator("dep06","notraymontamnonmaori",nmaggn,910)
}
{
  tobias = c(paste("A",15:19,sep=""),paste("A",38:41,sep=""),"A46","A481",paste("B",50:54,sep=""),"B90",
             "G00","G03","J020",paste("J",13:15,sep=""),"J18","L03",paste("C",18:21,sep=""),
             paste("C",43:44,sep=""),"C50",paste("C",53:55,sep=""),"C67","C73","C81","C910",
             paste("C",95:97,sep=""),paste("D",10:36,sep=""),paste("E0",0:7,sep=""),paste("G",40:41,sep=""),
             paste("K",35:38,sep=""),paste("K",40:46,sep=""),paste("K",80:83,sep=""),"K85","K86","K915",
             paste("E",10:14,sep=""),"H311",paste("O0",1:9,sep=""),paste("O",10:99,sep=""),"P00",
             paste("P0",3:9,sep=""),paste("P",10:95,sep=""),paste("Q0",0:9,sep=""),paste("Q",10:99,sep=""),
             paste("I0",1:9,sep=""),paste("I",11:13,sep=""),paste("I",20:25,sep=""),paste("I",60:69,sep=""),
             paste("J",40:46,sep=""),paste("K",25:28,sep=""),paste("N0",0:9,sep=""),"N13",
             paste("N",19:21,sep=""),"N35","N40","N991"
             #icd 9
             ,"1123"  ,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,
             "1196"	,"1304"	,"1390"	,"1500"	,"1501"	,"1502"	,"1503"	,"1504"	,"1505"	,"1510"	,"1511"	,"1512"	,
             "1513"	,"1514"	,"1515"	,"1516"	,"1520"	,"1521"	,"1522"	,"1523"	,"1550"	,"1551"	,"1552"	,"1570"	,
             "1571"	,"1572"	,"1573"	,"1574"	,"1580"	,"1590"	,"1591"	,"1600"	,"1601"	,"1602"	,"1603"	,"1610"	,
             "1611"	,"1612"	,"1620"	,"1622"	,"1623"	,"1624"	,"1625"	,"1630"	,"1631"	,"1720"	,"1721"	,"1722"	,
             "1723"	,"1724"	,"1725"	,"1726"	,"1401"	,"1404"	,"1701"	,"1702"	,"1703"	,"1704"	,"1706"	,"1710"	,
             "1712"	,"1713"	,"1714"	,"1715"	,"1716"	,"1730"	,"1731"	,"1732"	,"1733"	,"1734"	,"1735"	,"1736"	,
             "1740"	,"1741"	,"1742"	,"1743"	,"1744"	,"1745"	,"1746"	,"1765"	,"1800"	,"1801"	,"1880"	,"1881"	,
             "1882"	,"1883"	,"1884"	,"1885"	,"1886"	,"1890"	,"1891"	,"1892"	,"1893"	,"1894"	,"1895"	,"360"	,
             "363"	,"362"	,"382"	,"380"	,"381"	,"3841"	,"383"	,"3842"	,"3840"	,"3849"	,"388"	,"389"	,
             "461"	,"462"	,"463"	,"460"	,"1372"	,"1373"	,"1370"	,"3200"	,"3203"	,"340"	,"481"	,"4822"	,
             "4820"	,"4821"	,"4824"	,"48230"	,"48231"	,"48239"	,"48282"	,"48283"	,"48289"	,"4829"	,"485"	,
             "486"	,"68100"	,"68111"	,"6823"	,"6824"	,"6826"	,"6827"	,"6822"	,"6828"	,"6829"	,"1534"	,"1535"	,
             "1536"	,"1530"	,"1531"	,"1537"	,"1532"	,"1533"	,"1538"	,"1539"	,"1540"	,"1541"	,"1543"	,"1542"	,
             "1548"	,"1727"	,"1728"	,"1729"	,"1737"	,"1738"	,"1739"	,"1748"	,"1759"	,"1749"	,"1808"	,"1809"	,
             "1821"	,"1820"	,"179"	,"1887"	,"1888"	,"1889"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,
             "20150"	,"20151"	,"20152"	,"20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,
             "20190"	,"20195"	,"20198"	,"20400"	,"20401"	,"20410"	,"20240"	,"20480"	,"20490"	,"20800"	,
             "20810"	,"20880"	,"20890"	,"20250"	,"20260"	,"20290"	,"2106"	,"2113"	,"2114"	,"2112"	,"2115"	,
             "2116"	,"2117"	,"2123"	,"2126"	,"2127"	,"2136"	,"2148"	,"22809"	,"22801"	,"22802"	,"22804"	,
             "2281"	,"2150"	,"2189"	,"2230"	,"2231"	,"2252"	,"2254"	,"2250"	,"2251"	,"2253"	,"2270"	,"2273"	,
             "2449"	,"2410"	,"2411"	,"2409"	,"24200"	,"24220"	,"24221"	,"24230"	,"24290"	,"24291"	,"2452"	,
             "34550"	,"34500"	,"34510"	,"34511"	,"34560"	,"34570"	,"34580"	,"34590"	,"34591"	,"3453"	,
             "3452"	,"34540"	,"5400"	,"5401"	,"5409"	,"541"	,"55012"	,"55092"	,"55010"	,"55000"	,"55001"	,
             "55200"	,"55100"	,"55300"	,"5521"	,"5511"	,"55220"	,"55221"	,"55321"	,"5523"	,"5533"	,"5528"	,
             "55229"	,"5518"	,"5538"	,"5529"	,"57400"	,"57401"	,"57410"	,"57411"	,"57420"	,"57421"	,
             "57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,"5750"	,"5751"	,"5761"	,"5762"	,"5764"	,
             "5768"	,"5769"	,"5770"	,"5771"	,"5772"	,"5778"	,"5779"	,"25021"	,"25031"	,"25033"	,"25011"	,
             "25013"	,"25041"	,"25043"	,"25061"	,"25071"	,"25073"	,"25081"	,"25083"	,"25091"	,"25001"	,
             "25003"	,"25020"	,"25030"	,"25010"	,"25012"	,"25040"	,"25042"	,"25050"	,"25060"	,"25070"	,
             "25072"	,"25080"	,"25082"	,"25000"	,"25002"	,"64123"	,"66511"	,"67002"	,"64671"	,"67131"	,
             "67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,"64863"	,"64893"	,"76401"	,
             "76402"	,"76404"	,"76407"	,"76429"	,"76490"	,"76491"	,"76492"	,"76493"	,"76494"	,"76495"	,
             "76496"	,"76497"	,"76498"	,"76499"	,"76500"	,"76501"	,"76502"	,"76503"	,"76511"	,"76512"	,
             "76514"	,"76516"	,"76518"	,"7660"	,"7661"	,"7662"	,"7670"	,"7671"	,"7682"	,"7683"	,"7684"	,
             "7685"	,"7689"	,"7700"	,"7701"	,"7702"	,"7703"	,"7707"	,"7704"	,"7705"	,"7708"	,"7709"	,"7711"	,
             "7717"	,"7712"	,"7714"	,"7718"	,"7720"	,"7721"	,"7722"	,"7760"	,"7725"	,"7726"	,"7728"	,"7730"	,
             "7731"	,"7732"	,"7780"	,"7747"	,"7744"	,"7762"	,"7761"	,"7764"	,"7765"	,"7750"	,"7756"	,"7758"	,
             "7757"	,"7775"	,"7776"	,"7792"	,"7680"	,"7681"	,"7400"	,"7402"	,"7424"	,"74101"	,"74102"	,
             "74103"	,"74100"	,"74192"	,"74193"	,"74190"	,"74253"	,"74259"	,"7428"	,"7429"	,"74483"	,
             "74489"	,"7450"	,"74511"	,"74510"	,"74519"	,"74512"	,"7458"	,"7454"	,"7455"	,"74569"	,
             "7452"	,"74601"	,"74602"	,"74609"	,"7461"	,"7462"	,"7463"	,"7464"	,"7465"	,"7466"	,"7467"	,
             "74687"	,"74685"	,"74686"	,"74684"	,"74689"	,"7469"	,"7470"	,"74710"	,"74722"	,"74729"	,
             "7473"	,"74741"	,"74742"	,"74740"	,"7475"	,"74761"	,"74769"	,"74781"	,"74782"	,"7483"	,
             "7484"	,"7485"	,"74869"	,"74860"	,"74900"	,"74910"	,"75016"	,"75029"	,"7503"	,"7504"	,"7511"	,
             "7512"	,"7510"	,"7513"	,"7514"	,"7515"	,"75161"	,"75169"	,"75162"	,"7518"	,"7530"	,"75314"	,
             "75313"	,"75312"	,"75315"	,"75317"	,"75319"	,"75310"	,"7532"	,"7534"	,"7533"	,"7536"	,"7538"	,
             "75563"	,"75470"	,"7540"	,"7542"	,"75481"	,"75489"	,"75500"	,"75529"	,"75534"	,"75560"	,
             "75569"	,"75600"	,"75608"	,"75609"	,"75615"	,"75619"	,"75656"	,"75651"	,"75659"	,"7564"	,
             "75650"	,"7566"	,"7567"	,"75683"	,"7569"	,"7571"	,"75739"	,"23770"	,"23771"	,"23772"	,"7595"	,
             "7596"	,"75987"	,"75981"	,"75986"	,"75985"	,"75982"	,"75984"	,"7590"	,"7591"	,"7593"	,"7594"	,
             "7597"	,"75989"	,"7599"	,"7580"	,"7582"	,"7581"	,"7583"	,"7585"	,"7586"	,"7588"	,"7589"	,"3910"	,
             "3912"	,"3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,"3959"	,"3970"	,
             "3960"	,"3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"40201"	,"40211"	,
             "40291"	,"40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,"40492"	,"40413"	,
             "40493"	,"40490"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,"41012"	,"41021"	,
             "41031"	,"41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,"41091"	,"41092"	,
             "41071"	,"41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,"41410"	,"41419"	,
             "4148"	,"4149"	,"430"	,"431"	,"4321"	,"4320"	,"4329"	,"43301"	,"43311"	,"43321"	,"43331"	,
             "43391"	,"43401"	,"43411"	,"43491"	,"436"	,"43320"	,"43300"	,"43310"	,"43330"	,"43380"	,
             "43400"	,"43410"	,"43490"	,"4373"	,"4370"	,"4372"	,"4375"	,"4371"	,"4378"	,"4379"	,"438"	,"4918"	,
             "4919"	,"4920"	,"4928"	,"49121"	,"49120"	,"49320"	,"49321"	,"496"	,"49300"	,"49310"	,"49390"	,
             "49301"	,"49391"	,"53100"	,"53110"	,"53120"	,"53140"	,"53150"	,"53151"	,"53160"	,"53170"	,
             "53190"	,"53191"	,"53200"	,"53210"	,"53220"	,"53240"	,"53241"	,"53250"	,"53251"	,"53260"	,
             "53271"	,"53290"	,"53300"	,"53310"	,"53340"	,"53341"	,"53350"	,"53360"	,"53390"	,"53410"	,
             "53450"	,"58089"	,"5809"	,"5804"	,"5834"	,"5821"	,"5822"	,"58289"	,"5829"	,"5813"	,"5811"	,"5812"	,
             "5819"	,"5831"	,"5832"	,"5830"	,"5836"	,"58389"	,"5839"	,"591"	,"59372"	,"59373"	,"5996"	,"586"	,
             "5920"	,"5921"	,"5929"	,"5941"	,"5989")
  
  tobiasdf = adf[which(is.element(adf$icdd,tobias)&(!is.element(adf$ageband,c("75.79","80 and over")))),]
  tobiasndf = adf[which(!is.element(adf$icdd,tobias)&(!is.element(adf$ageband,c("75.79","80 and over")))),]
  mtobiasdf = mdf[which(is.element(mdf$icdd,tobias)&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
  mtobiasndf = mdf[which(!is.element(mdf$icdd,tobias)&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
  nmtobiasdf = nmdf[which(is.element(nmdf$icdd,tobias)&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
  nmtobiasndf = nmdf[which(!is.element(nmdf$icdd,tobias)&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
  
  ptobiasdf = pdf[which(is.element(pdf$icdd,tobias)&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
  ptobiasndf = pdf[which(!is.element(pdf$icdd,tobias)&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
  nptobiasdf = npdf[which(is.element(npdf$icdd,tobias)&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
  nptobiasndf = npdf[which(!is.element(npdf$icdd,tobias)&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
  astobiasdf = asdf[which(is.element(asdf$icdd,tobias)&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
  astobiasndf = asdf[which(!is.element(asdf$icdd,tobias)&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
  nastobiasdf = nasdf[which(is.element(nasdf$icdd,tobias)&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
  nastobiasndf = nasdf[which(!is.element(nasdf$icdd,tobias)&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
  npnmtobiasdf = npnmdf[which(is.element(npnmdf$icdd,tobias)&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
  npnmtobiasndf = npnmdf[which(!is.element(npnmdf$icdd,tobias)&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
  
  daagg = aggregate(DOD~GENDER+year+ageband,data = tobiasdf, FUN = function(x){NROW(x)})
  csvcreator("","tobiastotal",daagg,0)
  aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = tobiasdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","tobiastotal",aagg,i)
  }
  csvcreator("dep06","tobiastotal",aagg,12)
  csvcreator("dep06","tobiastotal",aagg,34)
  csvcreator("dep06","tobiastotal",aagg,56)
  csvcreator("dep06","tobiastotal",aagg,78)
  csvcreator("dep06","tobiastotal",aagg,910)
  
  dmagg = aggregate(DOD~GENDER+year+ageband,data = mtobiasdf, FUN = function(x){NROW(x)})
  csvcreator("","tobiasmaori",dmagg,0)
  magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","tobiasmaori",magg,i)
  }
  csvcreator("dep06","tobiasmaori",magg,12)
  csvcreator("dep06","tobiasmaori",magg,34)
  csvcreator("dep06","tobiasmaori",magg,56)
  csvcreator("dep06","tobiasmaori",magg,78)
  csvcreator("dep06","tobiasmaori",magg,910)
  #magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasdf, FUN = function(x){NROW(x)})
  
  dnmagg = aggregate(DOD~GENDER+year+ageband,data = nmtobiasdf, FUN = function(x){NROW(x)})
  csvcreator("","tobiasnonmaori",dnmagg,0)
  nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmtobiasdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","tobiasnonmaori",nmagg,i)
  }
  csvcreator("dep06","tobiasnonmaori",nmagg,12)
  csvcreator("dep06","tobiasnonmaori",nmagg,34)
  csvcreator("dep06","tobiasnonmaori",nmagg,56)
  csvcreator("dep06","tobiasnonmaori",nmagg,78)
  csvcreator("dep06","tobiasnonmaori",nmagg,910)
  #nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmtobiasdf, FUN = function(x){NROW(x)})
  
  
  #aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = tobiasndf, FUN = function(x){NROW(x)})
  #maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasndf, FUN = function(x){NROW(x)})
  #nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmtobiasndf, FUN = function(x){NROW(x)})
  
  daaggn = aggregate(DOD~GENDER+year+ageband,data = tobiasndf, FUN = function(x){NROW(x)})
  csvcreator("","nottobiastotal",daaggn,0)
  aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = tobiasndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","nottobiastotal",aaggn,i)
  }
  csvcreator("dep06","nottobiastotal",aaggn,12)
  csvcreator("dep06","nottobiastotal",aaggn,34)
  csvcreator("dep06","nottobiastotal",aaggn,56)
  csvcreator("dep06","nottobiastotal",aaggn,78)
  csvcreator("dep06","nottobiastotal",aaggn,910)
  
  dmaggn = aggregate(DOD~GENDER+year+ageband,data = mtobiasndf, FUN = function(x){NROW(x)})
  csvcreator("","nottobiasmaori",dmaggn,0)
  maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","nottobiasmaori",maggn,i)
  }
  csvcreator("dep06","nottobiasmaori",maggn,12)
  csvcreator("dep06","nottobiasmaori",maggn,34)
  csvcreator("dep06","nottobiasmaori",maggn,56)
  csvcreator("dep06","nottobiasmaori",maggn,78)
  csvcreator("dep06","nottobiasmaori",maggn,910)
  #magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasdf, FUN = function(x){NROW(x)})
  
  dnmaggn = aggregate(DOD~GENDER+year+ageband,data = nmtobiasndf, FUN = function(x){NROW(x)})
  csvcreator("","nottobiasnonmaori",dnmaggn,0)
  nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmtobiasndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","nottobiasnonmaori",nmaggn,i)
  }
  csvcreator("dep06","nottobiasnonmaori",nmaggn,12)
  csvcreator("dep06","nottobiasnonmaori",nmaggn,34)
  csvcreator("dep06","nottobiasnonmaori",nmaggn,56)
  csvcreator("dep06","nottobiasnonmaori",nmaggn,78)
  csvcreator("dep06","nottobiasnonmaori",nmaggn,910)
}
{
  moh = c(paste("A",15:16,sep=""),"A39","A403",paste("B",20:24,sep=""),"G001","J13","C16",
          paste("C",19:21,sep=""),paste("C",40:41,sep=""),"C43","C50","C53",paste("C",61:62,sep=""),
          "C73","C81","C910",paste("O0",0:9,sep=""),paste("O",10:96,sep=""),paste("O",98:99,sep=""),
          paste("P0",1:3,sep=""),paste("P0",5:9,sep=""),paste("P",10:94,sep=""),"Q21","E09",
          paste("E",10:14,sep=""),"I01",paste("I0",5:9,sep=""),paste("I",10:13,sep=""),
          paste("I",20:26,sep=""),paste("I",33:37,sep=""),"I50",paste("I",60:69,sep=""),
          paste("J",40:46,sep=""),paste("K",25:27,sep=""),"K80",paste("N",17:19,sep=""),
          paste("V0",1:4,sep=""),paste("V0",6:9,sep=""),paste("V",10:14,sep=""),paste("V",16:24,sep=""),
          paste("V",26:34,sep=""),paste("V",36:44,sep=""),paste("V",46:54,sep=""),paste("V",56:64,sep=""),
          paste("V",66:74,sep=""),paste("V",76:79,sep=""),paste("V80",0:5,sep=""),paste("V80",7:9,sep=""),
          paste("V",82:86,sep=""),paste("V87",0:5,sep=""),paste("V87",7:9,sep=""),paste("V88",0:5,sep=""),
          paste("V88",7:9,sep=""),"V89",paste("V",98:99,sep=""),paste("W0",0:8,sep=""),"W18",
          paste("X0",0:9,sep=""),paste("X",60:84,sep=""),paste("Y",60:82,sep="")
          #icd 9
          ,"1123"  ,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,"1196"	,
          "360"	,"363"	,"362"	,"382"	,"3201"	,"481"	,"1518"	,"1519"	,"1540"	,"1541"	,"1543"	,"1542"	,"1548"	,
          "1707"	,"1708"	,"1709"	,"17001"	,"17002"	,"1727"	,"1728"	,"1729"	,"1748"	,"1759"	,"1749"	,"1808"	,
          "1809"	,"185"	,"1869"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,"20150"	,"20151"	,"20152"	,
          "20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,"20190"	,"20195"	,"20198"	,
          "20400"	,"20401"	,"20410"	,"20240"	,"20480"	,"20490"	,"64123"	,"66511"	,"67002"	,"64671"	,
          "67131"	,"67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,"64863"	,"64893"	,"76401"	,
          "76402"	,"76404"	,"76407"	,"76429"	,"76490"	,"76491"	,"76492"	,"76493"	,"76494"	,"76495"	,
          "76496"	,"76497"	,"76498"	,"76499"	,"76500"	,"76501"	,"76502"	,"76503"	,"76511"	,"76512"	,
          "76514"	,"76516"	,"76518"	,"7660"	,"7661"	,"7662"	,"7670"	,"7671"	,"7682"	,"7683"	,"7684"	,"7685"	,
          "7689"	,"7700"	,"7701"	,"7702"	,"7703"	,"7707"	,"7704"	,"7705"	,"7708"	,"7709"	,"7711"	,"7717"	,"7712"	,
          "7714"	,"7718"	,"7720"	,"7721"	,"7722"	,"7760"	,"7725"	,"7726"	,"7728"	,"7730"	,"7731"	,"7732"	,"7780"	,
          "7747"	,"7744"	,"7762"	,"7761"	,"7764"	,"7765"	,"7750"	,"7756"	,"7758"	,"7757"	,"7775"	,"7776"	,"7792"	,
          "7454"	,"7455"	,"74569"	,"7452"	,"25021"	,"25031"	,"25033"	,"25011"	,"25013"	,"25041"	,"25043"	,
          "25061"	,"25071"	,"25073"	,"25081"	,"25083"	,"25091"	,"25001"	,"25003"	,"25020"	,"25030"	,
          "25010"	,"25012"	,"25040"	,"25042"	,"25050"	,"25060"	,"25070"	,"25072"	,"25080"	,"25082"	,
          "25000"	,"25002"	,"3910"	,"3912"	,"3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,
          "3959"	,"3970"	,"3960"	,"3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"4019"	,
          "40201"	,"40211"	,"40291"	,"40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,
          "40492"	,"40413"	,"40493"	,"40490"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,"41012"	,
          "41021"	,"41031"	,"41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,"41091"	,
          "41092"	,"41071"	,"41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,"41410"	,"41419"	,
          "4148"	,"4149"	,"41519"	,"4210"	,"4219"	,"4240"	,"4241"	,"4242"	,"4280"	,"4281"	,"4289"	,"430"	,"431"	,
          "4321"	,"4320"	,"4329"	,"43301"	,"43311"	,"43321"	,"43331"	,"43391"	,"43401"	,"43411"	,"43491"	,
          "436"	,"43320"	,"43300"	,"43310"	,"43330"	,"43380"	,"43400"	,"43410"	,"43490"	,"4373"	,"4370"	,
          "4372"	,"4375"	,"4371"	,"4378"	,"4379"	,"438"	,"4918"	,"4919"	,"4920"	,"4928"	,"49121"	,"49120"	,
          "49320"	,"49321"	,"496"	,"49300"	,"49310"	,"49390"	,"49301"	,"49391"	,"53100"	,"53110"	,"53120"	,
          "53140"	,"53150"	,"53151"	,"53160"	,"53170"	,"53190"	,"53191"	,"53200"	,"53210"	,"53220"	,
          "53240"	,"53241"	,"53250"	,"53251"	,"53260"	,"53271"	,"53290"	,"53300"	,"53310"	,"53340"	,
          "53341"	,"53350"	,"53360"	,"53390"	,"57400"	,"57401"	,"57410"	,"57411"	,"57420"	,"57421"	,
          "57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,"5845"	,"5849"	,"5859"	,"5851"	,"586"	,
          "82270"	,"82271"	,"82273"	,"82276"	,"82278"	,"81475"	,"82804"	,"82903"	,"82570"	,"82573"	,
          "82574"	,"82579"	,"81875"	,"81365"	,"81265"	,"82613"	,"82615"	,"82618"	,"81525"	,"81535"	,
          "81625"	,"81825"	,"81635"	,"82128"	,"82220"	,"82221"	,"82528"	,"81225"	,"81235"	,"81925"	,
          "82406"	,"82304"	,"82308"	,"82314"	,"81505"	,"81515"	,"81715"	,"81605"	,"81805"	,"81615"	,
          "81815"	,"82104"	,"82108"	,"82114"	,"82211"	,"82502"	,"82503"	,"82504"	,"82508"	,"81205"	,
          "81115"	,"81215"	,"81915"	,"82724"	,"82820"	,"82824"	,"82825"	,"82828"	,"81885"	,"81895"	,
          "80798"	,"82483"	,"82590"	,"82598"	,"81995"	,"82891"	,"84603"	,"88500"	,"88507"	,"88506"	,
          "88504"	,"88505"	,"88508"	,"88509"	,"88697"	,"88604"	,"88699"	,"88460"	,"88467"	,"88466"	,
          "88469"	,"88440"	,"88447"	,"88449"	,"88420"	,"88427"	,"88429"	,"88470"	,"88477"	,"89020"	,
          "89030"	,"89080"	,"89090"	,"89127"	,"89137"	,"89136"	,"89131"	,"89200"	,"89205"	,"89208"	,
          "89500"	,"89400"	,"89300"	,"89380"	,"89810"	,"95000"	,"95007"	,"95008"	,"95009"	,"95010"	,
          "95020"	,"95030"	,"95037"	,"95036"	,"95028"	,"95038"	,"95039"	,"95040"	,"95050"	,"95047"	,
          "95046"	,"95045"	,"95041"	,"95049"	,"87007"	,"87009"	,"87027"	,"87047"	,"87067"	,"87057"	,
          "87087"	,"87187"	,"87257"	,"87407"	,"87687")
  
  mohdf = adf[which(is.element(adf$icdd,moh)&(!is.element(adf$ageband,c("75.79","80 and over")))),]
  mohndf = adf[which(!is.element(adf$icdd,moh)&(!is.element(adf$ageband,c("75.79","80 and over")))),]
  mmohdf = mdf[which(is.element(mdf$icdd,moh)&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
  mmohndf = mdf[which(!is.element(mdf$icdd,moh)&(!is.element(mdf$ageband,c("75.79","80 and over")))),]
  nmmohdf = nmdf[which(is.element(nmdf$icdd,moh)&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
  nmmohndf = nmdf[which(!is.element(nmdf$icdd,moh)&(!is.element(nmdf$ageband,c("75.79","80 and over")))),]
  
  pmohdf = pdf[which(is.element(pdf$icdd,moh)&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
  pmohndf = pdf[which(!is.element(pdf$icdd,moh)&(!is.element(pdf$ageband,c("75.79","80 and over")))),]
  npmohdf = npdf[which(is.element(npdf$icdd,moh)&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
  npmohndf = npdf[which(!is.element(npdf$icdd,moh)&(!is.element(npdf$ageband,c("75.79","80 and over")))),]
  asmohdf = asdf[which(is.element(asdf$icdd,moh)&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
  asmohndf = asdf[which(!is.element(asdf$icdd,moh)&(!is.element(asdf$ageband,c("75.79","80 and over")))),]
  nasmohdf = nasdf[which(is.element(nasdf$icdd,moh)&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
  nasmohndf = nasdf[which(!is.element(nasdf$icdd,moh)&(!is.element(nasdf$ageband,c("75.79","80 and over")))),]
  npnmmohdf = npnmdf[which(is.element(npnmdf$icdd,moh)&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
  npnmmohndf = npnmdf[which(!is.element(npnmdf$icdd,moh)&(!is.element(npnmdf$ageband,c("75.79","80 and over")))),]
  
  daagg = aggregate(DOD~GENDER+year+ageband,data = mohdf, FUN = function(x){NROW(x)})
  csvcreator("","mohtotal",daagg,0)
  aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mohdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","mohtotal",aagg,i)
  }
  csvcreator("dep06","mohtotal",aagg,12)
  csvcreator("dep06","mohtotal",aagg,34)
  csvcreator("dep06","mohtotal",aagg,56)
  csvcreator("dep06","mohtotal",aagg,78)
  csvcreator("dep06","mohtotal",aagg,910)
  
  dmagg = aggregate(DOD~GENDER+year+ageband,data = mmohdf, FUN = function(x){NROW(x)})
  csvcreator("","mohmaori",dmagg,0)
  magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","mohmaori",magg,i)
  }
  csvcreator("dep06","mohmaori",magg,12)
  csvcreator("dep06","mohmaori",magg,34)
  csvcreator("dep06","mohmaori",magg,56)
  csvcreator("dep06","mohmaori",magg,78)
  csvcreator("dep06","mohmaori",magg,910)
  #magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohdf, FUN = function(x){NROW(x)})
  
  dnmagg = aggregate(DOD~GENDER+year+ageband,data = nmmohdf, FUN = function(x){NROW(x)})
  csvcreator("","mohnonmaori",dnmagg,0)
  nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmmohdf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","mohnonmaori",nmagg,i)
  }
  csvcreator("dep06","mohnonmaori",nmagg,12)
  csvcreator("dep06","mohnonmaori",nmagg,34)
  csvcreator("dep06","mohnonmaori",nmagg,56)
  csvcreator("dep06","mohnonmaori",nmagg,78)
  csvcreator("dep06","mohnonmaori",nmagg,910)
  #nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmmohdf, FUN = function(x){NROW(x)})
  
  
  #aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mohndf, FUN = function(x){NROW(x)})
  #maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohndf, FUN = function(x){NROW(x)})
  #nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmmohndf, FUN = function(x){NROW(x)})
  
  daaggn = aggregate(DOD~GENDER+year+ageband,data = mohndf, FUN = function(x){NROW(x)})
  csvcreator("","notmohtotal",daaggn,0)
  aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mohndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","notmohtotal",aaggn,i)
  }
  csvcreator("dep06","notmohtotal",aaggn,12)
  csvcreator("dep06","notmohtotal",aaggn,34)
  csvcreator("dep06","notmohtotal",aaggn,56)
  csvcreator("dep06","notmohtotal",aaggn,78)
  csvcreator("dep06","notmohtotal",aaggn,910)
  
  dmaggn = aggregate(DOD~GENDER+year+ageband,data = mmohndf, FUN = function(x){NROW(x)})
  csvcreator("","notmohmaori",dmaggn,0)
  maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","notmohmaori",maggn,i)
  }
  csvcreator("dep06","notmohmaori",maggn,12)
  csvcreator("dep06","notmohmaori",maggn,34)
  csvcreator("dep06","notmohmaori",maggn,56)
  csvcreator("dep06","notmohmaori",maggn,78)
  csvcreator("dep06","notmohmaori",maggn,910)
  #magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohdf, FUN = function(x){NROW(x)})
  
  dnmaggn = aggregate(DOD~GENDER+year+ageband,data = nmmohndf, FUN = function(x){NROW(x)})
  csvcreator("","notmohnonmaori",dnmaggn,0)
  nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmmohndf, FUN = function(x){NROW(x)})
  for(i in 1:10){
    csvcreator("dep06","notmohnonmaori",nmaggn,i)
  }
  csvcreator("dep06","notmohnonmaori",nmaggn,12)
  csvcreator("dep06","notmohnonmaori",nmaggn,34)
  csvcreator("dep06","notmohnonmaori",nmaggn,56)
  csvcreator("dep06","notmohnonmaori",nmaggn,78)
  csvcreator("dep06","notmohnonmaori",nmaggn,910)
}

##################
##################
#corrected up to this point
##################
##################
{
pagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = ppearcedf, FUN = function(x){NROW(x)})
npagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nppearcedf, FUN = function(x){NROW(x)})
asagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = aspearcedf, FUN = function(x){NROW(x)})
nasagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = naspearcedf, FUN = function(x){NROW(x)})
npnmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmpearcedf, FUN = function(x){NROW(x)})

paggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = ppearcendf, FUN = function(x){NROW(x)})
npaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nppearcendf, FUN = function(x){NROW(x)})
asaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = aspearcendf, FUN = function(x){NROW(x)})
nasaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = naspearcendf, FUN = function(x){NROW(x)})
npnmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmpearcendf, FUN = function(x){NROW(x)})

csvcreator("pearce")

aagg = NULL
magg = NULL
nmagg = NULL

pagg = NULL
npagg = NULL
asagg = NULL
nasagg = NULL
npnmagg = NULL

aaggn = NULL
maggn = NULL
nmaggn = NULL

paggn = NULL
npaggn = NULL
asaggn = NULL
nasaggn = NULL
npnmaggn = NULL

allagg = NULL
maoriagg = NULL
nonmaoriagg = NULL

pacificagg = NULL
nonpacificagg = NULL
asianagg = NULL
nonasianagg = NULL
nonpacificnonmaoriagg = NULL

allaggn = NULL
maoriaggn = NULL
nonmaoriaggn = NULL

pacificaggn = NULL
nonpacificaggn = NULL
asianaggn = NULL
nonasianaggn = NULL
nonpacificnonmaoriaggn = NULL

raymontav = c(paste("A",15:19,sep=""),"B90",paste("A",38:41,sep=""),"A46","A481",paste("B",50:54,sep=""),
              "G00","G03","J020",paste("J",13:15,sep=""),"J18","L03",paste("B",15:24,sep=""),"J10","J12",
              "J171","J21",paste("E0",0:7,sep=""),"I26","I802",paste("K",35:38,sep=""),
              paste("K",40:46,sep=""),paste("K",80:83,sep=""),paste("K",85:86,sep=""),"K915",
              paste("O0",0:9,sep=""),paste("O",10:99,sep=""),paste("C0",0:9,sep=""),paste("C",10:16,sep=""),
              paste("C",18:22,sep=""),paste("C",33:34,sep=""),paste("C",43:44,sep=""),"C50",
              paste("C",53:55,sep=""),"C67","C73","C81",paste("C91",0:1,sep=""),paste("D",10:36,sep=""),
              paste("E",10:14,sep=""),paste("G",40:41,sep=""),paste("I0",0:9,sep=""),paste("I",11:13,sep=""),
              paste("I",20:25,sep=""),paste("I",60:69,sep=""),"I71",paste("N0",0:9,sep=""),"N13",
              paste("N",17:21,sep=""),"N35","N40","N991",paste("J",40:46,sep=""),paste("K",25:28,sep=""),
              paste("K",73:74,sep=""),"H311","P00",paste("P0",3:9,sep=""),paste("P",10:95,sep=""),
              paste("Q0",0:9,sep=""),paste("Q",10:99,sep=""),paste("F",10:16,sep=""),paste("F",18:19,sep=""),
              "I426","K292","K70",paste("V0",1:4,sep=""),"V06","V09",paste("V",10:80,sep=""),"V87","V89",
              "V99",paste("W0",0:9,sep=""),paste("W",10:19,sep=""),paste("W",65:74,sep=""),
              paste("X0",0:9,sep=""),paste("X",40:49,sep=""),paste("X",60:99,sep=""),paste("Y0",0:9,sep=""),
              paste("Y87",0:1,sep="")
              #icd 9
              ,"1123"	,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,
              "1196"	,"1304"	,"1390"	,"1500"	,"1501"	,"1502"	,"1503"	,"1504"	,"1505"	,"1510"	,"1511"	,"1512"	,
              "1513"	,"1514"	,"1515"	,"1516"	,"1520"	,"1521"	,"1522"	,"1523"	,"1550"	,"1551"	,"1552"	,"1570"	,
              "1571"	,"1572"	,"1573"	,"1574"	,"1580"	,"1590"	,"1591"	,"1600"	,"1601"	,"1602"	,"1603"	,"1610"	,
              "1611"	,"1612"	,"1620"	,"1622"	,"1623"	,"1624"	,"1625"	,"1630"	,"1631"	,"1720"	,"1721"	,"1722"	,
              "1723"	,"1724"	,"1725"	,"1726"	,"1401"	,"1404"	,"1701"	,"1702"	,"1703"	,"1704"	,"1706"	,"1710"	,
              "1712"	,"1713"	,"1714"	,"1715"	,"1716"	,"1730"	,"1731"	,"1732"	,"1733"	,"1734"	,"1735"	,"1736"	,
              "1740"	,"1741"	,"1742"	,"1743"	,"1744"	,"1745"	,"1746"	,"1765"	,"1800"	,"1801"	,"1880"	,"1881"	,
              "1882"	,"1883"	,"1884"	,"1885"	,"1886"	,"1890"	,"1891"	,"1892"	,"1893"	,"1894"	,"1895"	,"360"	,
              "363"	,"362"	,"382"	,"380"	,"381"	,"3841"	,"383"	,"3842"	,"3840"	,"3849"	,"388"	,"389"	,
              "461"	,"462"	,"463"	,"460"	,"1372"	,"1373"	,"1370"	,"3200"	,"3201"	,"3202"	,"3203"	,"3209"	,
              "32082"	,"3222"	,"3229"	,"340"	,"481"	,"4822"	,"4820"	,"4821"	,"4824"	,"48230"	,"48231"	,
              "48239"	,"48282"	,"48283"	,"48289"	,"4829"	,"485"	,"486"	,"68100"	,"68111"	,"6823"	,"6824"	,
              "6826"	,"6827"	,"6822"	,"6828"	,"6829"	,"701"	,"7031"	,"7020"	,"7030"	,"7041"	,"7051"	,"7049"	,
              "7033"	,"7022"	,"7032"	,"7044"	,"7054"	,"709"	,"4870"	,"4871"	,"4878"	,"4800"	,"4801"	,"4808"	,
              "4809"	,"4661"	,"2449"	,"2410"	,"2411"	,"2409"	,"24200"	,"24220"	,"24221"	,"24230"	,"24290"	,
              "24291"	,"2452"	,"41519"	,"45119"	,"5400"	,"5401"	,"5409"	,"541"	,"55012"	,"55092"	,"55010"	,
              "55000"	,"55001"	,"55200"	,"55100"	,"55300"	,"5521"	,"5511"	,"55220"	,"55221"	,"55321"	,
              "5523"	,"5533"	,"5528"	,"55229"	,"5518"	,"5538"	,"5529"	,"57400"	,"57401"	,"57410"	,"57411"	,
              "57420"	,"57421"	,"57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,"5750"	,"5751"	,
              "5761"	,"5762"	,"5764"	,"5768"	,"5769"	,"5770"	,"5771"	,"5772"	,"5778"	,"5779"	,"64123"	,"66511"	,
              "67002"	,"64671"	,"67131"	,"67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,
              "64863"	,"64893"	,"1409"	,"1410"	,"1411"	,"1412"	,"1413"	,"1414"	,"1418"	,"1419"	,"1430"	,"1431"	,
              "1440"	,"1441"	,"1448"	,"1449"	,"1452"	,"1453"	,"1454"	,"1455"	,"1450"	,"1451"	,"1456"	,"1458"	,
              "1459"	,"1420"	,"1421"	,"1429"	,"1461"	,"1462"	,"1460"	,"1468"	,"1469"	,"1471"	,"1478"	,"1479"	,
              "1488"	,"1489"	,"1490"	,"1498"	,"1509"	,"1518"	,"1519"	,"1534"	,"1535"	,"1536"	,"1530"	,"1531"	,
              "1537"	,"1532"	,"1533"	,"1538"	,"1539"	,"1540"	,"1541"	,"1543"	,"1542"	,"1548"	,"1628"	,"1629"	,
              "1727"	,"1728"	,"1729"	,"1737"	,"1738"	,"1739"	,"1748"	,"1759"	,"1749"	,"1808"	,"1809"	,"1821"	,
              "1820"	,"179"	,"1887"	,"1888"	,"1889"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,"20150"	,
              "20151"	,"20152"	,"20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,"20190"	,
              "20195"	,"20198"	,"20400"	,"20401"	,"2106"	,"2113"	,"2114"	,"2112"	,"2115"	,"2116"	,"2117"	,
              "2123"	,"2126"	,"2127"	,"2136"	,"2148"	,"22809"	,"22801"	,"22802"	,"22804"	,"2281"	,"2150"	,
              "2189"	,"2230"	,"2231"	,"2252"	,"2254"	,"2250"	,"2251"	,"2253"	,"2270"	,"2273"	,"25021"	,"25031"	,
              "25033"	,"25011"	,"25013"	,"25041"	,"25043"	,"25061"	,"25071"	,"25073"	,"25081"	,"25083"	,
              "25091"	,"25001"	,"25003"	,"25020"	,"25030"	,"25010"	,"25012"	,"25040"	,"25042"	,"25050"	,
              "25060"	,"25070"	,"25072"	,"25080"	,"25082"	,"25000"	,"25002"	,"34550"	,"34500"	,"34510"	,
              "34511"	,"34560"	,"34570"	,"34580"	,"34590"	,"34591"	,"3453"	,"3452"	,"34540"	,"3910"	,"3912"	,
              "3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,"3959"	,"3970"	,"3960"	,
              "3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"40201"	,"40211"	,"40291"	,
              "40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,"40492"	,"40413"	,"40493"	,
              "40490"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,"41012"	,"41021"	,"41031"	,
              "41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,"41091"	,"41092"	,"41071"	,
              "41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,"41410"	,"41419"	,"4148"	,
              "4149"	,"430"	,"431"	,"4321"	,"4320"	,"4329"	,"43301"	,"43311"	,"43321"	,"43331"	,"43391"	,
              "43401"	,"43411"	,"43491"	,"436"	,"43320"	,"43300"	,"43310"	,"43330"	,"43380"	,"43400"	,
              "43410"	,"43490"	,"4373"	,"4370"	,"4372"	,"4375"	,"4371"	,"4378"	,"4379"	,"438"	,"44100"	,
              "44101"	,"44102"	,"44103"	,"4411"	,"4412"	,"4413"	,"4414"	,"4416"	,"4417"	,"4415"	,"4419"	,
              "58089"	,"5809"	,"5804"	,"5834"	,"5821"	,"5822"	,"58289"	,"5829"	,"5813"	,"5811"	,"5812"	,"5819"	,
              "5831"	,"5832"	,"5830"	,"5836"	,"58389"	,"5839"	,"591"	,"59372"	,"59373"	,"5996"	,"5845"	,
              "5849"	,"5859"	,"5851"	,"586"	,"5920"	,"5921"	,"5929"	,"5941"	,"5989"	,"4918"	,"4919"	,"4920"	,
              "4928"	,"49121"	,"49120"	,"49320"	,"49321"	,"496"	,"49300"	,"49310"	,"49390"	,"49301"	,
              "49391"	,"53100"	,"53110"	,"53120"	,"53140"	,"53150"	,"53151"	,"53160"	,"53170"	,"53190"	,
              "53191"	,"53200"	,"53210"	,"53220"	,"53240"	,"53241"	,"53250"	,"53251"	,"53260"	,"53271"	,
              "53290"	,"53300"	,"53310"	,"53340"	,"53341"	,"53350"	,"53360"	,"53390"	,"53410"	,"53450"	,
              "57149"	,"57140"	,"5716"	,"5715"	,"76401"	,"76402"	,"76404"	,"76407"	,"76429"	,"76490"	,
              "76491"	,"76492"	,"76493"	,"76494"	,"76495"	,"76496"	,"76497"	,"76498"	,"76499"	,"76500"	,
              "76501"	,"76502"	,"76503"	,"76511"	,"76512"	,"76514"	,"76516"	,"76518"	,"7660"	,"7661"	,
              "7662"	,"7670"	,"7671"	,"7682"	,"7683"	,"7684"	,"7685"	,"7689"	,"7700"	,"7701"	,"7702"	,"7703"	,
              "7707"	,"7704"	,"7705"	,"7708"	,"7709"	,"7711"	,"7717"	,"7712"	,"7714"	,"7718"	,"7720"	,"7721"	,
              "7722"	,"7760"	,"7725"	,"7726"	,"7728"	,"7730"	,"7731"	,"7732"	,"7780"	,"7747"	,"7744"	,"7762"	,
              "7761"	,"7764"	,"7765"	,"7750"	,"7756"	,"7758"	,"7757"	,"7775"	,"7776"	,"7792"	,"7680"	,"7681"	,
              "7400"	,"7402"	,"7424"	,"74101"	,"74102"	,"74103"	,"74100"	,"74192"	,"74193"	,"74190"	,
              "74253"	,"74259"	,"7428"	,"7429"	,"74483"	,"74489"	,"7450"	,"74511"	,"74510"	,"74519"	,
              "74512"	,"7458"	,"7454"	,"7455"	,"74569"	,"7452"	,"74601"	,"74602"	,"74609"	,"7461"	,"7462"	,
              "7463"	,"7464"	,"7465"	,"7466"	,"7467"	,"74687"	,"74685"	,"74686"	,"74684"	,"74689"	,"7469"	,
              "7470"	,"74710"	,"74722"	,"74729"	,"7473"	,"74741"	,"74742"	,"74740"	,"7475"	,"74761"	,
              "74769"	,"74781"	,"74782"	,"7483"	,"7484"	,"7485"	,"74869"	,"74860"	,"74900"	,"74910"	,
              "75016"	,"75029"	,"7503"	,"7504"	,"7511"	,"7512"	,"7510"	,"7513"	,"7514"	,"7515"	,"75161"	,
              "75169"	,"75162"	,"7518"	,"7530"	,"75314"	,"75313"	,"75312"	,"75315"	,"75317"	,"75319"	,
              "75310"	,"7532"	,"7534"	,"7533"	,"7536"	,"7538"	,"75563"	,"75470"	,"7540"	,"7542"	,"75481"	,
              "75489"	,"75500"	,"75529"	,"75534"	,"75560"	,"75569"	,"75600"	,"75608"	,"75609"	,"75615"	,
              "75619"	,"75656"	,"75651"	,"75659"	,"7564"	,"75650"	,"7566"	,"7567"	,"75683"	,"7569"	,"7571"	,
              "75739"	,"23770"	,"23771"	,"23772"	,"7595"	,"7596"	,"75987"	,"75981"	,"75986"	,"75985"	,
              "75982"	,"75984"	,"7590"	,"7591"	,"7593"	,"7594"	,"7597"	,"75989"	,"7599"	,"7580"	,"7582"	,"7581"	,
              "7583"	,"7585"	,"7586"	,"7588"	,"7589"	,"30300"	,"30301"	,"30302"	,"30500"	,"30501"	,"30502"	,
              "30390"	,"30393"	,"2911"	,"2912"	,"30550"	,"30552"	,"30553"	,"30400"	,"30401"	,"30470"	,
              "30471"	,"30540"	,"30410"	,"30570"	,"30531"	,"30460"	,"30461"	,"30462"	,"30480"	,"30481"	,
              "4255"	,"5710"	,"5711"	,"5712"	,"5713"	,"82270"	,"82271"	,"82273"	,"82276"	,"82278"	,"81475"	,
              "82804"	,"82903"	,"82570"	,"82573"	,"82574"	,"82579"	,"81875"	,"81365"	,"80138"	,"81265"	,
              "82613"	,"82615"	,"82618"	,"81025"	,"81035"	,"81525"	,"81535"	,"81625"	,"81825"	,"81635"	,
              "82128"	,"82220"	,"82221"	,"82528"	,"81225"	,"81235"	,"81925"	,"81005"	,"81015"	,"82406"	,
              "82304"	,"82308"	,"82314"	,"81505"	,"81515"	,"81715"	,"81605"	,"81805"	,"81615"	,"81815"	,
              "82104"	,"82108"	,"82114"	,"82211"	,"82502"	,"82503"	,"82504"	,"82508"	,"81205"	,"81115"	,
              "81215"	,"81915"	,"82724"	,"82820"	,"82824"	,"82825"	,"82828"	,"81885"	,"81895"	,"82483"	,
              "82590"	,"82598"	,"81995"	,"82891"	,"88500"	,"88507"	,"88506"	,"88504"	,"88505"	,"88508"	,
              "88509"	,"88697"	,"88604"	,"88699"	,"88460"	,"88467"	,"88466"	,"88469"	,"88440"	,"88447"	,
              "88449"	,"88420"	,"88427"	,"88429"	,"88090"	,"88097"	,"88096"	,"88094"	,"88015"	,"88098"	,
              "88099"	,"88105"	,"88103"	,"88108"	,"88109"	,"88113"	,"88200"	,"88206"	,"88205"	,"88203"	,
              "88208"	,"88209"	,"88430"	,"88410"	,"88414"	,"88411"	,"88418"	,"88300"	,"88308"	,"88310"	,
              "88390"	,"88397"	,"88315"	,"88391"	,"88398"	,"88470"	,"88477"	,"88490"	,"88800"	,"88497"	,
              "88807"	,"88806"	,"88494"	,"88804"	,"88495"	,"88805"	,"88491"	,"88801"	,"88498"	,"88808"	,
              "88499"	,"88809"	,"91040"	,"91050"	,"91056"	,"91054"	,"91058"	,"91098"	,"91060"	,"91064"	,
              "91024"	,"91018"	,"91028"	,"91038"	,"91070"	,"91074"	,"91071"	,"91078"	,"91080"	,"91084"	,
              "91081"	,"91088"	,"89020"	,"89030"	,"89080"	,"89090"	,"89127"	,"89137"	,"89136"	,"89131"	,
              "89200"	,"89205"	,"89208"	,"89500"	,"89400"	,"89300"	,"89380"	,"89810"	,"85080"	,"85046"	,
              "85086"	,"85049"	,"85320"	,"85420"	,"85500"	,"85010"	,"85020"	,"85017"	,"85027"	,"85029"	,
              "85510"	,"85700"	,"85830"	,"85880"	,"85890"	,"85709"	,"85809"	,"85829"	,"86010"	,"86020"	,
              "86210"	,"86240"	,"86700"	,"86820"	,"86830"	,"86880"	,"86805"	,"86833"	,"86983"	,"86808"	,
              "86818"	,"86828"	,"86988"	,"86338"	,"86407"	,"95000"	,"95007"	,"95008"	,"95009"	,"95010"	,
              "95020"	,"95030"	,"95037"	,"95036"	,"95028"	,"95038"	,"95039"	,"95040"	,"95050"	,"95047"	,
              "95046"	,"95045"	,"95041"	,"95049"	,"95110"	,"95113"	,"95290"	,"95060"	,"95065"	,"95061"	,
              "95070"	,"95090"	,"95097"	,"95099"	,"95300"	,"95310"	,"95307"	,"95317"	,"95387"	,"95306"	,
              "95316"	,"95304"	,"95314"	,"95305"	,"95385"	,"95302"	,"95303"	,"95301"	,"95308"	,"95309"	,
              "95400"	,"95407"	,"95406"	,"95401"	,"95408"	,"95500"	,"95504"	,"95510"	,"95520"	,"95516"	,
              "95514"	,"95524"	,"95515"	,"95525"	,"95522"	,"95511"	,"95521"	,"95518"	,"95528"	,"95540"	,
              "95547"	,"95544"	,"95545"	,"95541"	,"95548"	,"95549"	,"95550"	,"95810"	,"95817"	,"95815"	,
              "95818"	,"95819"	,"95824"	,"95600"	,"95607"	,"95605"	,"95603"	,"95609"	,"95700"	,"95710"	,
              "95707"	,"95717"	,"95716"	,"95715"	,"95713"	,"95718"	,"95728"	,"95806"	,"95805"	,"95803"	,
              "95808"	,"95855"	,"95858"	,"95840"	,"95847"	,"96220"	,"96228"	,"96300"	,"96307"	,"96306"	,
              "96308"	,"96408"	,"96510"	,"96520"	,"96516"	,"96514"	,"96515"	,"96525"	,"96531"	,"96518"	,
              "96528"	,"96540"	,"96546"	,"96543"	,"96548"	,"96800"	,"96807"	,"96808"	,"96600"	,"96607"	,
              "96606"	,"96604"	,"96605"	,"96603"	,"96608"	,"96609"	,"96820"	,"96826"	,"96824"	,"96825"	,
              "96823"	,"96828"	,"96829"	,"96855"	,"96000"	,"96007"	,"96006"	,"96005"	,"96008"	,"96009"	,
              "96720"	,"96725"	,"96700"	,"96704"	,"96709"	,"96730"	,"96710"	,"96790"	,"96799"	,"96889"	,
              "96890"	,"96897"	,"96895"	,"96898"	,"96899"	,"95909"	,"96909")

raymontavdf = adf[which(is.element(substring(adf$icdd,1,3),substring(raymontav,1,3))),]
raymontavndf = adf[which(!is.element(substring(adf$icdd,1,3),substring(raymontav,1,3))),]
mraymontavdf = mdf[which(is.element(substring(mdf$icdd,1,3),substring(raymontav,1,3))),]
mraymontavndf = mdf[which(!is.element(substring(mdf$icdd,1,3),substring(raymontav,1,3))),]
nmraymontavdf = nmdf[which(is.element(substring(nmdf$icdd,1,3),substring(raymontav,1,3))),]
nmraymontavndf = nmdf[which(!is.element(substring(nmdf$icdd,1,3),substring(raymontav,1,3))),]

praymontavdf = pdf[which(is.element(substring(pdf$icdd,1,3),substring(raymontav,1,3))),]
praymontavndf = pdf[which(!is.element(substring(pdf$icdd,1,3),substring(raymontav,1,3))),]
npraymontavdf = npdf[which(is.element(substring(npdf$icdd,1,3),substring(raymontav,1,3))),]
npraymontavndf = npdf[which(!is.element(substring(npdf$icdd,1,3),substring(raymontav,1,3))),]
asraymontavdf = asdf[which(is.element(substring(asdf$icdd,1,3),substring(raymontav,1,3))),]
asraymontavndf = asdf[which(!is.element(substring(asdf$icdd,1,3),substring(raymontav,1,3))),]
nasraymontavdf = nasdf[which(is.element(substring(nasdf$icdd,1,3),substring(raymontav,1,3))),]
nasraymontavndf = nasdf[which(!is.element(substring(nasdf$icdd,1,3),substring(raymontav,1,3))),]
npnmraymontavdf = npnmdf[which(is.element(substring(npnmdf$icdd,1,3),substring(raymontav,1,3))),]
npnmraymontavndf = npnmdf[which(!is.element(substring(npnmdf$icdd,1,3),substring(raymontav,1,3))),]

aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontavdf, FUN = function(x){NROW(x)})
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavdf, FUN = function(x){NROW(x)})
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontavdf, FUN = function(x){NROW(x)})
aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontavndf, FUN = function(x){NROW(x)})
maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontavndf, FUN = function(x){NROW(x)})
nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontavndf, FUN = function(x){NROW(x)})

pagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = praymontavdf, FUN = function(x){NROW(x)})
npagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npraymontavdf, FUN = function(x){NROW(x)})
asagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = asraymontavdf, FUN = function(x){NROW(x)})
nasagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nasraymontavdf, FUN = function(x){NROW(x)})
npnmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmraymontavdf, FUN = function(x){NROW(x)})

paggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = praymontavndf, FUN = function(x){NROW(x)})
npaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npraymontavndf, FUN = function(x){NROW(x)})
asaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = asraymontavndf, FUN = function(x){NROW(x)})
nasaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nasraymontavndf, FUN = function(x){NROW(x)})
npnmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmraymontavndf, FUN = function(x){NROW(x)})

csvcreator("raymontav")

aagg = NULL
magg = NULL
nmagg = NULL

pagg = NULL
npagg = NULL
asagg = NULL
nasagg = NULL
npnmagg = NULL

aaggn = NULL
maggn = NULL
nmaggn = NULL

paggn = NULL
npaggn = NULL
asaggn = NULL
nasaggn = NULL
npnmaggn = NULL

allagg = NULL
maoriagg = NULL
nonmaoriagg = NULL

pacificagg = NULL
nonpacificagg = NULL
asianagg = NULL
nonasianagg = NULL
nonpacificnonmaoriagg = NULL

allaggn = NULL
maoriaggn = NULL
nonmaoriaggn = NULL

pacificaggn = NULL
nonpacificaggn = NULL
asianaggn = NULL
nonasianaggn = NULL
nonpacificnonmaoriaggn = NULL

raymontam = c(paste("A",15:19,sep=""),paste("A",38:41,sep=""),"A46","A481",paste("B",50:54,sep=""),"B90",
              "G00","G03","J020",paste("J",13:15,sep=""),"J18","L03",paste("B",20:24,sep=""),"C16",
              paste("C",18:21,sep=""),paste("C",40:41,sep=""),paste("C",43:44,sep=""),"C50",
              paste("C",53:55,sep=""),paste("C",61:62,sep=""),"C67","C73","C81","C910",paste("D",10:36,sep=""),
              paste("E0",0:7,sep=""),paste("G",40:41,sep=""),paste("K",35:38,sep=""),paste("K",40:46,sep=""),
              paste("K",80:83,sep=""),"K85","K915",paste("E",10:14,sep=""),"H311",paste("O0",0:9,sep=""),
              paste("O",10:96,sep=""),paste("O",98:99,sep=""),paste("P0",0:9,sep=""),paste("P",10:95,sep=""),
              paste("Q0",0:9,sep=""),paste("Q",10:99,sep=""),paste("I0",1:9,sep=""),paste("I",10:13,sep=""),
              paste("I",20:26,sep=""),paste("I",33:37,sep=""),"I50",paste("I",60:69,sep=""),
              paste("J",40:46,sep=""),paste("K",25:28,sep=""),"N13",paste("N",17:21,sep=""),"N35","N40","N991"
              #icd 9
              ,"1123"	,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,
              "1196"	,"1304"	,"1390"	,"1500"	,"1501"	,"1502"	,"1503"	,"1504"	,"1505"	,"1510"	,"1511"	,"1512"	,
              "1513"	,"1514"	,"1515"	,"1516"	,"1520"	,"1521"	,"1522"	,"1523"	,"1550"	,"1551"	,"1552"	,"1570"	,
              "1571"	,"1572"	,"1573"	,"1574"	,"1580"	,"1590"	,"1591"	,"1600"	,"1601"	,"1602"	,"1603"	,"1610"	,
              "1611"	,"1612"	,"1620"	,"1622"	,"1623"	,"1624"	,"1625"	,"1630"	,"1631"	,"1720"	,"1721"	,"1722"	,
              "1723"	,"1724"	,"1725"	,"1726"	,"1401"	,"1404"	,"1701"	,"1702"	,"1703"	,"1704"	,"1706"	,"1710"	,
              "1712"	,"1713"	,"1714"	,"1715"	,"1716"	,"1730"	,"1731"	,"1732"	,"1733"	,"1734"	,"1735"	,"1736"	,
              "1740"	,"1741"	,"1742"	,"1743"	,"1744"	,"1745"	,"1746"	,"1765"	,"1800"	,"1801"	,"1880"	,"1881"	,
              "1882"	,"1883"	,"1884"	,"1885"	,"1886"	,"1890"	,"1891"	,"1892"	,"1893"	,"1894"	,"1895"	,"360"	,
              "363"	,"362"	,"382"	,"380"	,"381"	,"3841"	,"383"	,"3842"	,"3840"	,"3849"	,"388"	,"389"	,
              "461"	,"462"	,"463"	,"460"	,"1372"	,"1373"	,"1370"	,"3200"	,"3201"	,"3202"	,"3203"	,"3209"	,
              "32082"	,"3222"	,"3229"	,"340"	,"481"	,"4822"	,"4820"	,"4821"	,"4824"	,"48230"	,"48231"	,
              "48239"	,"48282"	,"48283"	,"48289"	,"4829"	,"485"	,"486"	,"68100"	,"68111"	,"6823"	,"6824"	,
              "6826"	,"6827"	,"6822"	,"6828"	,"6829"	,"2449"	,"2410"	,"2411"	,"2409"	,"24200"	,"24220"	,
              "24221"	,"24230"	,"24290"	,"24291"	,"2452"	,"41519"	,"5400"	,"5401"	,"5409"	,"541"	,"55012"	,
              "55092"	,"55010"	,"55000"	,"55001"	,"55200"	,"55100"	,"55300"	,"5521"	,"5511"	,"55220"	,
              "55221"	,"55321"	,"5523"	,"5533"	,"5528"	,"55229"	,"5518"	,"5538"	,"5529"	,"57400"	,"57401"	,
              "57410"	,"57411"	,"57420"	,"57421"	,"57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,
              "5750"	,"5751"	,"5761"	,"5762"	,"5764"	,"5768"	,"5769"	,"5770"	,"64123"	,"66511"	,"67002"	,
              "64671"	,"67131"	,"67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,"64863"	,
              "64893"	,"1518"	,"1519"	,"1534"	,"1535"	,"1536"	,"1530"	,"1531"	,"1537"	,"1532"	,"1533"	,"1538"	,
              "1539"	,"1540"	,"1541"	,"1543"	,"1542"	,"1548"	,"1707"	,"1708"	,"1709"	,"17001"	,"17002"	,"1727"	,
              "1728"	,"1729"	,"1737"	,"1738"	,"1739"	,"1748"	,"1759"	,"1749"	,"1808"	,"1809"	,"1821"	,"1820"	,
              "179"	,"185"	,"1869"	,"1887"	,"1888"	,"1889"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,
              "20150"	,"20151"	,"20152"	,"20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,
              "20190"	,"20195"	,"20198"	,"20400"	,"20401"	,"2106"	,"2113"	,"2114"	,"2112"	,"2115"	,"2116"	,
              "2117"	,"2123"	,"2126"	,"2127"	,"2136"	,"2148"	,"22809"	,"22801"	,"22802"	,"22804"	,"2281"	,
              "2150"	,"2189"	,"2230"	,"2231"	,"2252"	,"2254"	,"2250"	,"2251"	,"2253"	,"2270"	,"2273"	,"25021"	,
              "25031"	,"25033"	,"25011"	,"25013"	,"25041"	,"25043"	,"25061"	,"25071"	,"25073"	,"25081"	,
              "25083"	,"25091"	,"25001"	,"25003"	,"25020"	,"25030"	,"25010"	,"25012"	,"25040"	,"25042"	,
              "25050"	,"25060"	,"25070"	,"25072"	,"25080"	,"25082"	,"25000"	,"25002"	,"34550"	,"34500"	,
              "34510"	,"34511"	,"34560"	,"34570"	,"34580"	,"34590"	,"34591"	,"3453"	,"3452"	,"34540"	,
              "3910"	,"3912"	,"3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,"3959"	,
              "3970"	,"3960"	,"3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"40201"	,
              "40211"	,"40291"	,"40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,"40492"	,
              "40413"	,"40493"	,"40490"	,"4019"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,
              "41012"	,"41021"	,"41031"	,"41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,
              "41091"	,"41092"	,"41071"	,"41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,
              "41410"	,"41419"	,"4148"	,"4149"	,"430"	,"431"	,"4321"	,"4320"	,"4329"	,"43301"	,"43311"	,
              "43321"	,"43331"	,"43391"	,"43401"	,"43411"	,"43491"	,"436"	,"43320"	,"43300"	,"43310"	,
              "43330"	,"43380"	,"43400"	,"43410"	,"43490"	,"4373"	,"4370"	,"4372"	,"4375"	,"4371"	,"4378"	,
              "4379"	,"438"	,"4280"	,"4281"	,"4289"	,"4210"	,"4219"	,"4240"	,"4241"	,"4242"	,"591"	,"59372"	,
              "59373"	,"5996"	,"5845"	,"5849"	,"5859"	,"5851"	,"586"	,"5920"	,"5921"	,"5929"	,"5941"	,"5989"	,
              "4918"	,"4919"	,"4920"	,"4928"	,"49121"	,"49120"	,"49320"	,"49321"	,"496"	,"49300"	,"49310"	,
              "49390"	,"49301"	,"49391"	,"53100"	,"53110"	,"53120"	,"53140"	,"53150"	,"53151"	,"53160"	,
              "53170"	,"53190"	,"53191"	,"53200"	,"53210"	,"53220"	,"53240"	,"53241"	,"53250"	,"53251"	,
              "53260"	,"53271"	,"53290"	,"53300"	,"53310"	,"53340"	,"53341"	,"53350"	,"53360"	,"53390"	,
              "53410"	,"53450"	,"76401"	,"76402"	,"76404"	,"76407"	,"76429"	,"76490"	,"76491"	,"76492"	,
              "76493"	,"76494"	,"76495"	,"76496"	,"76497"	,"76498"	,"76499"	,"76500"	,"76501"	,"76502"	,
              "76503"	,"76511"	,"76512"	,"76514"	,"76516"	,"76518"	,"7660"	,"7661"	,"7662"	,"7670"	,"7671"	,
              "7682"	,"7683"	,"7684"	,"7685"	,"7689"	,"7700"	,"7701"	,"7702"	,"7703"	,"7707"	,"7704"	,"7705"	,
              "7708"	,"7709"	,"7711"	,"7717"	,"7712"	,"7714"	,"7718"	,"7720"	,"7721"	,"7722"	,"7760"	,"7725"	,
              "7726"	,"7728"	,"7730"	,"7731"	,"7732"	,"7780"	,"7747"	,"7744"	,"7762"	,"7761"	,"7764"	,"7765"	,
              "7750"	,"7756"	,"7758"	,"7757"	,"7775"	,"7776"	,"7792"	,"7680"	,"7681"	,"7400"	,"7402"	,"7424"	,
              "74101"	,"74102"	,"74103"	,"74100"	,"74192"	,"74193"	,"74190"	,"74253"	,"74259"	,"7428"	,
              "7429"	,"74483"	,"74489"	,"7450"	,"74511"	,"74510"	,"74519"	,"74512"	,"7458"	,"7454"	,"7455"	,
              "74569"	,"7452"	,"74601"	,"74602"	,"74609"	,"7461"	,"7462"	,"7463"	,"7464"	,"7465"	,"7466"	,
              "7467"	,"74687"	,"74685"	,"74686"	,"74684"	,"74689"	,"7469"	,"7470"	,"74710"	,"74722"	,
              "74729"	,"7473"	,"74741"	,"74742"	,"74740"	,"7475"	,"74761"	,"74769"	,"74781"	,"74782"	,
              "7483"	,"7484"	,"7485"	,"74869"	,"74860"	,"74900"	,"74910"	,"75016"	,"75029"	,"7503"	,"7504"	,
              "7511"	,"7512"	,"7510"	,"7513"	,"7514"	,"7515"	,"75161"	,"75169"	,"75162"	,"7518"	,"7530"	,
              "75314"	,"75313"	,"75312"	,"75315"	,"75317"	,"75319"	,"75310"	,"7532"	,"7534"	,"7533"	,"7536"	,
              "7538"	,"75563"	,"75470"	,"7540"	,"7542"	,"75481"	,"75489"	,"75500"	,"75529"	,"75534"	,
              "75560"	,"75569"	,"75600"	,"75608"	,"75609"	,"75615"	,"75619"	,"75656"	,"75651"	,"75659"	,
              "7564"	,"75650"	,"7566"	,"7567"	,"75683"	,"7569"	,"7571"	,"75739"	,"23770"	,"23771"	,"23772"	,
              "7595"	,"7596"	,"75987"	,"75981"	,"75986"	,"75985"	,"75982"	,"75984"	,"7590"	,"7591"	,"7593"	,
              "7594"	,"7597"	,"75989"	,"7599"	,"7580"	,"7582"	,"7581"	,"7583"	,"7585"	,"7586"	,"7588"	,"7589")

raymontamdf = adf[which(is.element(substring(adf$icdd,1,3),substring(raymontam,1,3))),]
raymontamndf = adf[which(!is.element(substring(adf$icdd,1,3),substring(raymontam,1,3))),]
mraymontamdf = mdf[which(is.element(substring(mdf$icdd,1,3),substring(raymontam,1,3))),]
mraymontamndf = mdf[which(!is.element(substring(mdf$icdd,1,3),substring(raymontam,1,3))),]
nmraymontamdf = nmdf[which(is.element(substring(nmdf$icdd,1,3),substring(raymontam,1,3))),]
nmraymontamndf = nmdf[which(!is.element(substring(nmdf$icdd,1,3),substring(raymontam,1,3))),]

praymontamdf = pdf[which(is.element(substring(pdf$icdd,1,3),substring(raymontam,1,3))),]
praymontamndf = pdf[which(!is.element(substring(pdf$icdd,1,3),substring(raymontam,1,3))),]
npraymontamdf = npdf[which(is.element(substring(npdf$icdd,1,3),substring(raymontam,1,3))),]
npraymontamndf = npdf[which(!is.element(substring(npdf$icdd,1,3),substring(raymontam,1,3))),]
asraymontamdf = asdf[which(is.element(substring(asdf$icdd,1,3),substring(raymontam,1,3))),]
asraymontamndf = asdf[which(!is.element(substring(asdf$icdd,1,3),substring(raymontam,1,3))),]
nasraymontamdf = nasdf[which(is.element(substring(nasdf$icdd,1,3),substring(raymontam,1,3))),]
nasraymontamndf = nasdf[which(!is.element(substring(nasdf$icdd,1,3),substring(raymontam,1,3))),]
npnmraymontamdf = npnmdf[which(is.element(substring(npnmdf$icdd,1,3),substring(raymontam,1,3))),]
npnmraymontamndf = npnmdf[which(!is.element(substring(npnmdf$icdd,1,3),substring(raymontam,1,3))),]

aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontamdf, FUN = function(x){NROW(x)})
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamdf, FUN = function(x){NROW(x)})
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontamdf, FUN = function(x){NROW(x)})
aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = raymontamndf, FUN = function(x){NROW(x)})
maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mraymontamndf, FUN = function(x){NROW(x)})
nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmraymontamndf, FUN = function(x){NROW(x)})

pagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = praymontamdf, FUN = function(x){NROW(x)})
npagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npraymontamdf, FUN = function(x){NROW(x)})
asagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = asraymontamdf, FUN = function(x){NROW(x)})
nasagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nasraymontamdf, FUN = function(x){NROW(x)})
npnmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmraymontamdf, FUN = function(x){NROW(x)})

paggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = praymontamndf, FUN = function(x){NROW(x)})
npaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npraymontamndf, FUN = function(x){NROW(x)})
asaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = asraymontamndf, FUN = function(x){NROW(x)})
nasaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nasraymontamndf, FUN = function(x){NROW(x)})
npnmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmraymontamndf, FUN = function(x){NROW(x)})

csvcreator("raymontam")

aagg = NULL
magg = NULL
nmagg = NULL

pagg = NULL
npagg = NULL
asagg = NULL
nasagg = NULL
npnmagg = NULL

aaggn = NULL
maggn = NULL
nmaggn = NULL

paggn = NULL
npaggn = NULL
asaggn = NULL
nasaggn = NULL
npnmaggn = NULL

allagg = NULL
maoriagg = NULL
nonmaoriagg = NULL

pacificagg = NULL
nonpacificagg = NULL
asianagg = NULL
nonasianagg = NULL
nonpacificnonmaoriagg = NULL

allaggn = NULL
maoriaggn = NULL
nonmaoriaggn = NULL

pacificaggn = NULL
nonpacificaggn = NULL
asianaggn = NULL
nonasianaggn = NULL
nonpacificnonmaoriaggn = NULL


tobias = c(paste("A",15:19,sep=""),paste("A",38:41,sep=""),"A46","A481",paste("B",50:54,sep=""),"B90",
           "G00","G03","J020",paste("J",13:15,sep=""),"J18","L03",paste("C",18:21,sep=""),
           paste("C",43:44,sep=""),"C50",paste("C",53:55,sep=""),"C67","C73","C81","C910",
           paste("C",95:97,sep=""),paste("D",10:36,sep=""),paste("E0",0:7,sep=""),paste("G",40:41,sep=""),
           paste("K",35:38,sep=""),paste("K",40:46,sep=""),paste("K",80:83,sep=""),"K85","K86","K915",
           paste("E",10:14,sep=""),"H311",paste("O0",1:9,sep=""),paste("O",10:99,sep=""),"P00",
           paste("P0",3:9,sep=""),paste("P",10:95,sep=""),paste("Q0",0:9,sep=""),paste("Q",10:99,sep=""),
           paste("I0",1:9,sep=""),paste("I",11:13,sep=""),paste("I",20:25,sep=""),paste("I",60:69,sep=""),
           paste("J",40:46,sep=""),paste("K",25:28,sep=""),paste("N0",0:9,sep=""),"N13",
           paste("N",19:21,sep=""),"N35","N40","N991"
           #icd 9
           ,"1123"	,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,
           "1196"	,"1304"	,"1390"	,"1500"	,"1501"	,"1502"	,"1503"	,"1504"	,"1505"	,"1510"	,"1511"	,"1512"	,
           "1513"	,"1514"	,"1515"	,"1516"	,"1520"	,"1521"	,"1522"	,"1523"	,"1550"	,"1551"	,"1552"	,"1570"	,
           "1571"	,"1572"	,"1573"	,"1574"	,"1580"	,"1590"	,"1591"	,"1600"	,"1601"	,"1602"	,"1603"	,"1610"	,
           "1611"	,"1612"	,"1620"	,"1622"	,"1623"	,"1624"	,"1625"	,"1630"	,"1631"	,"1720"	,"1721"	,"1722"	,
           "1723"	,"1724"	,"1725"	,"1726"	,"1401"	,"1404"	,"1701"	,"1702"	,"1703"	,"1704"	,"1706"	,"1710"	,
           "1712"	,"1713"	,"1714"	,"1715"	,"1716"	,"1730"	,"1731"	,"1732"	,"1733"	,"1734"	,"1735"	,"1736"	,
           "1740"	,"1741"	,"1742"	,"1743"	,"1744"	,"1745"	,"1746"	,"1765"	,"1800"	,"1801"	,"1880"	,"1881"	,
           "1882"	,"1883"	,"1884"	,"1885"	,"1886"	,"1890"	,"1891"	,"1892"	,"1893"	,"1894"	,"1895"	,"360"	,
           "363"	,"362"	,"382"	,"380"	,"381"	,"3841"	,"383"	,"3842"	,"3840"	,"3849"	,"388"	,"389"	,
           "461"	,"462"	,"463"	,"460"	,"1372"	,"1373"	,"1370"	,"3200"	,"3203"	,"340"	,"481"	,"4822"	,
           "4820"	,"4821"	,"4824"	,"48230"	,"48231"	,"48239"	,"48282"	,"48283"	,"48289"	,"4829"	,"485"	,
           "486"	,"68100"	,"68111"	,"6823"	,"6824"	,"6826"	,"6827"	,"6822"	,"6828"	,"6829"	,"1534"	,"1535"	,
           "1536"	,"1530"	,"1531"	,"1537"	,"1532"	,"1533"	,"1538"	,"1539"	,"1540"	,"1541"	,"1543"	,"1542"	,
           "1548"	,"1727"	,"1728"	,"1729"	,"1737"	,"1738"	,"1739"	,"1748"	,"1759"	,"1749"	,"1808"	,"1809"	,
           "1821"	,"1820"	,"179"	,"1887"	,"1888"	,"1889"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,
           "20150"	,"20151"	,"20152"	,"20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,
           "20190"	,"20195"	,"20198"	,"20400"	,"20401"	,"20410"	,"20240"	,"20480"	,"20490"	,"20800"	,
           "20810"	,"20880"	,"20890"	,"20250"	,"20260"	,"20290"	,"2106"	,"2113"	,"2114"	,"2112"	,"2115"	,
           "2116"	,"2117"	,"2123"	,"2126"	,"2127"	,"2136"	,"2148"	,"22809"	,"22801"	,"22802"	,"22804"	,
           "2281"	,"2150"	,"2189"	,"2230"	,"2231"	,"2252"	,"2254"	,"2250"	,"2251"	,"2253"	,"2270"	,"2273"	,
           "2449"	,"2410"	,"2411"	,"2409"	,"24200"	,"24220"	,"24221"	,"24230"	,"24290"	,"24291"	,"2452"	,
           "34550"	,"34500"	,"34510"	,"34511"	,"34560"	,"34570"	,"34580"	,"34590"	,"34591"	,"3453"	,
           "3452"	,"34540"	,"5400"	,"5401"	,"5409"	,"541"	,"55012"	,"55092"	,"55010"	,"55000"	,"55001"	,
           "55200"	,"55100"	,"55300"	,"5521"	,"5511"	,"55220"	,"55221"	,"55321"	,"5523"	,"5533"	,"5528"	,
           "55229"	,"5518"	,"5538"	,"5529"	,"57400"	,"57401"	,"57410"	,"57411"	,"57420"	,"57421"	,
           "57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,"5750"	,"5751"	,"5761"	,"5762"	,"5764"	,
           "5768"	,"5769"	,"5770"	,"5771"	,"5772"	,"5778"	,"5779"	,"25021"	,"25031"	,"25033"	,"25011"	,
           "25013"	,"25041"	,"25043"	,"25061"	,"25071"	,"25073"	,"25081"	,"25083"	,"25091"	,"25001"	,
           "25003"	,"25020"	,"25030"	,"25010"	,"25012"	,"25040"	,"25042"	,"25050"	,"25060"	,"25070"	,
           "25072"	,"25080"	,"25082"	,"25000"	,"25002"	,"64123"	,"66511"	,"67002"	,"64671"	,"67131"	,
           "67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,"64863"	,"64893"	,"76401"	,
           "76402"	,"76404"	,"76407"	,"76429"	,"76490"	,"76491"	,"76492"	,"76493"	,"76494"	,"76495"	,
           "76496"	,"76497"	,"76498"	,"76499"	,"76500"	,"76501"	,"76502"	,"76503"	,"76511"	,"76512"	,
           "76514"	,"76516"	,"76518"	,"7660"	,"7661"	,"7662"	,"7670"	,"7671"	,"7682"	,"7683"	,"7684"	,
           "7685"	,"7689"	,"7700"	,"7701"	,"7702"	,"7703"	,"7707"	,"7704"	,"7705"	,"7708"	,"7709"	,"7711"	,
           "7717"	,"7712"	,"7714"	,"7718"	,"7720"	,"7721"	,"7722"	,"7760"	,"7725"	,"7726"	,"7728"	,"7730"	,
           "7731"	,"7732"	,"7780"	,"7747"	,"7744"	,"7762"	,"7761"	,"7764"	,"7765"	,"7750"	,"7756"	,"7758"	,
           "7757"	,"7775"	,"7776"	,"7792"	,"7680"	,"7681"	,"7400"	,"7402"	,"7424"	,"74101"	,"74102"	,
           "74103"	,"74100"	,"74192"	,"74193"	,"74190"	,"74253"	,"74259"	,"7428"	,"7429"	,"74483"	,
           "74489"	,"7450"	,"74511"	,"74510"	,"74519"	,"74512"	,"7458"	,"7454"	,"7455"	,"74569"	,
           "7452"	,"74601"	,"74602"	,"74609"	,"7461"	,"7462"	,"7463"	,"7464"	,"7465"	,"7466"	,"7467"	,
           "74687"	,"74685"	,"74686"	,"74684"	,"74689"	,"7469"	,"7470"	,"74710"	,"74722"	,"74729"	,
           "7473"	,"74741"	,"74742"	,"74740"	,"7475"	,"74761"	,"74769"	,"74781"	,"74782"	,"7483"	,
           "7484"	,"7485"	,"74869"	,"74860"	,"74900"	,"74910"	,"75016"	,"75029"	,"7503"	,"7504"	,"7511"	,
           "7512"	,"7510"	,"7513"	,"7514"	,"7515"	,"75161"	,"75169"	,"75162"	,"7518"	,"7530"	,"75314"	,
           "75313"	,"75312"	,"75315"	,"75317"	,"75319"	,"75310"	,"7532"	,"7534"	,"7533"	,"7536"	,"7538"	,
           "75563"	,"75470"	,"7540"	,"7542"	,"75481"	,"75489"	,"75500"	,"75529"	,"75534"	,"75560"	,
           "75569"	,"75600"	,"75608"	,"75609"	,"75615"	,"75619"	,"75656"	,"75651"	,"75659"	,"7564"	,
           "75650"	,"7566"	,"7567"	,"75683"	,"7569"	,"7571"	,"75739"	,"23770"	,"23771"	,"23772"	,"7595"	,
           "7596"	,"75987"	,"75981"	,"75986"	,"75985"	,"75982"	,"75984"	,"7590"	,"7591"	,"7593"	,"7594"	,
           "7597"	,"75989"	,"7599"	,"7580"	,"7582"	,"7581"	,"7583"	,"7585"	,"7586"	,"7588"	,"7589"	,"3910"	,
           "3912"	,"3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,"3959"	,"3970"	,
           "3960"	,"3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"40201"	,"40211"	,
           "40291"	,"40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,"40492"	,"40413"	,
           "40493"	,"40490"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,"41012"	,"41021"	,
           "41031"	,"41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,"41091"	,"41092"	,
           "41071"	,"41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,"41410"	,"41419"	,
           "4148"	,"4149"	,"430"	,"431"	,"4321"	,"4320"	,"4329"	,"43301"	,"43311"	,"43321"	,"43331"	,
           "43391"	,"43401"	,"43411"	,"43491"	,"436"	,"43320"	,"43300"	,"43310"	,"43330"	,"43380"	,
           "43400"	,"43410"	,"43490"	,"4373"	,"4370"	,"4372"	,"4375"	,"4371"	,"4378"	,"4379"	,"438"	,"4918"	,
           "4919"	,"4920"	,"4928"	,"49121"	,"49120"	,"49320"	,"49321"	,"496"	,"49300"	,"49310"	,"49390"	,
           "49301"	,"49391"	,"53100"	,"53110"	,"53120"	,"53140"	,"53150"	,"53151"	,"53160"	,"53170"	,
           "53190"	,"53191"	,"53200"	,"53210"	,"53220"	,"53240"	,"53241"	,"53250"	,"53251"	,"53260"	,
           "53271"	,"53290"	,"53300"	,"53310"	,"53340"	,"53341"	,"53350"	,"53360"	,"53390"	,"53410"	,
           "53450"	,"58089"	,"5809"	,"5804"	,"5834"	,"5821"	,"5822"	,"58289"	,"5829"	,"5813"	,"5811"	,"5812"	,
           "5819"	,"5831"	,"5832"	,"5830"	,"5836"	,"58389"	,"5839"	,"591"	,"59372"	,"59373"	,"5996"	,"586"	,
           "5920"	,"5921"	,"5929"	,"5941"	,"5989")

tobiasdf = adf[which(is.element(substring(adf$icdd,1,3),substring(tobias,1,3))),]
tobiasndf = adf[which(!is.element(substring(adf$icdd,1,3),substring(tobias,1,3))),]
mtobiasdf = mdf[which(is.element(substring(mdf$icdd,1,3),substring(tobias,1,3))),]
mtobiasndf = mdf[which(!is.element(substring(mdf$icdd,1,3),substring(tobias,1,3))),]
nmtobiasdf = nmdf[which(is.element(substring(nmdf$icdd,1,3),substring(tobias,1,3))),]
nmtobiasndf = nmdf[which(!is.element(substring(nmdf$icdd,1,3),substring(tobias,1,3))),]

ptobiasdf = pdf[which(is.element(substring(pdf$icdd,1,3),substring(tobias,1,3))),]
ptobiasndf = pdf[which(!is.element(substring(pdf$icdd,1,3),substring(tobias,1,3))),]
nptobiasdf = npdf[which(is.element(substring(npdf$icdd,1,3),substring(tobias,1,3))),]
nptobiasndf = npdf[which(!is.element(substring(npdf$icdd,1,3),substring(tobias,1,3))),]
astobiasdf = asdf[which(is.element(substring(asdf$icdd,1,3),substring(tobias,1,3))),]
astobiasndf = asdf[which(!is.element(substring(asdf$icdd,1,3),substring(tobias,1,3))),]
nastobiasdf = nasdf[which(is.element(substring(nasdf$icdd,1,3),substring(tobias,1,3))),]
nastobiasndf = nasdf[which(!is.element(substring(nasdf$icdd,1,3),substring(tobias,1,3))),]
npnmtobiasdf = npnmdf[which(is.element(substring(npnmdf$icdd,1,3),substring(tobias,1,3))),]
npnmtobiasndf = npnmdf[which(!is.element(substring(npnmdf$icdd,1,3),substring(tobias,1,3))),]

aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = tobiasdf, FUN = function(x){NROW(x)})
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasdf, FUN = function(x){NROW(x)})
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmtobiasdf, FUN = function(x){NROW(x)})
aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = tobiasndf, FUN = function(x){NROW(x)})
maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mtobiasndf, FUN = function(x){NROW(x)})
nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmtobiasndf, FUN = function(x){NROW(x)})

pagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = ptobiasdf, FUN = function(x){NROW(x)})
npagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nptobiasdf, FUN = function(x){NROW(x)})
asagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = astobiasdf, FUN = function(x){NROW(x)})
nasagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nastobiasdf, FUN = function(x){NROW(x)})
npnmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmtobiasdf, FUN = function(x){NROW(x)})

paggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = ptobiasndf, FUN = function(x){NROW(x)})
npaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nptobiasndf, FUN = function(x){NROW(x)})
asaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = astobiasndf, FUN = function(x){NROW(x)})
nasaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nastobiasndf, FUN = function(x){NROW(x)})
npnmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmtobiasndf, FUN = function(x){NROW(x)})

csvcreator("tobias")

aagg = NULL
magg = NULL
nmagg = NULL

pagg = NULL
npagg = NULL
asagg = NULL
nasagg = NULL
npnmagg = NULL

aaggn = NULL
maggn = NULL
nmaggn = NULL

paggn = NULL
npaggn = NULL
asaggn = NULL
nasaggn = NULL
npnmaggn = NULL

allagg = NULL
maoriagg = NULL
nonmaoriagg = NULL

pacificagg = NULL
nonpacificagg = NULL
asianagg = NULL
nonasianagg = NULL
nonpacificnonmaoriagg = NULL

allaggn = NULL
maoriaggn = NULL
nonmaoriaggn = NULL

pacificaggn = NULL
nonpacificaggn = NULL
asianaggn = NULL
nonasianaggn = NULL
nonpacificnonmaoriaggn = NULL

moh = c(paste("A",15:16,sep=""),"A39","A403",paste("B",20:24,sep=""),"G001","J13","C16",
        paste("C",19:21,sep=""),paste("C",40:41,sep=""),"C43","C50","C53",paste("C",61:62,sep=""),
        "C73","C81","C910",paste("O0",0:9,sep=""),paste("O",10:96,sep=""),paste("O",98:99,sep=""),
        paste("P0",1:3,sep=""),paste("P0",5:9,sep=""),paste("P",10:94,sep=""),"Q21","E09",
        paste("E",10:14,sep=""),"I01",paste("I0",5:9,sep=""),paste("I",10:13,sep=""),
        paste("I",20:26,sep=""),paste("I",33:37,sep=""),"I50",paste("I",60:69,sep=""),
        paste("J",40:46,sep=""),paste("K",25:27,sep=""),"K80",paste("N",17:19,sep=""),
        paste("V0",1:4,sep=""),paste("V0",6:9,sep=""),paste("V",10:14,sep=""),paste("V",16:24,sep=""),
        paste("V",26:34,sep=""),paste("V",36:44,sep=""),paste("V",46:54,sep=""),paste("V",56:64,sep=""),
        paste("V",66:74,sep=""),paste("V",76:79,sep=""),paste("V80",0:5,sep=""),paste("V80",7:9,sep=""),
        paste("V",82:86,sep=""),paste("V87",0:5,sep=""),paste("V87",7:9,sep=""),paste("V88",0:5,sep=""),
        paste("V88",7:9,sep=""),"V89",paste("V",98:99,sep=""),paste("W0",0:8,sep=""),"W18",
        paste("X0",0:9,sep=""),paste("X",60:84,sep=""),paste("Y",60:82,sep="")
        #icd 9
        ,"1123"	,"1163"	,"1173"	,"1193"	,"1124"	,"1194"	,"1195"	,"1160"	,"1190"	,"1204"	,"1126"	,"1192"	,"1196"	,
        "360"	,"363"	,"362"	,"382"	,"3201"	,"481"	,"1518"	,"1519"	,"1540"	,"1541"	,"1543"	,"1542"	,"1548"	,
        "1707"	,"1708"	,"1709"	,"17001"	,"17002"	,"1727"	,"1728"	,"1729"	,"1748"	,"1759"	,"1749"	,"1808"	,
        "1809"	,"185"	,"1869"	,"193"	,"20140"	,"20143"	,"20145"	,"20148"	,"20150"	,"20151"	,"20152"	,
        "20153"	,"20154"	,"20160"	,"20161"	,"20165"	,"20168"	,"20173"	,"20190"	,"20195"	,"20198"	,
        "20400"	,"20401"	,"20410"	,"20240"	,"20480"	,"20490"	,"64123"	,"66511"	,"67002"	,"64671"	,
        "67131"	,"67311"	,"67401"	,"67434"	,"677"	,"64894"	,"67404"	,"64843"	,"64863"	,"64893"	,"76401"	,
        "76402"	,"76404"	,"76407"	,"76429"	,"76490"	,"76491"	,"76492"	,"76493"	,"76494"	,"76495"	,
        "76496"	,"76497"	,"76498"	,"76499"	,"76500"	,"76501"	,"76502"	,"76503"	,"76511"	,"76512"	,
        "76514"	,"76516"	,"76518"	,"7660"	,"7661"	,"7662"	,"7670"	,"7671"	,"7682"	,"7683"	,"7684"	,"7685"	,
        "7689"	,"7700"	,"7701"	,"7702"	,"7703"	,"7707"	,"7704"	,"7705"	,"7708"	,"7709"	,"7711"	,"7717"	,"7712"	,
        "7714"	,"7718"	,"7720"	,"7721"	,"7722"	,"7760"	,"7725"	,"7726"	,"7728"	,"7730"	,"7731"	,"7732"	,"7780"	,
        "7747"	,"7744"	,"7762"	,"7761"	,"7764"	,"7765"	,"7750"	,"7756"	,"7758"	,"7757"	,"7775"	,"7776"	,"7792"	,
        "7454"	,"7455"	,"74569"	,"7452"	,"25021"	,"25031"	,"25033"	,"25011"	,"25013"	,"25041"	,"25043"	,
        "25061"	,"25071"	,"25073"	,"25081"	,"25083"	,"25091"	,"25001"	,"25003"	,"25020"	,"25030"	,
        "25010"	,"25012"	,"25040"	,"25042"	,"25050"	,"25060"	,"25070"	,"25072"	,"25080"	,"25082"	,
        "25000"	,"25002"	,"3910"	,"3912"	,"3918"	,"3919"	,"3940"	,"3941"	,"3942"	,"3949"	,"3950"	,"3951"	,"3952"	,
        "3959"	,"3970"	,"3960"	,"3961"	,"3962"	,"3963"	,"3968"	,"3969"	,"3979"	,"39890"	,"39891"	,"4019"	,
        "40201"	,"40211"	,"40291"	,"40200"	,"40290"	,"40301"	,"40311"	,"40391"	,"40390"	,"40491"	,
        "40492"	,"40413"	,"40493"	,"40490"	,"4139"	,"41000"	,"41001"	,"41002"	,"41010"	,"41011"	,"41012"	,
        "41021"	,"41031"	,"41041"	,"41042"	,"41051"	,"41052"	,"41061"	,"41081"	,"41090"	,"41091"	,
        "41092"	,"41071"	,"41072"	,"41181"	,"41189"	,"4292"	,"41400"	,"41401"	,"41402"	,"41410"	,"41419"	,
        "4148"	,"4149"	,"41519"	,"4210"	,"4219"	,"4240"	,"4241"	,"4242"	,"4280"	,"4281"	,"4289"	,"430"	,"431"	,
        "4321"	,"4320"	,"4329"	,"43301"	,"43311"	,"43321"	,"43331"	,"43391"	,"43401"	,"43411"	,"43491"	,
        "436"	,"43320"	,"43300"	,"43310"	,"43330"	,"43380"	,"43400"	,"43410"	,"43490"	,"4373"	,"4370"	,
        "4372"	,"4375"	,"4371"	,"4378"	,"4379"	,"438"	,"4918"	,"4919"	,"4920"	,"4928"	,"49121"	,"49120"	,
        "49320"	,"49321"	,"496"	,"49300"	,"49310"	,"49390"	,"49301"	,"49391"	,"53100"	,"53110"	,"53120"	,
        "53140"	,"53150"	,"53151"	,"53160"	,"53170"	,"53190"	,"53191"	,"53200"	,"53210"	,"53220"	,
        "53240"	,"53241"	,"53250"	,"53251"	,"53260"	,"53271"	,"53290"	,"53300"	,"53310"	,"53340"	,
        "53341"	,"53350"	,"53360"	,"53390"	,"57400"	,"57401"	,"57410"	,"57411"	,"57420"	,"57421"	,
        "57430"	,"57440"	,"57431"	,"57441"	,"57450"	,"57451"	,"5845"	,"5849"	,"5859"	,"5851"	,"586"	,
        "82270"	,"82271"	,"82273"	,"82276"	,"82278"	,"81475"	,"82804"	,"82903"	,"82570"	,"82573"	,
        "82574"	,"82579"	,"81875"	,"81365"	,"81265"	,"82613"	,"82615"	,"82618"	,"81525"	,"81535"	,
        "81625"	,"81825"	,"81635"	,"82128"	,"82220"	,"82221"	,"82528"	,"81225"	,"81235"	,"81925"	,
        "82406"	,"82304"	,"82308"	,"82314"	,"81505"	,"81515"	,"81715"	,"81605"	,"81805"	,"81615"	,
        "81815"	,"82104"	,"82108"	,"82114"	,"82211"	,"82502"	,"82503"	,"82504"	,"82508"	,"81205"	,
        "81115"	,"81215"	,"81915"	,"82724"	,"82820"	,"82824"	,"82825"	,"82828"	,"81885"	,"81895"	,
        "80798"	,"82483"	,"82590"	,"82598"	,"81995"	,"82891"	,"84603"	,"88500"	,"88507"	,"88506"	,
        "88504"	,"88505"	,"88508"	,"88509"	,"88697"	,"88604"	,"88699"	,"88460"	,"88467"	,"88466"	,
        "88469"	,"88440"	,"88447"	,"88449"	,"88420"	,"88427"	,"88429"	,"88470"	,"88477"	,"89020"	,
        "89030"	,"89080"	,"89090"	,"89127"	,"89137"	,"89136"	,"89131"	,"89200"	,"89205"	,"89208"	,
        "89500"	,"89400"	,"89300"	,"89380"	,"89810"	,"95000"	,"95007"	,"95008"	,"95009"	,"95010"	,
        "95020"	,"95030"	,"95037"	,"95036"	,"95028"	,"95038"	,"95039"	,"95040"	,"95050"	,"95047"	,
        "95046"	,"95045"	,"95041"	,"95049"	,"87007"	,"87009"	,"87027"	,"87047"	,"87067"	,"87057"	,
        "87087"	,"87187"	,"87257"	,"87407"	,"87687")

mohdf = adf[which(is.element(substring(adf$icdd,1,3),substring(moh,1,3))),]
mohndf = adf[which(!is.element(substring(adf$icdd,1,3),substring(moh,1,3))),]
mmohdf = mdf[which(is.element(substring(mdf$icdd,1,3),substring(moh,1,3))),]
mmohndf = mdf[which(!is.element(substring(mdf$icdd,1,3),substring(moh,1,3))),]
nmmohdf = nmdf[which(is.element(substring(nmdf$icdd,1,3),substring(moh,1,3))),]
nmmohndf = nmdf[which(!is.element(substring(nmdf$icdd,1,3),substring(moh,1,3))),]

pmohdf = pdf[which(is.element(substring(pdf$icdd,1,3),substring(moh,1,3))),]
pmohndf = pdf[which(!is.element(substring(pdf$icdd,1,3),substring(moh,1,3))),]
npmohdf = npdf[which(is.element(substring(npdf$icdd,1,3),substring(moh,1,3))),]
npmohndf = npdf[which(!is.element(substring(npdf$icdd,1,3),substring(moh,1,3))),]
asmohdf = asdf[which(is.element(substring(asdf$icdd,1,3),substring(moh,1,3))),]
asmohndf = asdf[which(!is.element(substring(asdf$icdd,1,3),substring(moh,1,3))),]
nasmohdf = nasdf[which(is.element(substring(nasdf$icdd,1,3),substring(moh,1,3))),]
nasmohndf = nasdf[which(!is.element(substring(nasdf$icdd,1,3),substring(moh,1,3))),]
npnmmohdf = npnmdf[which(is.element(substring(npnmdf$icdd,1,3),substring(moh,1,3))),]
npnmmohndf = npnmdf[which(!is.element(substring(npnmdf$icdd,1,3),substring(moh,1,3))),]

aagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mohdf, FUN = function(x){NROW(x)})
magg = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohdf, FUN = function(x){NROW(x)})
nmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmmohdf, FUN = function(x){NROW(x)})
aaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mohndf, FUN = function(x){NROW(x)})
maggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = mmohndf, FUN = function(x){NROW(x)})
nmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nmmohndf, FUN = function(x){NROW(x)})

pagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = pmohdf, FUN = function(x){NROW(x)})
npagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npmohdf, FUN = function(x){NROW(x)})
asagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = asmohdf, FUN = function(x){NROW(x)})
nasagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = nasmohdf, FUN = function(x){NROW(x)})
npnmagg = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmmohdf, FUN = function(x){NROW(x)})

paggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = pmohndf, FUN = function(x){NROW(x)})
npaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npmohndf, FUN = function(x){NROW(x)})
asaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = asmohndf, FUN = function(x){NROW(x)})
nasaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = nasmohndf, FUN = function(x){NROW(x)})
npnmaggn = aggregate(DOD~GENDER+year+ageband+Dep06,data = npnmmohndf, FUN = function(x){NROW(x)})

csvcreator("moh")

}

csvcreator = function(type,n,givenagg,dep){
  ### Dropping the unknowns genders.
  agg = givenagg[givenagg$GENDER!="U",]
  if(dep == 0){
    colnames(agg) = c("G","Year","ageband","deaths")
  }else{
    colnames(agg) = c("G","Year","ageband","dep06","deaths")
  }
  
  agg$Factor = NA

  for(i in 1:length(agg$Factor)){
    if(agg$G[i] == 'M'){
      agg$Factor[i] = "Male"
    }else{
      agg$Factor[i] = "Female"
    }
  }
  
  agg = agg[-1]
  
  #create empty output file
  ryear = seq(startingDate,2013,1)
  repyear = rep(NA,length(ryear)*3)
  for(i in 1:length(ryear)){
    repyear[(i-1)*3+1] = ryear[i]
    repyear[(i-1)*3+2] = ryear[i]
    repyear[(i-1)*3+3] = ryear[i]
  }
  rgender = c("Male","Female","Total")
  repgender = rep(NA,length(ryear)*3)
  for(i in 1:length(ryear)){
    repgender[(i-1)*3+1] = rgender[1]
    repgender[(i-1)*3+2] = rgender[2]
    repgender[(i-1)*3+3] = rgender[3]
  }
  rages = rep(0,length(repyear)*18)
  repages = matrix(rages,ncol = 18)
  agesdf = as.data.frame(repages)

  newdf = data.frame(Year = repyear,Factor = repgender,agesdf,Dep06 = dep)
  colnames(newdf)[3:20] = NAMES
  
  #fill empty frame
  
  for(i in 1:length(agg$Year)){
    if(dep == 0){
      newdf[which(newdf$Year == agg$Year[i]  & newdf$Factor == agg$Factor[i] & newdf$Dep06 == 0),which(agg$ageband[i] == colnames(newdf))] = agg$deaths[i]
    }else if(dep>10){
      if(dep>100){
        d1 = 9
        d2 = 10
      }else{
        d1 = round(dep/10,0)
        d2 = dep%%10
      }
      newdf[which(newdf$Year == agg$Year[i]  & newdf$Factor == agg$Factor[i] & d1 == agg$dep06[i]),which(agg$ageband[i] == colnames(newdf))] = agg$deaths[i]
      newdf[which(newdf$Year == agg$Year[i]  & newdf$Factor == agg$Factor[i] & d2 == agg$dep06[i]),which(agg$ageband[i] == colnames(newdf))] = newdf[which(newdf$Year == agg$Year[i]  & newdf$Factor == agg$Factor[i] & d2 == agg$dep06[i]),which(agg$ageband[i] == colnames(newdf))] + agg$deaths[i]
    }else{
      newdf[which(newdf$Year == agg$Year[i]  & newdf$Factor == agg$Factor[i] & newdf$Dep06 == agg$dep06[i]),which(agg$ageband[i] == colnames(newdf))] = agg$deaths[i]
    }
    
  }
  for(i in 1:(length(newdf$Year)/3)){
    for(j in 3:19){
      newdf[i*3,j] = newdf[i*3-1,j] + newdf[i*3-2,j]
    }
  }
  for(i in 1:length(newdf$Year)){
    newdf[i,20] = sum(newdf[i,3:19])
  }
  
  write.csv(newdf,file=paste("mort",dep,type,n,"nat.csv",sep=""),row.names=FALSE)
}
