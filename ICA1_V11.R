#######################################
#### TASK 2 Load and Reparing Data ####
#######################################

### Load tenants data###
tenants <- read.csv("D:/Year 2/Further Computing Statistics/ICA 1/ICA1data/tenantsCSV.csv",header= TRUE,
colClass= c("factor", "factor","numeric","factor","factor","factor"))
names(tenants)<-c ( "last.name ","first.name", "age", "gender","flatID","status")
summary(tenants)

### Load customer data###
customersColname<- c("CustomerID", "Fullname","Gender","ZIPcode","Package")
customerColclass<- c("factor","factor","factor","factor","factor")
customers<- read.table ("D:/Year 2/Further Computing Statistics/ICA 1/ICA1data/customers.dat", sep=",",
col.names=customersColname, colClass=customerColclass )
#gender 1,2 = female and male
levels(customers$Gender) <- c("female","male")

# change class of "package" to numeric
as.numeric <- function(customers) {seq_along(levels(customers$Package))[customers$Package]}

# Find errors
summary (customers)
# customerID 14974011 was entered 6 times
#remove duplicated entries
customers<- unique(customers)

#gender 1,2 = female and male
levels(customers$Gender) <- c("female","male")

# change class of "package" to numeric
as.numeric <- function(customers) {seq_along(levels(customers$Package))[customers$Package]}

# Find errors
summary (customers)
# customerID 14974011 was entered 6 times
#remove duplicated entries
customers<- unique(customers)
summary (customers)

###Load usage data###
usageColname<- c("Month","CustomerID","NO.fCallsMade","NO.ofCallsRecieved","CallsLength","IntDownstreamExlTV","IntUpstream","IntDownstreamTV","TVHours","Package")
usageColClass<- c("numeric","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
usage<-read.table("D:/Year 2/Further Computing Statistics/ICA 1/ICA1data/usage.dat",sep=",",
col.names= usageColname, colClass=usageColClass )

#change display of numbers
options(scipen=100,digits= 4)

#Find errors
summary (usage)

#downsteam exclude TV : an outlier was found '-1'; NA was found i.e usage downstream exclusive TV of 2 customers were missing
#upstream : an outlier was found i.e '-1' ; NA was found i.e usage upstream of 2 customers were missing
#remove NA rows instead of changing to 0 as it will change the average
usage<- na.omit(usage)
summary (usage)
#No.of calls made: 2 outliers was found i.e '-1' and '16777216'
#call length: an outlier was found i.e '16777216'
usage<- usage[!usage$NO.fCallsMade %in% c(-1,16777216),]
usage<- usage[usage$CallsLength !=16777216 ,]
usage<- usage[usage$IntDownstreamExlTV !=-1 ,]
usage<- usage[usage$IntUpstream !=-1 ,]
summary (usage)


################################
####TASK 3 Create Data Frame####
################################

###(a)Creat data frame of customer monthly average usage###
#calculate the average internet usage for each unique customer ID
MonthlyUsage<- aggregate(cbind(usage$NO.fCallsMade,usage$NO.ofCallsRecieved,usage$CallsLength,usage$IntDownstreamExlTV,usage$IntUpstream,
usage$IntDownstreamTV,usage$TVHours,usage$Package),list(usage$CustomerID),mean)

##add col names to new data frame
names(MonthlyUsage)<-c("CustomerID","avgNO.fCallsMade","avgNO.ofCallsRecieved","avgCallsLength","avgIntDownstreamExlTV",
"avgIntUpstream","avgIntDownstreamTV","avgTVHours","Package")

###(b)Re-creat data frame of tenants sorted by age group and gender###

##creat age groups asscoiated with gender##
ageGroup10<- tenants[tenants$age>=0 &tenants$age<= 10,]
ageGroup10<-ageGroup10[order(ageGroup10$gender),]
Tab1<- table(ageGroup10$gender) 

ageGroup20<- tenants[tenants$age>=11 &tenants$age<= 20,]
ageGroup20<-ageGroup20[order(ageGroup10$gender),]
Tab2<-table(ageGroup20$gender) 

ageGroup30<- tenants[tenants$age>=21 &tenants$age<=30 ,]
ageGroup30<-ageGroup30[order(ageGroup30$gender),]
Tab3<-table(ageGroup30$gender) 

ageGroup40<- tenants[tenants$age>=31 &tenants$age<=45 ,]
ageGroup40<-ageGroup40[order(ageGroup40$gender),]
Tab4<-table(ageGroup40$gender) 

ageGroup50<- tenants[tenants$age>=46 &tenants$age<=65 ,]
ageGroup50<-ageGroup50[order(ageGroup50$gender),]
Tab5<-table(ageGroup50$gender) 

ageGroup60<- tenants[tenants$age>65,]
ageGroup60<-ageGroup60[order(ageGroup60$gender),]
Tab6<-table(ageGroup60$gender) 

NO.of.Residence_ageGroup<-do.call(rbind, list(Tab1,Tab2,Tab3,Tab4,Tab5,Tab6))
rownames(NO.of.Residence_ageGroup)<- c("0-10","11-20","21-30","31-45","46-65",">65")

tenants$ageGroup<- ifelse ( tenants$age>=0 &tenants$age<= 10 &tenants$gender== "male",tenants$ageGroup<- "0-10 male",
ifelse (tenants$age>=0 &tenants$age<= 10 &tenants$gender== "female",tenants$ageGroup<- "0-10 female",
ifelse ( tenants$age>=11 &tenants$age<= 20 &tenants$gender== "male",tenants$ageGroup<- "11-20 male",
ifelse  (tenants$age>=11 &tenants$age<= 20 &tenants$gender== "female",tenants$ageGroup<- "11-20 female",
ifelse ( tenants$age>=21 &tenants$age<= 30 &tenants$gender== "male",tenants$ageGroup<- "21-30 male",
ifelse(tenants$age>=21 &tenants$age<= 30 &tenants$gender== "female",tenants$ageGroup<- "21-30 female",
ifelse ( tenants$age>=31 &tenants$age<= 45 &tenants$gender== "male",tenants$ageGroup<- "31-45 male", 
ifelse ( tenants$age>=31 &tenants$age<= 45 &tenants$gender== "female",tenants$ageGroup<- "31-45 female", 
ifelse ( tenants$age>=46 &tenants$age<= 65 &tenants$gender== "male",tenants$ageGroup<- "46-65 male",
ifelse ( tenants$age>=46 &tenants$age<= 65 &tenants$gender== "female",tenants$ageGroup<- "46-65 female",
ifelse ( tenants$age>65 &tenants$gender== "male",tenants$ageGroup<- ">65 male",tenants$ageGroup<- ">65 female" )))))))))))
table (tenants$ageGroup)



##Creat data frame for tenants with househood size##
bedroomSize<-substr(tenants$flatID,1,1)
tenants$houseSize<-bedroomSize 
#class of householdsize
as.numeric <- function(tenants) {seq_along(levels(tenants$houseSize))[tenants$houseSize]}




#####################################################################################
####TASK 4 Merge Customer Data with Monthly Avergae Usage and Tenants Information####
#####################################################################################

###Merge customer data with monthly average###
CustomerData<- merge(customers,MonthlyUsage, by.customers = "CustomerID",by.MonthlyUsage="CustomerID")
summary (CustomerData)

###Formulate tenants data ###
tenants$Fullname = paste(tenants$first.name, tenants$last.name, sep=" ")

tenantsNew <- data.frame(tenants$Fullname,tenants$age,tenants$flatID,tenants$houseSize,tenants$status)
names(tenantsNew)<- c("Fullname","Age","FlatID","Housesize","Status")
tenantsNew$ageGroup <- tenants$ageGroup
as.factor (tenantsNew$ageGroup)
summary(tenantsNew)
table (tenantsNew$ageGroup)

###Merge tenants data with CustomerData###

# change fullname to capital letters
tenantsNew$Fullname<-toupper(tenantsNew$Fullname)
CustomerData$Fullname<-toupper(CustomerData$Fullname)

#to find out tenants who have internet pacages in dataset by merging two dataframes
final.dataframe<-merge (CustomerData,tenantsNew, by="Fullname",all=TRUE)

#remove tenants who do not have purchased the pacages
final.dataframe<-na.omit(final.dataframe)
summary(final.dataframe)

#there are tenants who have the same name but different ages, household and purchase details
#therefore we decide to delete the doublicated names from the dataframe we have got since it will hugely affect the further analysis
final.customer<-final.dataframe[!final.dataframe$CustomerID %in% c( 15037362,14995105),]
summary(final.customer)

#######################
####Usage Exceeding####
#######################
usage
usageEx<- usage
summary (usageEx)

##attach data vol of internet to usage data frame
usageEx$DatInt<- ifelse (usageEx$Package == 1, usageEx$DatInt<- 5, 
ifelse (usageEx$Package == 2, usageEx$DatInt<- 10,
ifelse (usageEx$Package == 3, usageEx$DatInt<- 10,Inf)))# 'inf represent unlimited'
##caculate internet exceeding
usageEx$IntT<- usageEx$IntDownstreamExlTV+usageEx$IntUpstream
usageEx$IntEx <- ifelse ( usageEx$DatInt== Inf, 0,
usageEx$IntEx <- usageEx$DatInt-usageEx$IntDownstreamExlTV-usageEx$IntUpstream)
##Find out how many time the customer exceed the data limit of internet
usageEx$IntExc<- ifelse ( usageEx$IntEx< 0, 1, 0)
## create data frame of internet exceeding counts asscoiated with customer ID
IntExc<-aggregate(IntExc~CustomerID, data=usageEx, sum)
summary (IntExc)
##Merge final.dat with IntExc
#removed the coustomers who had been removed in task 4
IntExc <-IntExc[!IntExc$CustomerID %in% c( 15037362,14995105),]
final.customerEx <- merge(final.customer,IntExc, by.final.customer= "CustomerID", by.IntExc="CustomerID")
summary (final.customerEx) # need to find out which customer exceed package later


##attach data vol of calls to usage data frame
usageEx$DatCal<- ifelse (usageEx$Package == 1, usageEx$DatCal<- 60, 
ifelse (usageEx$Package == 2, usageEx$DatCal<- Inf,
ifelse (usageEx$Package == 3, usageEx$DatCal<- 200,Inf)))# 'inf represent unlimited'
##calculate calls exceeding 
usageEx$CalEx <- ifelse (usageEx$DatCal == Inf, 0,usageEx$CalEx <- usageEx$DatCal- usageEx$CallsLength)
##Find out how many time the customer exceed the data limit of phone calls
usageEx$CalExc<- ifelse ( usageEx$CalEx< 0, 1, 0)
## create data frame of calls exceeding counts asscoiated with customer ID
CalExc<-aggregate(CalExc~CustomerID, data=usageEx, sum)
summary (CalExc)
##Merge final.dat with CalExc
#removed the coustomers who had been removed in task 4
CalExc <-CalExc[!CalExc$CustomerID %in% c( 15037362,14995105),]
final.customerEx <- merge(final.customerEx,CalExc, by.final.customer= "CustomerID", by.CalExc="CustomerID")
summary (final.customerEx) # need to find out which customer exceed package later


##attach data vol of TV to usage data frame
usageEx$DatTV<- ifelse (usageEx$Package == 1|usageEx$Package == 2,
usageEx$DatTV<- 0, Inf)  # 'inf represent unlimited'
##calculate TV exceeding 
usageEx$TVEx <- ifelse (usageEx$DatTV== Inf, 0,usageEx$TVEx <- usageEx$DatTV- usageEx$IntDownstreamTV)
summary (usageEx$TVEx)
##Find out how many time the customer exceed the data limit of TV
usageEx$TVExc<- ifelse ( usageEx$TVEx< 0, 1, 0)
## create data frame of TV exceeding counts asscoiated with customer ID
TVExc<-aggregate(TVExc~CustomerID, data=usageEx, sum)
summary (TVExc)
##Merge final.dat with TVExc
#removed the coustomers who had been removed in task 4
TVExc <-TVExc[!TVExc$CustomerID %in% c( 15037362,14995105),]
final.customerEx <- merge(final.customerEx,TVExc, by.final.customer= "CustomerID", by.TVExc="CustomerID")
summary (final.customerEx) # need to find out which customer exceed package later



#############################################################
#### Analysis of customers who purchase expensive package####
#############################################################

final.customerEx$P<- ifelse ( final.customerEx$Package== 1,final.customerEx$P <- "P1",
ifelse (final.customerEx$Package== 2, final.customerEx$P<- "P2", 
ifelse (final.customerEx$Package == 3, final.customerEx$P <-"P3", "P4")))

####mosaiplot of age group with package preference 
mosaicplot (table (final.customerEx[ ,c("ageGroup","P")]))

### statistics summary of package 1, 2, 3, 4
P4 <- final.customerEx[ final.customerEx$Package== 4,]
summary (P4)
 

P3 <- final.customerEx[ final.customerEx$Package== 3,]
summary (P3)

P2 <- final.customerEx[ final.customerEx$Package== 2,]
summary (P2)

P1 <- final.customerEx[ final.customerEx$Package== 1,]
summary (P1)

###Housize and package
table (final.customerEx[ , c("Housesize","P")])

### Internet exceeding counts with package 
table (final.customerEx[ , c("IntExc","P")])

### Call exceeding counts with package 
table (final.customerEx[ , c("CalExc","P")])

######################################################
#### Analysis of customer who exceeded data limit ####
######################################################

###Internet exceeding with housesize, gender
mosaicplot (table (final.customerEx[ ,c("IntExc","Housesize","Gender")]),
main= "Mosaicplot of customer who exceed Internet limit with demographic factor",
sub= "Counts of exceeding internet data limit" )

 
### Totoal usage of internet 
usageEx$IntT<- usageEx$IntDownstreamExlTV + usageEx$IntUpstream 

            
###Internet exceeding with Package 
mosaicplot (table (final.customerEx[ ,c("IntExc","P")]),
main= "Mosaicplot of package with counts of internet exceeding",sub="counts of exceeding internet data limit")
             
 
###Call data exceeding with Package ###
mosaicplot (table (final.customerEx[ ,c("P","CalExc")]),
main= "Mosaicplot of package with counts of calls exceeding",sub="Package")
           


###################################
####calculate exceeding credit ####
###################################
summary (usageEx)

#### calculate call exceeding credit####
usageEx$CalExCred <- ifelse ( usageEx$Package == 1 & usageEx$CalEx <0 , usageEx$CalExCred <- usageEx$CalEx * 0.1, 
ifelse(usageEx$Package == 3 &usageEx$CalEx <0  , usageEx$CalExCred <- usageEx$CalEx * 0.1,0 ))
usageEx$CalExCred <- abs ( usageEx$CalExCred)
summary ( usageEx$CalExCred)


#### calculate Internet exceeding credit####
usageEx$IntExCred <- ifelse ( usageEx$Package == 1 & usageEx$IntEx<0  , usageEx$IntExCred <- usageEx$IntEx *1000/5 *0.1,
ifelse (  usageEx$Package == 2 | usageEx$IntEx<0,usageEx$IntExCred <- usageEx$IntEx *1000/10* 0.1,
ifelse ( usageEx$Package == 3 | usageEx$IntEx<0, usageEx$IntExCred <- usageEx$IntEx *1000/10* 0.1,0)))
usageEx$IntExCred <- abs (usageEx$IntExCred)
summary ( usageEx$IntExCred)


#### Total exceeding credit ####
usageEx$TotalExCred <- usageEx$IntExCred + usageEx$CalExCred

#### Total credit ####
usageEx$TotalCred <- ifelse(  usageEx$Package == 1, usageEx$TotalCred <- usageEx$TotalExCred + 24.98, 
ifelse(  usageEx$Package == 2, usageEx$TotalCred <- usageEx$TotalExCred + 34.95,
ifelse(  usageEx$Package == 3, usageEx$TotalCred <- usageEx$TotalExCred + 44.99, 59.95)))

#### Assign package number ####
usageEx$P <- ifelse(  usageEx$Package == 1, usageEx$P <- "P1", 
ifelse(  usageEx$Package == 2, usageEx$P <- "P2", 
ifelse(  usageEx$Package == 3, usageEx$P <- "P3", "P4")))

### Boxplot of credit for exceeding internet usage with package 
boxplot (usageEx$IntExCred~ usageEx$P, xlab= " package",ylab= " Internet exceeding credit")

###Boxplot of total credit with package 
boxplot (usageEx$TotalCred~ usageEx$P, xlab= " package",ylab= " Total credit")

### Scatter plot matrix of total credit, internet exceeding credit and call exceeding credit
pairs ( ~TotalExCred+IntExCred+CalExCred, data= usageEx)


################################################################
#####Analysis of customer who did NOT exceed the data limit ####
################################################################

usageIntNotEx <- usageEx [ usageEx$IntExCred == 0 , ]
summary (usageIntNotEx)
boxplot ( usageIntNotEx$IntT~ usageIntNotEx$P, xlab= " Total internet usage", ylab ="Package" )
 
usageNotCalEx <- usageEx [ usageEx$CalExCred == 0, ]
summary ( usageNotCalEx)
boxplot ( usageNotCalEx$CallsLength~usageNotCalEx$P )
table (usageNotCalEx$P)

usageNotEx <-  usageEx [ usageEx$CalExCred == 0 & usageEx$IntExCred == 0, ]
boxplot ( usageNotEx$CallsLength~usageNotEx$P )
boxplot ( usageNotEx$IntT~usageNotEx$P )

summary ( usageNotEx)


######################################
#### Linear Relationship Analysis ####
######################################

summary (usageEx)
P3P2P1 <- usageEx [usageEx$Package== 3 |usageEx$Package== 2|usageEx$Package== 1,]


#### must say why exclude P4
TotalCredlm1<- lm (formula =  TotalCred~IntT+CallsLength+TVHours   ,data=usageEx)
summary (TotalCredlm1)


TotalCredlm2<- lm (formula =  TotalCred~IntT+TVHours   ,data=P3P2P1)
summary (TotalCredlm2)


TotalCredlm3<- lm (formula =  TotalCred~CallsLength+IntT   ,data=P3P2P1)
summary (TotalCredlm3)


TotalCredlm4<- lm (formula =  TotalCred~IntT   ,data=P3P2P1)
summary (TotalCredlm4)



##############################################################################
summary (final.customerEx)


avgCred<-aggregate(TotalCred~CustomerID, data=usageEx, mean)
##Merge final.dat with average total credit
#removed the coustomers who had been removed in task 4
avgCred <-avgCred[!avgCred$CustomerID %in% c( 15037362,14995105),]
final.customerEx <- merge(final.customerEx,avgCred, by.final.customer= "CustomerID", by.avgCred="CustomerID")
final.customerEx$avgIntT <- final.customerEx$avgIntDownstreamExlTV + final.customerEx$avgIntUpstream

avgP3P2P1<- final.customerEx [final.customerEx$Package== 3 |final.customerEx$Package== 2|final.customerEx$Package== 1,]


avgCredlm1<- lm( formula =TotalCred~Gender+ Age+ Status + Housesize+avgIntT+avgTVHours, data =avgP3P2P1)
summary (avgCredlm1)

avgCredlm2<- lm( formula =TotalCred~+ Status + Housesize+ avgIntT +avgTVHours, data =avgP3P2P1)
summary (avgCredlm2)


avgCredlm3 <- lm( formula =TotalCred~Status+avgIntT , data =avgP3P2P1)
summary (avgCredlm3)

avgCredlm4 <- lm( formula =TotalCred~Housesize+avgIntT , data =avgP3P2P1)
summary (avgCredlm4)


#summary (final.customerEx)
mosaicplot (table (tenants[ , c("ageGroup", "houseSize")])) 
mosaicplot(table ( tenants [ , c("ageGroup", "status")]))
boxplot (final.customerEx$avgIntT~final.customerEx$ageGroup)
boxplot (final.customerEx$avgTVHours~final.customerEx$ageGroup)
boxplot (final.customerEx$TotalCred~final.customerEx$ageGroup)
