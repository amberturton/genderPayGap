#Coursework 3 
library(ggplot2)
library(GGally)
library(rockchalk)
library(arm)
library(ResourceSelection)
library(plyr)
library(dplyr)
library(gridExtra)
library(forcats)
library(scales)
library(survey)
#### Loading data ####
#amber
setwd("~/Documents/UniWork/appliedStatistics/")
load(file = "lfs2018.RData")

'''
"TEN1" -  Accomodation details
"housex"- Housing tenure  
"sexx"     
"AGE" - continuous age var     
"AGES"- categorical age in bands
"NTNLTY12"-nationality    
"regionx" -     
"numchild04"- number of children aged 0-4
"numchild516" - num children aged 5-16
"AYFL19"- Age of youngest child in family under 19     
"ETHUKEUL"- Ethnicity- 9 categories   
"fbx"- whether born outside the UK    
"arrivalx"- year of arrival in the uk
"marstax"- marital status   
"LIV12W"- whether living together as a couple   
"marcivx" - whether married/ civil partner
"INECAC05" - basic economic activity
"statusx" - economic status  
"ILODEFR" - economic activity  
"GRSSWK" - gross weekly pay in main job
"HOURPAY" - gross hourly pay 
"SOC10M"- main job unit code   
"SC10MMJ" - main job major group 
"nsecm10x"- NS-SEC major group  
"JOBTYP" -permanent or temporary job   
"CONMPY" - year started work for current employer
"ptimex"- whether part time (self reported)   
"ptimehrs"- whether part time (work < 31 hrs) use this one    
"TTUSHR"- total usual hours   
"PUBLICR"- public or Private sector    
"MANAGER"- managerial status   
"SECJOB"- second job in ref week    
"YTETJB"- whether had paid job in a addition to scheme  
"WRKING"- whether did paid work in reference week     
"OWNBUS"- un paid work for own business
"RELBUS"- un paid work for relatives business
"STATR" - employment status in own job  
"LOOK4" -  looking for paid work in 4 weeks ending ref week
"LKYT4" - looking for scheme place 4 weeks    
"START" - able to start work in 2 weeks     
"WAIT"  - waiting to take up job already obtained   
"LIKEWK" - not looking but would like paid job 
"YSTART"  - reason could not start work within 2 weeks 
"NOLWM"    - main reason not looking for work
"HIQUAL15" - highest qualification
"HIQUL15D"- highest qualification detailed
"LEVQUL15"- level of highest qualification
"EDAGE"   - age when completed continued full time education 
"bhealthx" - bad health 
"CASENOP" - identifier
"HSERIALP" - identifier
"pwt18x"  - person weight
'''

#Missing data: -8: no answer, -9 Does not apply




#### Checking the different categories ####

### Household Info

summary(lfs$TEN1)
lfs$TEN1[lfs$TEN1=="No answer"]<- NA
lfs$TEN1[lfs$TEN1=="Does not apply"]<- NA
# Only 15 people with no answer, seems good although probably not relevant

### Respondent socio-demographics

summary(lfs$sexx)
# everone answered
summary(lfs$AGE)
summary(lfs$AGES)
# all good for ages
summary(lfs$NTNLTY12)
# does not apply 24? weird and no answer 7 but overall not too many
lfs$NTNLTY12[lfs$NTNLTY12=="No answer"]<- NA
lfs$NTNLTY12[lfs$NTNLTY12=="Does not apply"]<- NA

summary(lfs$regionx)
# everyone answered

summary(lfs$numchild04)
summary(lfs$numchild516)

lfs$numchild04[lfs$numchild04==-9]<- NA
lfs$numchild04[lfs$numchild04==-8]<- NA

lfs$numchild516[lfs$numchild516==-9]<- NA
lfs$numchild516[lfs$numchild516==-8]<- NA

# right now it is as numerical so lets add categorical variable
lfs$numchild04.cat <- as.factor(lfs$numchild04)
lfs$numchild516.cat <- as.factor(lfs$numchild516)
summary(lfs$numchild04.cat)
summary(lfs$numchild516.cat)


summary(lfs$AYFL19)

# same people again here did not answer these
lfs$AYFL19[lfs$AYFL19==-9]<- NA
lfs$AYFL19[lfs$AYFL19==-8]<- NA
summary(lfs$AYFL19)

lfs$AYFL19.cat <- as.factor(lfs$AYFL19)
summary(lfs$AYFL19.cat)

# NAs are now taken care of in both numerical and categorical

summary(lfs$ETHUKEUL)
# 56 No answers which we can change to NA's
lfs$ETHUKEUL[lfs$ETHUKEUL=="No answer"]<- NA
summary(lfs$ETHUKEUL)

summary(lfs$fbx)
lfs$fbx[lfs$fbx=="Does not apply"]<- NA
summary(lfs$fbx)

summary(lfs$arrivalx)
#arrival year in the UK
lfs$arrivalx[lfs$arrivalx==0]<- NA
lfs$arrivalx[lfs$arrivalx==1]<- "Pre 1959"
lfs$arrivalx[lfs$arrivalx==2]<- "1960-1969"
lfs$arrivalx[lfs$arrivalx==3]<- "1970-1979"
lfs$arrivalx[lfs$arrivalx==4]<- "1980-1989"
lfs$arrivalx[lfs$arrivalx==5]<- "1990-1999"
lfs$arrivalx[lfs$arrivalx==6]<- "2000-2009"
lfs$arrivalx[lfs$arrivalx==7]<- "2010-2018"

summary(lfs$marstax)
summary(lfs$LIV12W)

# This is a subsection of the previous question so only a proportion of the population answered this question

summary(lfs$marcivx)
lfs$marcivx[lfs$marcivx=="Does not apply"]<- NA

### Employment details

summary(lfs$INECAC05)

summary(lfs$statusx)
# This one seems like a easier version of the previous variable with less options
summary(lfs$ILODEFR)

#gross pay 
summary(lfs$GRSSWK)
summary(as.factor(lfs$GRSSWK))
# just over 43,000 no weekly --> which is more than the unemployed or not seeking employment number.. could be self employed people who do not know per week
#group factor
lfs$GRSSWK.cat<- NA
lfs$GRSSWK.cat[(0<= lfs$GRSSWK) & (lfs$GRSSWK <100)] <-"Less than £100"
lfs$GRSSWK.cat[(100<= lfs$GRSSWK) & (lfs$GRSSWK <200)] <-"£100 - £200"
lfs$GRSSWK.cat[(200<= lfs$GRSSWK) & (lfs$GRSSWK <300)] <-"£200 - £300"
lfs$GRSSWK.cat[(300<= lfs$GRSSWK) & (lfs$GRSSWK <400)] <-"£300 - £400"
lfs$GRSSWK.cat[(400<= lfs$GRSSWK) & (lfs$GRSSWK <500)] <-"£400 - £500"
lfs$GRSSWK.cat[(500<= lfs$GRSSWK) & (lfs$GRSSWK <600)] <-"£500 - £600"
lfs$GRSSWK.cat[(600<= lfs$GRSSWK) & (lfs$GRSSWK <700)] <-"£600 - £700"
lfs$GRSSWK.cat[(700<= lfs$GRSSWK) & (lfs$GRSSWK <800)] <-"£700 - £800"
lfs$GRSSWK.cat[lfs$GRSSWK >=800] <-"£800+"
summary(as.factor(lfs$GRSSWK.cat))

lfs$GRSSWK[lfs$GRSSWK==-9]<-NA
lfs$GRSSWK[lfs$GRSSWK==-8]<-NA

summary(as.factor(lfs$HOURPAY))
# just under 43,000 people did not answer this question
lfs$HOURPAY[lfs$HOURPAY==-9]<-NA
lfs$HOURPAY[lfs$HOURPAY==-8]<-NA

lfs <- lfs[which( is.na(lfs$HOURPAY) | (lfs$HOURPAY>=3)) , ]

lfs$HOURPAY.cat<- NA
lfs$HOURPAY.cat[(0<= lfs$HOURPAY) & (lfs$HOURPAY <5)] <-"Less than £5"
lfs$HOURPAY.cat[(5<= lfs$HOURPAY) & (lfs$HOURPAY <10)] <-"£5 - £10"
lfs$HOURPAY.cat[(10<= lfs$HOURPAY) & (lfs$HOURPAY <15)] <-"£10 - £15"
lfs$HOURPAY.cat[(15<= lfs$HOURPAY) & (lfs$HOURPAY <20)] <-"£15 - £20"
lfs$HOURPAY.cat[(20<= lfs$HOURPAY) & (lfs$HOURPAY <25)] <-"£20- £25"
lfs$HOURPAY.cat[(25<= lfs$HOURPAY) & (lfs$HOURPAY <30)] <-"£25 - £30"
lfs$HOURPAY.cat[(30<= lfs$HOURPAY) & (lfs$HOURPAY <35)] <-"£30 - £35"
lfs$HOURPAY.cat[lfs$HOURPAY >=35] <-"£35+"




summary(lfs$SOC10M)
summary(lfs$SC10MMJ)
# 38 people did not answer so let's change this to NAs
lfs$SC10MMJ[lfs$SC10MMJ=="No answer"]<- NA
lfs$SC10MMJ[lfs$SC10MMJ=="Does not apply"]<- NA
lfs$SOC10M[lfs$SOC10M=="No answer"]<- NA
lfs$SOC10M[lfs$SOC10M=="Does not apply"]<- NA
summary(lfs$SOC10M)
summary(lfs$SC10MMJ)
# ok now the no answers are NA's
#Used later for graphical summaries
levels(lfs$SC10MMJ)<- c("Managers, Directors And Senior Officials",        
                        "Professional Occupations",                        
                        "Associate Professional And Technical Occupations",
                        "Administrative And Secretarial Occupations",      
                        "Skilled Trades Occupations" ,                     
                        "Caring, Leisure And Other Service Occupations",   
                        "Sales And Customer Service Occupations",          
                        "Process, Plant And Machine Operatives",           
                        "Elementary Occupations")

summary(lfs$nsecm10x)
lfs$nsecm10x[ (lfs$nsecm10x == "16.0 Not classified or inadequately stated") |
              (lfs$nsecm10x == "17.0 Not classifiable for other reasons") ] <-NA

summary(lfs$JOBTYP)
# 26 people did not answer and it did not apply to 20,027 people
lfs$JOBTYP[lfs$JOBTYP=="No answer"]<- NA
lfs$JOBTYP[lfs$JOBTYP=="Does not apply"]<- NA
summary(lfs$JOBTYP)

summary(lfs$CONMPY)
# -9 is the minimum value so some missing values
lfs$CONMPY[lfs$CONMPY==-9]<- NA
# -8 means there were some non applicable answers, meaning they did not work, so let's change these to NA's as well
lfs$CONMPY[lfs$CONMPY==-8]<- NA
summary(lfs$CONMPY)

lfs$yearsAtComp <-2018- lfs$CONMPY

summary(lfs$ptimex)#self report
summary(lfs$ptimehrs)#use this one
#  All good

summary(lfs$TTUSHR)
lfs$TTUSHR[lfs$TTUSHR==-9]<- NA
lfs$TTUSHR[lfs$TTUSHR==-8]<- NA
summary(lfs$TTUSHR)

summary(lfs$PUBLICR)
lfs$PUBLICR[lfs$PUBLICR=="No answer"]<- NA
lfs$PUBLICR[lfs$PUBLICR=="Does not apply"]<- NA
summary(lfs$PUBLICR)

summary(lfs$MANAGER)
lfs$MANAGER[lfs$MANAGER=="No answer"]<- NA
lfs$MANAGER[lfs$MANAGER=="Does not apply"]<- NA
summary(lfs$MANAGER)

summary(lfs$SECJOB)
lfs$SECJOB[lfs$SECJOB=="No answer"]<- NA
lfs$SECJOB[lfs$SECJOB=="Does not apply"]<- NA
summary(lfs$SECJOB)

summary(lfs$YTETJB)
lfs$YTETJB[lfs$YTETJB=="Does not apply"]<- NA
summary(lfs$YTETJB)

summary(lfs$WRKING)
lfs$WRKING[lfs$WRKING=="Does not apply"]<- NA
summary(lfs$WRKING)

summary(lfs$OWNBUS)
lfs$OWNBUS[lfs$OWNBUS=="Does not apply"]<- NA
summary(lfs$OWNBUS)

summary(lfs$RELBUS)
lfs$RELBUS[lfs$RELBUS=="Does not apply"]<- NA
summary(lfs$RELBUS)

summary(lfs$STATR)
lfs$STATR[lfs$STATR=="Does not apply"]<- NA
summary(lfs$STATR)

summary(lfs$LOOK4)
lfs$LOOK4[lfs$LOOK4=="Does not apply"]<- NA
summary(lfs$LOOK4)
summary(lfs$LKYT4)
lfs$LKYT4[lfs$LKYT4=="Does not apply"]<- NA
summary(lfs$LKYT4)

summary(lfs$START)
lfs$START[lfs$START=="No answer"]<- NA
lfs$START[lfs$START=="Does not apply"]<- NA
summary(lfs$START)

summary(lfs$WAIT)
lfs$WAIT[lfs$WAIT=="No answer"]<- NA
lfs$WAIT[lfs$WAIT=="Does not apply"]<- NA
summary(lfs$WAIT)

summary(lfs$LIKEWK)
lfs$LIKEWK[lfs$LIKEWK=="No answer"]<- NA
lfs$LIKEWK[lfs$LIKEWK=="Does not apply"]<- NA
summary(lfs$LIKEWK)

summary(lfs$YSTART)
lfs$YSTART[lfs$YSTART=="No answer"]<- NA
lfs$YSTART[lfs$YSTART=="Does not apply"]<- NA
summary(lfs$YSTART)

summary(lfs$NOLWM)
lfs$NOLWM[lfs$NOLWM=="Does not apply"]<- NA
summary(lfs$NOLWM)


### Other important variables

summary(lfs$HIQUAL15)
lfs$HIQUAL15[lfs$HIQUAL15=="No answer"]<- NA
lfs$HIQUAL15[lfs$HIQUAL15=="Don't know"]<- NA
summary(lfs$HIQUAL15)

summary(lfs$HIQUL15D)
lfs$HIQUL15D[lfs$HIQUL15D=="No answer"]<- NA
lfs$HIQUL15D[lfs$HIQUL15D=="Don't know"]<- NA
summary(lfs$HIQUL15D)

summary(lfs$LEVQUL15)
lfs$LEVQUL15[lfs$LEVQUL15=="No answer"]<- NA
summary(lfs$LEVQUL15)

summary(lfs$EDAGE)
lfs$EDAGE[lfs$EDAGE==-9]<- NA
lfs$EDAGE[lfs$EDAGE==-8]<- NA
lfs$EDAGE[lfs$EDAGE==96]<- NA
lfs$EDAGE[lfs$EDAGE==97]<- 0
summary(lfs$EDAGE)
#could cetegorise this 

summary(lfs$bhealthx)

### Ifentifiers and weighting variables

summary(lfs$CASENOP)
summary(lfs$HSERIALP)
summary(lfs$pwt18x)


fvars = c("TEN1","housex", "sexx","AGES","NTNLTY12","regionx", "numchild04.cat","numchild516.cat","AYFL19.cat", "ETHUKEUL", "fbx", 
          "arrivalx", "marstax", "LIV12W",  "marcivx", "INECAC05", "statusx", "ILODEFR",  "GRSSWK.cat", "HOURPAY.cat",
          "SOC10M", "SC10MMJ","nsecm10x","JOBTYP","ptimex","ptimehrs", "PUBLICR", "MANAGER", "SECJOB", "YTETJB","WRKING","OWNBUS",
          "RELBUS", "STATR","LOOK4","LKYT4", "WAIT","START", "LIKEWK","YSTART","NOLWM","HIQUAL15","HIQUL15D","LEVQUL15","EDAGE" )

cvars = c("AGE","numchild04", "numchild516","AYFL19","GRSSWK", "HOURPAY", "CONMPY", "yearsAtComp",  "TTUSHR", "bhealthx"  )

#Get rid of any factor categories with 0 respondents- these are useless for our analysis
for (n in fvars){
  lfs[[n]] <- factor(lfs[[n]])
}









###########################################################
###Question 2####
#point a
#part time hours using both variables
lfs.w <- svydesign(ids=~1, data = lfs, weights = lfs$pwt18x)
prop.table(svytable(~ptimehrs+sexx,lfs.w),2)
prop.table(svytable(~ptimex+sexx,lfs.w),2)

#Lets look at why this happens

#Do women/men decrease their working hours to care for children?
#caring duties

#indicator if person has a under school age child
lfs$child04 <- factor(ifelse(lfs$numchild04 == 0, 0, 1))
lfs$child516 <- factor(ifelse(lfs$numchild516 == 0, 0, 1))
lfs$nochild <- factor(ifelse(lfs$numchild04 == 1 | lfs$numchild516 == 1, 0, 1))

lfs$childStatus <- NA
lfs$childStatus <- replace(lfs$childStatus, which(lfs$nochild==1), "No children aged 16 or below")
lfs$childStatus <- replace(lfs$childStatus, which(lfs$child04==1), "Has children aged 0-4 yrs")
lfs$childStatus <- replace(lfs$childStatus, which(lfs$numchild516==1), "Has children aged 5-16 yrs")
lfs$childStatus <- factor(lfs$childStatus)

#proportions for no dependent 
lfs.w <- svydesign(ids=~1, data = lfs, weights = lfs$pwt18x) #reload because added variables
prop.table(svytable(~ptimehrs+sexx,lfs.w[which(lfs$nochild==1),] ),2) 
prop.table(svytable(~ptimehrs+sexx,lfs.w[which(lfs$child04==1),] ),2) 
prop.table(svytable(~ptimehrs+sexx,lfs.w[which(lfs$child516==1),] ),2) 



#This shows a trend of men increasing their working hours and women decreasing theirs
#Supporting the idea that women and men share caring responsibilities unequally
#What about women who completely stop working?
prop.table(svytable(~WRKING+sexx,lfs.w[which(lfs$nochild==1),] ),2) 
prop.table(svytable(~WRKING+sexx,lfs.w[which(lfs$child04==1),] ),2) 
prop.table(svytable(~WRKING+sexx,lfs.w[which(lfs$child516==1),] ),2) 

#Reasons given for not working
#When there is a child 04 the overwhelming reason is to look after family home from both men and women
#
svytable(~ptimehrs+sexx+WRKING,lfs.w)


lfs$ecStatusFam <- lfs$INECAC05
inActiveOther = c( "Inactive, seeking, unavailable, student",                              
                   "Inactive, seeking, unavailable, temporarily sick or injured",          
                   "Inactive, seeking, unavailable, long-term sick or disabled",           
                   "Inactive, seeking, unavailable, other reason"      ,                   
                   "Inactive, seeking, unavailable, no reason given"  ,                    
                   "Inactive, not seeking, would like, waiting results of job application",
                   "Inactive, not seeking, would like, student"      ,                     
                   "Inactive, not seeking, would like, temporarily sick or injured" ,      
                   "Inactive, not seeking, would like, long term sick or disabled" ,       
                   "Inactive, not seeking, would like, believes no jobs available",        
                   "Inactive, not seeking, would like, not yet started looking"  ,         
                   "Inactive, not seeking, would like, does not need or want employment",  
                   "Inactive, not seeking, would like, retired from paid work",            
                   "Inactive, not seeking, would like, other reason",                      
                   "Inactive, not seeking, would like, no reason given" ,                  
                   "Inactive, not seeking, not like, waiting results of job application",  
                   "Inactive, not seeking, not like, student" ,                            
                   "Inactive, not seeking, not like, temporarily sick or injured",         
                   "Inactive, not seeking, not like, long term sick or disabled",          
                   "Inactive, not seeking, not like, believes no jobs available",          
                   "Inactive, not seeking, not like, not yet started looking" ,            
                   "Inactive, not seeking, not like, does not need or want employment" ,   
                   "Inactive, not seeking, not like, retired from paid work"  ,            
                   "Inactive, not seeking, not like, other reason" ,                       
                   "Inactive, not seeking, not like, no reason given")
inActiveFamily = c("Inactive, seeking, unavailable, looking after family, home", "Inactive, not seeking, would like, looking after family, home","Inactive, not seeking, not like, looking after family, home" )

lfs$ecStatusFam <-combineLevels(lfs$ecStatusFam,  inActiveOther, "Inactive Other")
lfs$ecStatusFam <-combineLevels(lfs$ecStatusFam,  inActiveFamily, "Inactive Family Reasons")

prop.table(svytable(~ecStatusFam+ sexx,lfs.w),2)
#Proportion of men Inactive in the labour force due to family reasons 1.09%
#Proportion of women inactive in the labour force due to family reasons 8.80%

#proportions of part time by male and female and part time  by m/f

prop.table(xtabs(~ecStatusFam+ sexx,lfs[which(lfs$child04==1),]),2)
#this increases when we focus on people with a child where we see those
#inactive from the workforce for family reasons- male 1.71% female 24.9%

prop.table(xtabs(~ecStatusFam+ sexx,lfs[which(lfs$child516==1),]),2)
#with a similar trend in those with a child between 5-16 yrs
# male 1.56% female 15%

#A variable: not working other, not working- family reasons , part time, full time 
lfs$ecStatus <- NA
lfs$ecStatus <- replace(lfs$ecStatus, which( (lfs$WRKING == "Yes") & (lfs$ptimehrs == "Yes")) , "Part time worker" )
lfs$ecStatus <- replace(lfs$ecStatus, which( (lfs$WRKING == "Yes") & (lfs$ptimehrs == "No")) , "Full time worker" )
lfs$ecStatus <- replace(lfs$ecStatus, which( (lfs$WRKING == "No") ) , "Not working- other" )
lfs$ecStatus <- replace(lfs$ecStatus, which( (lfs$WRKING == "No") & (lfs$ecStatusFam == "Inactive Family Reasons")) , "Not working- Family Reasons" )
lfs$ecStatus <- factor(lfs$ecStatus)

#want to plot: female vs male
ggplot(lfs[which(!is.na(lfs$childStatus) & !is.na(lfs$ecStatus)),], aes(x=ecStatus ))+
  geom_bar(aes( y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], weight=pwt18x, fill=sexx ), position="dodge") +
  scale_fill_manual(name= "Gender", values=c("#b0ddff","#ffb0df" ))+
  labs(title= "Employment status of individuals split into groups based on dependence of children (weighted data)",
       x="Employment type", y="Percentage of people in each status group"  )+
  facet_wrap(~childStatus, scales = "fixed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = percent, expand = c(0,0))

prop.table(svytable(~ecStatus+sexx, lfs.w[which(lfs$childStatus=="Has children aged 0-4 yrs"),] ) )
prop.table(svytable(~ecStatus+sexx, lfs.w[which(lfs$childStatus=="Has children aged 5-16 yrs"),] ) )
prop.table(svytable(~ecStatus+sexx, lfs.w[which(lfs$childStatus=="No children aged 16 or below"),] ) )




#Point b
#Women choose to work in low-paid roles and sectors; 
#sector worked in by gender
prop.table(svytable(~SC10MMJ+sexx, lfs.w),1)


levels(lfs$SC10MMJ) <- c('Managers, Directors And Senior Officials',
                         'Professional Occupations', 
                         'Associate Professional And Technical Occupations',
                         'Administrative And Secretarial Occupations',
                         'Skilled Trades Occupations',
                         'Caring, Leisure And Other Service Occupations',
                         'Sales And Customer Service Occupations',
                         'Process, Plant And Machine Operatives',
                         'Elementary Occupations')                        
                           

aggGW = aggregate(GRSSWK~SC10MMJ, lfs, FUN = mean)
aggGW <- aggGW[order(aggGW$GRSSWK),]
labelsGW <- paste(aggGW$SC10MMJ, "-£", round(aggGW$GRSSWK, 2),"GWP" )
gwOrder =  as.vector(aggGW$SC10MMJ)


svyby(~GRSSWK,by=~SC10MMJ,design=lfs.w,FUN =svymean, na.rm = TRUE)

#Plot to show proportion differences in gender by sector
lfs[which(!is.na(lfs$SC10MMJ)),] %>%
  mutate(SC10MMJ = fct_relevel(SC10MMJ, gwOrder )) %>%
ggplot( aes(x=SC10MMJ,weight=pwt18x, fill=sexx))+
  geom_bar( position="fill") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels=labelsGW) +
  scale_fill_manual(name= "Gender", values=c("#b0ddff","#ffb0df" ))+
  coord_flip()+
  labs(title= "The proportion differences in gender of those employed by each sector (weighted data)",
       y="Proportion of employees in given sector", x="Sector of employment and average gross weekly pay per sector (GWP) "  )+
  geom_hline(aes(colour='#bfc3c9'), yintercept=c(0.5), linetype="dotted" )



#We can see from this graph that some sectors have a very clear disparity in gender
#those sectors with disproportionately high female numbers are 
#average gross weekly pay in brackets 
#Caring, Leisure, and other service occupations (£285.95)
#and administrative and secretarial occupations (£394.25)
#Sales and customer service (£295.25)

# sectors which are disproportianately male are: 
#skilled trades occupations:(£520.83)
#Managers directors (£874.28)

#We can see that those that are disproportiantely male are much higher paying than those that are disprop female
#However there are both high paying and low paying sectors with a fairly even spread of gender: 
#elementary occupations(£269.33)
# Associate Professional And Technical Occupations(£650.43)

#Highest paid 
#Proportion of Managers, directors and senior officials men:0.633, women: 0.367
#Lowest paid sector 
#Elementary occupations male 0.5078107 female 0.4921893

#Would say that yes those sectors which are overwhelmingly female are much lower payong than 
#those that are male dominated

#Missing data here is 25.7% for SC10MMJ
#
summary(lfs$sexx)

summary(lfs$SC10MMJ)
#25.7% missing data 
summary(lfs[is.na(lfs$SC10MMJ),]$sexx)
#21.1% of men didnt respond 29% of women didnt respond 
# could be some bias here as the proportion of missing data is not evenly spread
summary(lfs$GRSSWK)
#82.9% missing 
summary(lfs[is.na(lfs$GRSSWK),]$sexx)
#82.9% men  82.7% women didnt respond- missing at random




#Part c: Senior roles 
prop.table(svytable(~MANAGER+sexx, lfs.w),1)
prop.table(svytable(~SOC10M+sexx, lfs.w),1)

#Male:Female
#Manager                   0.5726197 0.4273803
#Foreman or supervisor     0.4862051 0.5137949
#Not manager or supervisor 0.4454106 0.5545894
#For the extrememly senior roles the difference in gender is much more significant 
#'Chief executives and Snr officials'                   0.730158730 0.269841270 - soc10m

summary(lfs$sexx)
#NAs in nsecm10x 5.8%

#NAs in MANAGER 36.3%
summary(lfs[is.na(lfs$MANAGER),]$sexx)
#37.1% missing in women, 35.5%
#would classify this as missing at random

#NAs in SOC10M 25.6%
#Use ncsem10x


#We have 3221 observations unclassified around 5% asumme missingness is random 
snrRoles <- lfs[which( (lfs$nsecm10x == "1.0 Employers in large organisations") |
                        (lfs$nsecm10x == "8.1 Employers in small orgs non-professional") |
                        (lfs$nsecm10x == "8.2 Employers in small orgs agriculture") |
                        (lfs$nsecm10x == "2.0 Higher managerial occupations" )   | 
                        (lfs$nsecm10x == "5.0 Lower managerial occupations" )    |
                        (lfs$nsecm10x == "6.0 Higher supervisory occupations" )    |
                        (lfs$nsecm10x == "10.0 Lower supervisory occupations" )    ), ]
snrRoles$nsecm10x <- factor(snrRoles2$nsecm10x )
levels(snrRoles$nsecm10x) <- c("Employers in large organisations",
                                "Higher managerial occupations",           
                                "Lower managerial occupations",
                                "Higher supervisory occupations",          
                                "Employers in small orgs non-professional",
                                "Employers in small orgs agriculture",     
                                "Lower supervisory occupations" )
smallOrgs <- c( "Employers in small orgs non-professional","Employers in small orgs agriculture")
snrRoles$nsecm10x <-combineLevels(snrRoles$nsecm10x, smallOrgs, "Employers in small orgs")

orderNs = c("Employers in large organisations",
            "Employers in small orgs", 
            "Higher managerial occupations",           
            "Lower managerial occupations",
            "Higher supervisory occupations",          
            "Lower supervisory occupations" )

snrRoles.w <- svydesign(ids=~1, data = snrRoles, weights = snrRoles$pwt18x)
prop.table(svytable(~nsecm10x+sexx, snrRoles.w),1)



snrRoles %>%
  mutate(nsecm10x = fct_relevel(nsecm10x, orderNs )) %>%
ggplot( aes(x=nsecm10x,weight=pwt18x, fill=sexx))+
  geom_bar( position="fill") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name= "Gender", values=c("#b0ddff","#ffb0df" ))+
  coord_flip()+
  labs(title= "The proportion of each gender employed in various senior roles (weighted data)",
       y="Proportion of employees", x="Type of senior role"  )+
  geom_hline(aes(colour='#bfc3c9'), yintercept=c(0.5), linetype="dotted" )




