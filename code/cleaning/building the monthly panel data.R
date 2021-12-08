#Building the Dependent Variables

library(tidyverse)

#importing the VSSR data:
VSRR<-read.csv("C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/data/raw/VSRR data pull/VSRR_Provisional_Drug_Overdose_Death_Counts.csv")
VSRR$Indicator %>% table()

#WONDER data
setwd("C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/data/raw/wonder query results/monthly")

#reading in each of the CDC Wonder files
files<-list.files()
wonder_tables<-substr(files,1,nchar(files)-4)
wonder_tables<-gsub(" ", "_", wonder_tables)
for(i in 1:length(files)){
        assign(wonder_tables[i], read.delim(files[i], header = T, stringsAsFactors = FALSE, quote = "", sep = "\t"))
}
rm(files, i)

#retreiving the names for each of the tables
map(wonder_tables, ~names(get(.x)))

#renaming the deaths variable and cleaning each file for a merge
map(wonder_tables, ~assign(wonder_tables[.x], get(wonder_tables[.x])[,6]))
for(i in 1:length(wonder_tables)){
        table<-get(wonder_tables[i])
        names(table)[6]<-paste0(wonder_tables[i], "_deaths")
        table<-table[,c(2,5,6)]
        table[,3]<-as.numeric(table[,3])
        table<-table %>% filter(X.State.!="")
        assign(wonder_tables[i], table)
}

#merging the tables
Wonder<-get(wonder_tables[1])
for(i in 2:length(wonder_tables)){
        Wonder<-merge(Wonder, get(wonder_tables[i]))
}
rm(list=setdiff(ls(), "Wonder"))
names(Wonder)[1:2]<-c("State", "Month")

Wonder<- Wonder %>% mutate(State=substr(State ,2 ,nchar(State)-1),
                           Month=substr(Month ,2 ,nchar(Month)-1))

#building the legal data panel

library(tidyverse)
library(openxlsx)
library(lubridate)

setwd("C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/data/raw/legal data")

#reading in legal data
files<-list.files()[grep(".xlsx", list.files())]
legal_tables<-substr(files,1,nchar(files)-5)
legal_tables<-gsub(" ", "_", legal_tables)
for(i in 1:length(files)){
        assign(legal_tables[i], read.xlsx(files[i], detectDates=T))
}

`20170919.MM.Patients.Stat.Data`$Effective.Date[!is.na(`20170919.MM.Patients.Stat.Data`$`date-first-law`)]<-`20170919.MM.Patients.Stat.Data`$`date-first-law`[!is.na(`20170919.MM.Patients.Stat.Data`$`date-first-law`)]

#date conversion worked for 1, 3, 4, 5, 6 but not for 2
`20170919.PDMP.Access.Stat.Data`<-`20170919.PDMP.Access.Stat.Data` %>% mutate(
        Effective.Date=as.character(Effective.Date),
        Valid.Through.Date=as.character(Valid.Through.Date))
`20170919.PDMP.Access.Stat.Data`<-`20170919.PDMP.Access.Stat.Data` %>% mutate(
        Effective.Date=parse_date_time(Effective.Date, "%Y-%m-%d"),
        Valid.Through.Date=parse_date_time(Valid.Through.Date, "%Y-%m-%d"))
#fixed

legal_tables
map(legal_tables, ~names(get(.x)))

#standardizing the format of the variables
legal_tables
map(legal_tables, ~"Effective.Date" %in% names(get(.x)))
map(legal_tables, ~"Jurisdictions" %in% names(get(.x)))
names(`7_1_2017_Syringe_possession_data`)[1]<-"Jurisdictions"
map(legal_tables, ~"Valid.Through.Date" %in% names(get(.x)))

#creating/isolating the variables for policies to analyze and dropping variables I won't use
legal_tables
`7_1_2017_Syringe_possession_data`<-`7_1_2017_Syringe_possession_data` %>% mutate(
        legal_syringes=case_when(poparaexyn_No==1 | poparaexcpts==1~1, T~0))
#keep legal_syringes
`7_1_2017_Syringe_possession_data`<-`7_1_2017_Syringe_possession_data` %>% select(Jurisdictions, Effective.Date, Valid.Through.Date, legal_syringes)
#this means that possesion of needles, syringes, or hypodermic devices are either legal or allowed through
#a needle exchange program to prevent bloodborne illness

#keep `20170919.MM.Patients.Stat.Data`$mmlaw
`20170919.MM.Patients.Stat.Data`<-`20170919.MM.Patients.Stat.Data` %>% select(Jurisdictions, Effective.Date, Valid.Through.Date, mmlaw)
#=1 if state has a medical marijuana program, 0 otherwise

#keep `20170919.PDMP.Access.Stat.Data`$pmp2012
`20170919.PDMP.Access.Stat.Data`<-`20170919.PDMP.Access.Stat.Data` %>% select(Jurisdictions, Effective.Date, Valid.Through.Date, pmp2012)
#=1 if professionals have access to a prescription drug monitoring program, 0 otherwise

#keep `Naloxone_Data_09112020`$naaddressoaayn
`Naloxone_Data_09112020`<-`Naloxone_Data_09112020` %>% select(Jurisdictions, Effective.Date, Valid.Through.Date, naaddressoaayn)
#=1 if there is a naloxone access law, 0 if not

#keep `Good_Samaritan_Overdose_Prevention_Laws_7.21.2021`$goodsam-law
`Good_Samaritan_Overdose_Prevention_Laws_7.21.2021`<-`Good_Samaritan_Overdose_Prevention_Laws_7.21.2021` %>% select(Jurisdictions, Effective.Date, Valid.Through.Date, `goodsam-law`)
#=1 if there is a good samaritan law, 0 otherwise

#keep MEDD_Data_Page$MEDDPolicy1
MEDD_Data_Page<-MEDD_Data_Page %>% select(Jurisdictions, Effective.Date, Valid.Through.Date, MEDDPolicy1)
#=1 if there is at least one morphine equivelent daily dose law, 0 otherwise

#all tables have 4 columns
map(legal_tables, ~ncol(get(.x)))
map_chr(legal_tables, ~names(get(.x))[4])

#renaming
new_names<-c("MM", "PDMP", "legal_syringes", "goodsam", "MEDD", "NAL")
for(i in 1:6){
        data<-get(legal_tables[i])
        names(data)[4]<-new_names[i]
        assign(legal_tables[i], data)
}

#building the panel (month x year x state) (we want this to be balenced)
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

#adding a state's abbreviation to the panel

#setting boundary dates
start <- as.POSIXct( cut(as.Date("1999/01/01"), "month") )
end <-as.POSIXct( cut(as.Date("2017/01/01"), "month") )
elapsed<-mondf(start, end)

#setting the effective dates for each state/policy
#effective_date<-as.POSIXct( as.Date("2015/06/05") )

#iterating through states
#state_iter<-"Colorado"

legal_data<-data.frame()

for(j in 1:50){
        
        state_iter<-state.name[j]
        
        #building a vector of the first effective date where the policy indicator is turned on for a state
        #in this order: c("MM", "PDMP", "legal_syringes", "goodsam", "MEDD", "NAL")
        effective_dates<-c()
        for(i in 1:length(new_names)){
                data<-get(legal_tables[i]) %>% filter(Jurisdictions==state_iter)
                if(mean(data[,4])>0){
                        effective_dates[i]<-data %>% filter(data[,4]==1) %>% summarize(min(Effective.Date)) %>% unname()
                } else {
                        effective_dates[i]<-NA
                }
        }
        
        #building the panel
        shell<-data.frame(
                state=rep(state_iter, elapsed),
                month=start %m+% months(1:elapsed)
        )
        
        for(i in 1:length(new_names)){
                shell[,2+i]<-ifelse(effective_dates[[i]]>shell$month, 0, 1)}
        shell[is.na(shell)]<-0
        names(shell)[3:8]<-new_names
        
        legal_data<-rbind(legal_data, shell)
}

rm(list=setdiff(ls(), c("Wonder", "legal_data")))

#merging the tables
names(Wonder)[1:2]<-c("state", "month")
Wonder<-Wonder %>% mutate(month=parse_date_time(month, "%Y/%m"),
                          month_character=as.character(month))
legal_data<-legal_data %>% mutate(state=as.character(state),
                                  month_character=as.character(month))

str(legal_data)
str(Wonder)

panel<-merge(Wonder, legal_data, by=c("state", "month_character"))
panel<-panel %>% select(-c(2,17))
names(panel)[2]<-"month"

na_by_state<-panel %>% group_by(state) %>% summarize(na_Opioids_deaths=mean(is.na(Opioids_deaths)),
                                                     na_OD_deaths=mean(is.na(Number_of_Drug_Overdose_Deaths_deaths)),
                                                     na_deaths=mean(is.na(Number_of_Deaths_deaths)),
                                                     na_Other_synthetic_narcotics_deaths=mean(is.na(Other_synthetic_narcotics_deaths)))
na_over_time<-panel %>% group_by(month) %>% summarize(na_Opioids_deaths=mean(is.na(Opioids_deaths)),
                                                      na_OD_deaths=mean(is.na(Number_of_Drug_Overdose_Deaths_deaths)),
                                                      na_deaths=mean(is.na(Number_of_Deaths_deaths)),
                                                      na_Other_synthetic_narcotics_deaths=mean(is.na(Other_synthetic_narcotics_deaths)))
write.csv(na_by_state, "C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/data/clean/na_by_state.csv")
write.csv(na_over_time, "C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/data/clean/na_over_time.csv")

#dealing with missing data

#dropping states with a high proportion of missing data
#drop_due_to_suppression<-na_by_state %>% filter(na_Opioids_deaths>.25)

#`%notin%` <- Negate(`%in%`)
#panel<-panel %>% filter(state %notin% drop_due_to_suppression$state) #30 states remain

#imputing suppressed values as 9
panel[is.na(panel)]<-9

#calculating opioid deaths as a fraction of total deaths
monthly_panel<-panel %>% mutate(Opioids_Death_Rate=Opioids_deaths/Number_of_Deaths_deaths)

#saving the panel
save(monthly_panel, file="C:/Users/Josh/OneDrive - University of Missouri/econometrics 2021/final project/data/clean/clean_monthly_panel.R", replace=T)
