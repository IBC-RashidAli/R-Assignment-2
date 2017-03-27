library(lubridate)
library(dplyr)
library(tidyr)
library(readr)


dst <- read.csv("hospitaldata.csv"
                ,strip.white = T, na.strings = c("-",""," ","\t","\n",NA), stringsAsFactors = F)

dataf <- tbl_df(dst) 



# Q1: Removing dots in the Column Names , in this we use gsub() from Pattern Matching and Replacment 
#     First we compare dot then replace with spaces("") in all columns and finaly View to see colums without Dots
names(dataf) <- gsub("\\.","",names(dataf))
View(dataf)

#2. Which day of the week is expected to have most visits?
#  First we convert Date Data into suitable Date format after that in 2nd step we retrieve max date 
#  and compare it list that date would be equal then again convert it into day for suitble answer     
dataf$Date <- mdy(dataf$Date) 
wday(dataf$Date[which(table(dataf$Date) == max(table(dataf$Date)))],label=T,abbr=F)

#3. What is the average age of patients? 
# Converting data into numeric then calculate average by calling Mean function 
dataf$Age <- parse_number(dataf$Age)
mean(dataf$Age,na.rm = T)

#4. How many children were entertained? (Make a Bracket of Age from 1-12) 
# We use chaining ,in this we select Age filtering with Age between 1 to 12 with count for posible output and View it

tot_child <- dataf %>%
  select(Age) %>%
  filter(Age >= 1 & Age <= 12, !is.na(.)) %>%
  count()
View(tot_child)

#5. Which gender type had what kind of procedure in abundance? i.e. Female visit mostly because of Gynae Problem
#  First we apply chaining then group it with required columns and count it by grouping bith columns
#  and also exclude na values in Sex column finally print or view both columns with its count  

dataf %>%
  group_by(Sex, Procedure) %>%
  count(sort = TRUE) %>%
  filter(!is.na(Sex)) %>%
  View()
#6. Which Doctor is earning highest? 
# First Remove Cancelled to NA as no payment is not given
# And group data by consultng Doctor then sumaarize it 
 
dataf$TotalCharges <- as.numeric(gsub("cancelled", NA, ignore.case = T,dataf$TotalCharges))

hst_earn_doc <-  dataf %>%
  group_by(ConsultingDoctor) %>%
  summarise_each(funs(sum(TotalCharges, na.rm = T))) %>%
  select(ConsultingDoctor, TotalCharges) %>%
  arrange(desc(TotalCharges))
(hst_earn_doc$ConsultingDoctor[which(hst_earn_doc$TotalCharges==max(hst_earn_doc$TotalCharges))])
 

#7. Which procedure type earns more money? 
# First we remove Cancelled to NA 
# Through Chaining or binding grouping it with Procedure column also excluding na vlues from Procedure column 
# Calulating procedure who earns max money
dataf$Procedure <- (gsub("cancelled", NA, ignore.case = T,dataf$Procedure))
hst_earn_proc <-  dataf %>%
  group_by(Procedure) %>%
  summarise_each(funs(sum(TotalCharges, na.rm = T))) %>%
  select(Procedure, TotalCharges) %>%
  filter(!is.na(Procedure)) %>%
  arrange(desc(TotalCharges))
View(hst_earn_proc)
(hst_earn_proc$Procedure[which(hst_earn_proc$TotalCharges==max(hst_earn_proc$TotalCharges))])


#8. Which time of the day has highest frequency of visits by hour? 
#  First excluding na Values on date and time column then group it to count and calculte highest Frequnecy

hft <-  dataf %>%
  filter(!is.na(Date), !is.na(Time)) %>%
  group_by(Date,Time) %>%
  count() %>%
  arrange(desc(n))
View(hft$Time[which (hft$n==max(hft$n))])

#9. Create a bracket of time by Morning, Afternoon, Evening, Night (6am - 12pm - Morning, 12 pm- 4 pm, Afternoon, 4 pm- 7pm, Evening, 7pm - 6 am, Night). 
 
 dataf %>%
  mutate(TimeBracket = ifelse(hour(strptime(dataf$Time, "%I:%M %p")) >= 6 & hour(strptime(dataf$Time, "%I:%M %p")) <= 12, "Morning",
                              ifelse(hour(strptime(dataf$Time, "%I:%M %p")) > 12 & hour(strptime(dataf$Time, "%I:%M %p")) <= 16, "Afternoon",
                                     ifelse(hour(strptime(dataf$Time, "%I:%M %p")) > 16 & hour(strptime(dataf$Time, "%I:%M %p")) <= 19, "Evening",
                                            ifelse(hour(strptime(dataf$Time, "%I:%M %p")) > 19, "Night", NA))))) %>%
  View()


#10. How many patients are repeated visitors?  
# counting Vistors with id filtering it by greater than 1 and finally count it 
RP_vist <-  dataf %>%
  group_by(id) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))%>%
print 
View (count(RP_vist))

#11. Give us the id of repeated visitors.
# Just print Id from above Solution

print(RP_vist$id)

#12. Which patients visited again for the same problem? 
# Counting and excluding na values of Procedure having count greater than 1
RP_Pt  <-  dataf %>%
  group_by(id, Procedure) %>%
  count() %>%
  filter(!is.na(Procedure), n > 1) %>%
  arrange(desc(n))
View(RP_Pt)

#13. What is the median age for Females and Males? 
# First using toupper function to correct form of f into F , Using summazrize Function to find median of Females and Males
med_f_m<- dataf %>% 
  group_by(toupper(Sex)) %>%
  summarize(median(Age, na.rm = T))
View(med_f_m)
 
#14. What is the total amount in balance? 
# First convertig it into numbers then sum of amount in Balance.
dataf$AmountBalance <- as.numeric(gsub(",",'',dataf$AmountBalance) )
sum(dataf$AmountBalance, na.rm = T)


#15. How much money was made by Procedure Type "Consultation"? 
# First grouping or binding  it with sum of total_amount nd compare it with TotalCarges removing na values
# and Filtering it with Consultation
dataf %>%
  group_by(Procedure) %>%
  summarise(Total_Amount = sum(TotalCharges, na.rm = T)) %>%
  filter(Procedure == c("Consultation"))

#16.Is there a relation between Age and Total Charges paid
# There is Direct Propotional realation between  Age and Total Charges paid
dt <- dataf %>%
  filter(!is.na(Age), !is.na(TotalCharges))
print (dt)
cor( dt$TotalCharges,dt$Age)

#17. Which Age group had highest number of visits?
# Highest Number of visits by ages is 30 
highest_age_visit <- dataf %>%
  group_by(Age) %>%
  tally() %>%
  filter(!is.na(Age)) %>%
  arrange(desc(n))
View(highest_age_visit)
highest_age_visit$Age[which(highest_age_visit$n==max(highest_age_visit$n))]

#18. What is the total cost earned by Procedure Type X Ray and Scalling together? 

TotEarn <- dataf %>%
  filter(Procedure == c("X Ray", "Scalling")) %>%
  summarise(Total = sum(TotalCharges, na.rm = T)) 
View(TotEarn)

write.csv(dataf, "CleanData.csv")

