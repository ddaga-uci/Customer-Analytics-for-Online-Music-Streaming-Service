#############################libraries########################
#install.packages("pastecs")
library(pastecs)
library(numpy)
library(dplyr)
library(ggplot2) 
#install.packages("MatchIt")
library(MatchIt)
library(psych)  
library(gridExtra)

############################getting Started####################################
#read data in the file
highnote <- read.csv('C:\\Users\\darsh\\OneDrive\\Winter Quarter\\BANA 277 - Cust & Social Analytics\\Week 7\\HighNote Data Midterm.csv')

#initial data sanity check
head(highnote)
dim(highnote) #43827    16

#checking for any missing values
sum(is.na(highnote)) #no missing value

highnote$user <- ifelse(highnote$adopter==1,'Adopters','Non_Adopters')
table(highnote$user)

############################Question 1 Summary statistics######################


#removing ID as it not important for our analysis
highnote <-subset(highnote,select = -ID)

#We need to understand how are the adopter and non-adopter different in their variables
#descriptive stats for adopters

a<- highnote[highnote$adopter==1,c(0,1,2,3,4,5,6,7,8,9,10,11,12,14,15)] 
stat_a<-psych::describe(a,skew=FALSE)

#descriptive stats for non adopters
n_a<- highnote[highnote$adopter==0,c(0,1,2,3,4,5,6,7,8,9,10,11,12,14,15)] 
stat_n_a<-psych::describe(n_a,skew=FALSE)

#print results for descriptive statistics 
print("Non_Adopters, Adopter = 0")
stat_a
print("Adopters, Adopter = 1")
stat_n_a

#Analyzing adopters and not adopters by comparing mean value of variables using t test
lapply(highnote[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
                   'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
                   'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country', 'subscriber_friend_cnt')], 
       function(x) t.test(x ~ highnote$adopter))
#on average non-adopters have 0.417 subscriber friends and premium customer have 1.67 subscriber friends and is highly significant
###Most of the variable are significantly different 

############################Question 2 Visualization########################
##################I have use Minitab & Tableau for Visualizations#####################
#DATA VISUALIZATION
#demographics
#age
ggplot(highnote,aes(x=age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#male
ggplot(highnote,aes(x=male,group=adopter,fill=adopter))+
  geom_bar(position="dodge")+theme_classic()

#good_country
ggplot(highnote,aes(x=good_country,group=adopter,fill=adopter))+
  geom_bar(position="dodge")+theme_classic()

#PEER INFLUENCE
#friend_cnt
friend_cnt<-highnote %>%
  group_by(adopter)%>%
  summarise(friend_cnt=mean(friend_cnt))
ggplot(friend_cnt,aes(x = adopter,y=friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#friend age
ggplot(highnote,aes(x=avg_friend_age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#friend country count
ggplot(highnote,aes(x=friend_country_cnt,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#subscriber friend count
subscriber_friend_cnt<-highnote %>%
  group_by(adopter)%>%
  summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt))
ggplot(subscriber_friend_cnt,aes(x = adopter,y=subscriber_friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#avg friend male
avg_friend_male<- highnote %>%
  group_by(adopter)%>%
  summarise(avg_friend_male=mean(avg_friend_male))
ggplot(avg_friend_male,aes(x = adopter,y=avg_friend_male)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


#USER ENGAGEMENT
#songs listened
songsListened<- highnote %>%
  group_by(adopter)%>%
  summarise(songsListened=mean(songsListened))
ggplot(songsListened,aes(x = adopter,y=songsListened)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#loved tracks
lovedTracks<- highnote %>%
  group_by(adopter)%>%
  summarise(lovedTracks=mean(lovedTracks))
ggplot(lovedTracks,aes(x = adopter,y=lovedTracks)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#posts
posts<-highnote %>%
  group_by(adopter)%>%
  summarise(posts=mean(posts))
ggplot(posts,aes(x = adopter,y=posts)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#playlists
playlists<-highnote %>%
  group_by(adopter)%>%
  summarise(playlists=mean(playlists))
ggplot(playlists,aes(x = adopter,y=playlists)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#shouts
shouts<-highnote %>%
  group_by(adopter)%>%
  summarise(shouts=mean(shouts))
ggplot(shouts,aes(x = adopter,y=shouts)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#tenure
tenure<- highnote %>%
  group_by(adopter)%>%
  summarise(tenure=mean(tenure))
ggplot(tenure,aes(x = adopter,y=tenure)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

############################Question 3 PCM########################
####reading the data in for Question 3######
Q3 <- highnote

#As per the question users who have more than 1 friend are grouped as 1 and rest are grouped as 0 for treatment
Q3$subscriber_friend_cnt <- ifelse(Q3$subscriber_friend_cnt >= 1, 1, 0)


#understanding how subscriber friend count affects the premium or non-premium customers
#Start by seeing if there is a difference in mean for both the group(adopter non-adopter),of subscriber friend count
Q3 %>%group_by(adopter) %>% summarise(friend_cnt_subscriber = mean(subscriber_friend_cnt),)

# there is a difference in mean, need to check if it's significant

#running a t test to check significance
with(Q3, t.test(subscriber_friend_cnt ~ adopter))
# the subscriber friend count is significant for the adopters and can help us understand their behavior

##################################----PSM Model----###################################
#Lot of data is skewed to reduce the effect of skeweness I decided to use logs
#Also for the Propensity score matching we have used all the variables
Q3_psm1 <- glm(subscriber_friend_cnt ~ log(age) + 
                 male +
                 log(friend_cnt+1) + 
                 log(avg_friend_age+1)  + 
                 log(avg_friend_male+1) + 
                 log(friend_country_cnt+1) + 
                 log(songsListened+1) + 
                 log(lovedTracks+1) + 
                 log(posts+1) + 
                 log(playlists+1) + 
                 log(shouts+1) + 
                 log(tenure+1) + 
                 good_country, data = Q3 , family = binomial())

summary(Q3_psm1)
#All the variables are highly significant for formulating propensity scores. 

# Using this model, we can now calculate the propensity score for each customer. 
# It is simply the customer's predicted probability of being Treated, 
# given the estimates from the logit model.
prs_psm1 <- data.frame(pr_score = predict(Q3_psm1, type = "response"),
                       subscriber_friend_cnt = Q3_psm1$model$subscriber_friend_cnt)
head(prs_psm1)


###Examining the region of common support####
labs <- paste("Treatment Type:", c("0 Subscriber_Friend_Cnt", "1 or more Subscriber_Friend_Cnt"))
prs_psm1 %>%
  mutate(subscriber_friend_cnt = ifelse(subscriber_friend_cnt == 0, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(fill = 'red', color ="white", bins = 40) +
  facet_wrap(~subscriber_friend_cnt) +
  xlab("Probability of subscriber friends") +
  theme_bw()+xlim(0,1.01)

#matchit

#######The method we use below is to find pairs of observations that have very 
#similar propensity scores, but that differ in their treatment status. We use 
#the package MatchIt for this. This package estimates the propensity score in 
#the background and then matches observations based on the method of choice 
#("nearest" in this case). We also used caliper at 0.5. Calipers ensure paired 
#units are close to each other on the calipered covariates, 
#which can ensure good balance in the matched sample. 

mod_match <- matchit(subscriber_friend_cnt ~ log(age) + 
                       male +
                       log(friend_cnt+1) + 
                       log(avg_friend_age+1)  + 
                       log(avg_friend_male+1) + 
                       log(friend_country_cnt+1) + 
                       log(songsListened+1) + 
                       log(lovedTracks+1) + 
                       log(posts+1) + 
                       log(playlists+1) + 
                       log(shouts+1) + 
                       log(tenure+1) + 
                       good_country, data = Q3, model='logit', caliper=.05,method="nearest")

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)
summary(mod_match)

dta_m<-match.data(mod_match)
dim(dta_m)

#############difference in means 
cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male','friend_country_cnt',
         'songsListened','lovedTracks','posts','playlists',
         'shouts','tenure','good_country')
dta_m %>%
  group_by(subscriber_friend_cnt) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean)) %>% data.frame 

###########Run t test to check significance 
lapply(cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$subscriber_friend_cnt)
})


##########visual inspection ####################
fn_bal <- function(dta_m, variable) {
  dta_m$variable <- dta_m[, variable]
  dta_m$subscriber_friend_cnt <- as.factor(dta_m$subscriber_friend_cnt)
  support <- c(min(dta_m$variable), max(dta_m$variable))
  ggplot(dta_m, aes(x = distance, y = variable, color = subscriber_friend_cnt)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}
# Making grids to see effect of propensity score matching
grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "male"),
  fn_bal(dta_m, "friend_cnt"),
  fn_bal(dta_m, "avg_friend_age"),
  nrow = 2 
)

grid.arrange(
  fn_bal(dta_m, "avg_friend_male"),
  fn_bal(dta_m, "friend_country_cnt"),
  fn_bal(dta_m, "songsListened"),
  fn_bal(dta_m, "lovedTracks"),
  nrow = 2 
)

grid.arrange(
  fn_bal(dta_m, "posts"),
  fn_bal(dta_m, "playlists"),
  fn_bal(dta_m, "shouts"),
  fn_bal(dta_m, "tenure"),
  fn_bal(dta_m, "good_country"),
  nrow = 3 
)



############################Question 4 Treatment Effect########################
####reading the data in for Question 4######

Q4 <- dta_m

####t. test ########
with(Q4, t.test(adopter ~ subscriber_friend_cnt))

##Model1 - testing with subscriber_friend_count

Model1 <- glm(adopter ~ subscriber_friend_cnt, data = Q4, family = binomial())
summary(Model1)

cbind(exp(coef(Model1)))

##Model2 - testing with all variables
Model2 <- glm(adopter ~ log(age) + 
                   male +
                   log(friend_cnt+1) + 
                   log(avg_friend_age+1)  + 
                   log(avg_friend_male+1) + 
                   log(friend_country_cnt+1) + 
                   log(songsListened+1) + 
                   log(lovedTracks+1) + 
                   log(posts+1) + 
                   log(playlists+1) + 
                   log(shouts+1) + 
                   log(tenure+1) + 
                   good_country +
                   subscriber_friend_cnt,data = Q4, family = binomial())

summary(Model2)
cbind(exp(coef(Model2)))

##Model3 - testing the model2 on orginal data
Model3<- glm(adopter ~ log(age) + 
                   male +
                   log(friend_cnt+1) + 
                   log(avg_friend_age+1)  + 
                   log(avg_friend_male+1) + 
                   log(friend_country_cnt+1) + 
                   log(songsListened+1) + 
                   log(lovedTracks+1) + 
                   log(posts+1) + 
                   log(playlists+1) + 
                   log(shouts+1) + 
                   log(tenure+1) + 
                   good_country +
                   subscriber_friend_cnt,data = highnote, family = binomial())

summary(Model3)

cbind(exp(coef(Model3)))
