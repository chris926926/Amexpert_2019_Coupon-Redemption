# Remove all objectives in the environment
rm(list=ls())

#Get filepath of the current working directory 
getwd()
setwd("C:/Users/yusha/OneDrive/Desktop/SDSU/Project 620 Group")


# load packages
library(ISLR)
library(dplyr)
library(caret)
library(psych)
library(ggplot2)
library("VIM")
library(ROSE)
library(DMwR)


# load the all tables into R and show the structure of the data.
Campaign_data<-read.csv("campaign_data.csv",header=T,na.strings="")
items<-read.csv("item_data.csv",header = T,na.strings = "")
coupon_item<-read.csv("coupon_item_mapping.csv",header = T,na.strings = "")
customer<-read.csv("customer_demographics.csv",header = T,na.strings = "")
transaction<-read.csv("customer_transaction_data.csv",header = T,na.strings = "")
train_data<-read.csv("train.csv",header = T,na.strings = "")

# check the datasets' structure
str(Campaign_data)        # 28 obs. of 4 variable
str(items)                # 74066 obs. of 4 variable
str(coupon_item)          #92663 obs. of 2 variables
str(customer)             # 760 obs. of 7 variable
str(transaction)          #1324566 obs of  7 variables
str(train_data)           #  78369 obs. of 5 varialbes

# summary the data 
summary(Campaign_data)
summary(items)
summary(coupon_item)
summary(customer)  # has missing value
summary(transaction)  # there ares some outliers in quantity and selling price.
summary(train_data)

# checking missing data, only customer dataset has missing value
sum(is.na(Campaign_data))
sum(is.na(items))
sum(is.na(coupon_item))
sum(is.na(customer)) # 867 missing value
sum(is.na(transaction))
sum(is.na(train_data))

#Since our dataset includes 6 excel file. We will explore each tabel firstly and then merge them
################################# Explore Campaign_data###################################################
attach(Campaign_data)

# change the start and end  date to duration
Camp_start<- as.Date(start_date,format="%d/%m/%y")
Camp_end<- as.Date(end_date,format="%d/%m/%y")
duration<-as.integer(difftime(Camp_end,Camp_start,units=c("days")))
duration

# Add new variable Camp_duration in the table
Campaign_data <- mutate(Campaign_data,duration)
hist(duration,col = "blue")   # most campaign duration occur in  30-35days 


#######################################Explore Customer Demographics##########################
# handle the missin value in customer dataset
aggr(customer,prop=FALSE,numbers=TRUE,cex.axis=.7) # marital_status and no_of_children have missing value 
sum(is.na(customer$marital_status)) # 329 value missing 
customer$no_of_children<-as.character(customer$no_of_children)
customer$no_of_children[is.na(customer$no_of_children)]<-'0'
str(customer$no_of_children)

customer$no_of_children<-recode(customer$no_of_children,"3+"="3")
length(unique(customer$customer_id))
length(unique(train_data$customer_id))
# Since family size has some relation with marital status and no_of children. Let plot the family siz group by marital status
x<-table(customer$marital_status,customer$family_size)
barplot(x,main ="family size distribution by marital status",xlab="family size",col = c("blue","orange"),legend=rownames(x),beside=TRUE,axis.lty=1)

#From above plot, we can see the family size is 1 and matrital status is single, which make sense, but for family size 3,4,5+,the single value still exist.
#If family_size > 1 we canNOT conlcude he/she is Married since a person can have children and still be Single. But, your children would definitely add to your family_size. So we can use family_size and no_of_children to impute missing values for marital_status.
#family_size would include the person itself as well as possible spouse and children
#If family_size = no_of_children + 1 then we can conclude he/she is Single.
#If family_size = no_of_children + 2 he/she is Married.
levels(customer$family_size)["5+"]<-"5"
customer$family_size<-as.integer(customer$family_size)
customer$no_of_children<-as.integer(customer$no_of_children)
head(customer$no_of_children,20)

status<-(is.na(customer$marital_status)) & (customer$family_size - customer$no_of_children==1)
sum(status)
customer$marital_status[status]<-'Single'
status2<-(is.na(customer$marital_status)) & (customer$family_size - customer$no_of_children==2)
customer$marital_status[status2]<-'Married'
sum(is.na(customer))  # now customer data has no missing value

###########################################Explore coupon_item mapping and items data 
# since coupon_item mapping dataset only has coupon id and items id, so we plan to merge it with items dataset
length(unique(coupon_item$coupon_id)) # 1116 coupon coupons
length(unique(coupon_item$item_id)) # 36289 items
length(unique(items$item_id))  # 74066, 48.9% of items were under promotion at once. 
length(unique(items$brand)) # 5528 brand 
length(unique(items$brand_type)) # 2 brand type
length(unique(items$category)) # 19 category

# merge items data with coupon_item mapping 
attach(coupon_item)
attach(items)

coupon_item_New<-merge(coupon_item,items,by="item_id",all.x = TRUE)
str(coupon_item_New)
sum(is.na(coupon_item_New))  # no missing value


# plot category on which most coupons are given
t1<- coupon_item_New %>% filter(!is.na(coupon_item_New$coupon_id)) %>% group_by(coupon_item_New$category) %>% count()
ggplot(t1,aes(x=`coupon_item_New$category`,y=n))+ geom_bar(stat="identity",fill="blue",colour="blue",position = position_stack(), width=0.5)+
  ggtitle("Groupon Given By Category")+
  ylab("coupon_counts") +
  xlab("Category") +   
  theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1))+
  list()           # grocery and pharmacy items are given most coupons

#plot brand on which most coupons are given
p_brand<-coupon_item_New %>% filter(!is.na(coupon_item_New$coupon_id)) %>%group_by(coupon_item_New$brand) %>% count()
p_brand1<-head(arrange(p_brand,desc(p_brand$n)),n=10)
ggplot(p_brand1,aes(x=as.character(`coupon_item_New$brand`),y=n))+geom_bar(stat="identity",fill="blue")+
  ggtitle("Groupon Given By Brand")+
  ylab("coupon_counts") +
  xlab("Brand")    # most of coupon are given on brand 56

# plot brand type on which most coupon are given
type<-coupon_item_New %>% filter(!is.na(coupon_item_New$coupon_id)) %>%group_by(coupon_item_New$brand_type) %>% count()
type
ggplot(type,aes(x=type$`coupon_item_New$brand_type`,y=type$n))+geom_bar(stat="identity",fill="blue")+
  ggtitle("Groupon Given By Brand Type")+
  ylab("coupon_counts") +
  xlab("Brand_type") # most of coupon on given on Established brand

#Aggregate the coupon_item_new dataset on coupon id. create new variables
#  New varibales :Number of items on Coupon
#                 Number of brands on Coupon 
#                 Number of brand type on coupon
#                 Number of Category on Coupon
coupon_item_agrregate<-aggregate(coupon_item_New[,-2],by=list(coupon_item_New$coupon_id),function(x) length(unique(x)))
names(coupon_item_agrregate)<-c("coupon_id","no_items_on_coupon","no_brands_on_coupon,","no_brandType_on_Coupon","no_category_on_Coupon")
head(coupon_item_agrregate)

#################################Expplore customer Transaction dataset 
str(transaction)
#correct date format
transaction$date<-as.Date(transaction$date,format="%Y-%m-%d")

length(unique(transaction$item_id))  # compared with data in items dataset, 3 items were  never sold 


# Merge customer Transaction data with Item Data on item_id 
transaction_new<-merge(transaction,items,by="item_id",all.x = TRUE)
str(transaction_new)  #1324556 *10
sum(is.na(transaction_new))  # no missing value

# using coupon_discount to check coupon redeem status 
length(which(transaction_new$coupon_discount!=0))  # 21286 coupons were redeemd

# aggregat some features in transaction_new by customer_id. 
# how much amount customer spend
revenue_from_customer<-aggregate(transaction_new$selling_price,by=list(transaction_new$customer_id),sum)
head(revenue_from_customer)
names(revenue_from_customer)<-c("customer_id","revenue_from_customer")

#how many items customer spend
purchased_by_customer<-aggregate(transaction_new[,c("item_id","brand","brand_type","category")],by=list(transaction_new$customer_id),function(x){length(unique(x))})
names(purchased_by_customer)<-c("customer_id","no_of_itemPurchased","no_of_brandPurchased","no_of_brandType_Purchased","no_of_CategoryPurchased")
head(purchased_by_customer)

coupon_disc_purchase_by_customer <- aggregate(transaction_new[transaction_new$coupon_discount != 0,c("item_id","brand","brand_type","category")],by = list(transaction_new[transaction_new$coupon_discount != 0,"customer_id"]),function(x){length(unique(x))})
names(coupon_disc_purchase_by_customer) <- c("customer_id","no_items_purchased_CouponRedeem","no_brands_purchased_CouponRedeem",
                                             "no_brand_type_purchased_CouponRedeem","no_category_purchased_CouponRedeem")
head(coupon_disc_purchase_by_customer)


# add new feature: customer_coupon_redemption_chance , historical item purchase by customer
customer_coupon_info <- merge(transaction,coupon_item,by = "item_id",all.x = T) 
customer_coupon_redemption_chance <-customer_coupon_info[!is.na(customer_coupon_info$coupon_id),c("customer_id","coupon_id")]
customer_coupon_redemption_chance$redemption_chance <- 1
customer_coupon_redemption_chance <- unique(customer_coupon_redemption_chance) #226598 coupon redeemed 

setDT(transaction_new)
# historical transaction : average selling price,coupon discount, quantity,coupon discount, customer dicount per brand by customer id
avg_custom_price<-transaction_new[,mean(selling_price),by=.(customer_id)]
avg_custom_discount<-transaction_new[,mean(coupon_discount),by=.(customer_id)]
avg_custom_quantity<-transaction_new[,mean(quantity),by=.(customer_id)]
avg_custom_otherdiscount<-transaction_new[,mean(other_discount),by=.(customer_id)]
avg_custom_brand<-transaction_new[,length(unique(brand)),by=.(customer_id)]
avg_custom_disc_per_brand<-transaction_new[,mean(coupon_discount),by=.(customer_id,brand)]
avg_custom_disc_per_brand<-avg_custom_disc_per_brand[,mean(V1),by=.(customer_id)]


#########################################Agrregate dataset
#merge train data with Campaign data 
data_1<-merge(train_data,Campaign_data,by.x ="campaign_id",all.x = TRUE)
head(data_1)
str(data_1)   #78369 * 9

#merge train_1 with customer demographics 
data_2<-merge(data_1,customer,by.x ="customer_id",all.x = TRUE)
head(data_2)
sum(is.na(data_2))  #208248 missing value,which means there are customer ids present in train which are not present in cust_demo
sapply(data_2, function(x) sum(is.na(x)))   # check which varaible has missing value

# merge data_2 with coupon_item_agrregate
data_3<-merge(data_2,coupon_item_agrregate,by.x = "coupon_id",all.x = TRUE)
head(data_3)

#merge data_3 with revenue_from_customer
data_4<-merge(data_3,revenue_from_customer,by = "customer_id",all.x = T)
head(data_4)
# merge data_4 with purchased_by_customer
data_5<-merge(data_4,purchased_by_customer,by = "customer_id",all.x = T)
# merge data_5 with coupon_discount_purchased by customer 
data_6<-merge(data_5,coupon_disc_purchase_by_customer,by = "customer_id",all.x = T)
head(data_6)
#merge data_6 with customer customer_coupon_redemption_chance
data_7 <- merge(data_6,customer_coupon_redemption_chance,by = c("customer_id","coupon_id"),all.x = T)
data_7<-data_7%>%left_join(avg_custom_price,by="customer_id")
data_7<-data_7%>%left_join(avg_custom_discount,by="customer_id")
data_7<-data_7%>%left_join(avg_custom_quantity,by="customer_id")
data_7<-data_7%>%left_join(avg_custom_otherdiscount,by="customer_id")
data_7<-data_7%>%left_join(avg_custom_disc_per_brand,by="customer_id")
names(data_7)
names(data_7)[30:34]<-c("Past_avg_custom_price","Past_avg_custom_discount","Past_avg_custom_quant","Past_avg_custom_otherdiscount","past_custom_disc_per_brand")

########################################Finished Dateset Merge#########################


##################################### clean the final merged table#################################################
#checking missing value
sapply(data_7, function(x) sum(is.na(x)))

data_7$redemption_status <- as.factor(data_7$redemption_status) 

data_7[is.na(data_7$no_items_purchased_CouponRedeem),"no_items_purchased_CouponRedeem"] <- 0
data_7[is.na(data_7$no_brands_purchased_CouponRedeem),"no_brands_purchased_CouponRedeem"] <- 0
data_7[is.na(data_7$no_brand_type_purchased_CouponRedeem),"no_brand_type_purchased_CouponRedeem"]<-0
data_7[is.na(data_7$no_category_purchased_CouponRedeem ),"no_category_purchased_CouponRedeem"]<-0
data_7[is.na(data_7$redemption_chance),"redemption_chance"] <- 0

percentmiss = function(x){sum(is.na(x))/ length(x) * 100}
apply(data_7,2, percentmiss)  # 44.28%  missing value in marital,rented,family-size,no_of_childre,income_bracket,age_range
unique(data_7$no_of_brandType_Purchased)  # all values are 2, not a useful variable. Need drop

aggr(data_7[,10:15],prop=FALSE,numbers=TRUE,cex.axis=.7)

#drop first-sight no use variables
drop<-c("no_of_brandType_Purchased","id","start_date","end_date","rented","marital_status","family_size","no_of_children","income_bracket","age_range")
new_data<- data_7[,!(names(data_7) %in% drop)]

# check unreasonal value
summary(new_data)


#check the redemption statua distribution
prop.table(table(new_data$redemption_status)) #Highly Skewed data (99.07% : 0.93%)  highly unbalanced data

# create dummy variable for campaign type
str(new_data)
dummy <- as.data.frame(model.matrix(~campaign_type, data = new_data))
new_data <- cbind(new_data[,-which("campaign_type" == names(new_data))], dummy[,-1])
names(new_data)[ncol(new_data)] <- names(dummy)[2]


#check the correlation  variables

data.cor <- cor(new_data[,-4])
highCorr <- findCorrelation(data.cor, cutoff=.7)
highCorr #no_brands_purchased_CouponRedeem,revenue_from_customer,no_of_itemPurchased,Past_avg_custom_discount
        # no_brandType_on_Coupon, no_items_on_coupon

# plot correlations among variables
install.packages("GGally")
library(GGally)
ggcorr(new_data[,-4],name = "corr", label = TRUE, hjust = 1, label_size = 3, angle = 0, size = 3,
       low = "grey", mid = "white", high = "orange")

# drop high correlation variables
drop_var<-c("no_brands_purchased_CouponRedeem","revenue_from_customer","no_of_itemPurchased","Past_avg_custom_discount","no_brandType_on_Coupon")
data<- new_data[,!(names(new_data) %in% drop_var)]

# check the correlation after delete the high correlation varaible
data.cor <- cor(data[,-4])
highCorr <- findCorrelation(data.cor, cutoff=.7)
names(data[,-4])
highCorr  # Also no_item_on_coupon still has high correlation, but I think this is an important variable)
ggcorr(data[,-4],name = "corr", label = TRUE, hjust = 1, label_size = 3, angle = 0, size = 3,
       low = "grey", mid = "white", high = "orange")
str(data)
summary(data)
head(data)
datanew <-data %>% distinct()
#write.csv(data,"Data_Final.csv",row.names = FALSE)



