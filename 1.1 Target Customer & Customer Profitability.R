setwd('C:/Users/Jacky Yang/Desktop/Marketing Analytics/Recommend System')
library(data.table)
library(dplyr)
transaction = fread("transaction_table.csv",header = T)
product = fread("product_table.csv",header = T)
#merge two original tables by product id
transaction_product = merge(transaction, product, by = "prod_id",all.x = T)

#-------------------------------Filter transactions by year and sales amount of 5000---------------------------------------
#only select the year 2017
transaction2017 = transaction_product[tran_dt>="2017-01-01" & tran_dt<="2017-12-31"]
#get the total spend amount
transaction2017_sum = transaction2017[,total:=lapply(.SD,sum),by = "cust_id",.SDcol = "tran_prod_paid_amt"]

#only select the total spend amount over $5000
target_customer = transaction2017_sum[total>=5000,]


#-------------------------------Use combined customer id, transaction id and store id as unique identifier---------------------------------------
#combine cust_id, tran_id,store_id
target_customer[,id:=paste0(cust_id,tran_id,store_id)]


#-------------------------------Calculate profitability for each customer---------------------------------------
#get the product price after discount
target_customer[, dis_unitprice:=(prod_unit_price+tran_prod_discount_amt/tran_prod_sale_qty), by = 'prod_id']
#find out the lowest price by product id as cost to calculate profit
target_customer[, min_price:=min(dis_unitprice), by = 'prod_id']
#get profit for each product
target_customer[,profit:=(dis_unitprice-min_price)*tran_prod_sale_qty]
#get profit and sale by transaction level
target_customer[,tran_profit:=sum(profit), by = c('cust_id')]
#get the total transation paid amount by three ids, which can used to create a unique id
target_customer[,total_tran_sale:=sum(tran_prod_paid_amt), by = c('tran_id','cust_id','store_id')]
target_customer[,sale_customer:=sum(tran_prod_paid_amt), by = c('cust_id')]
#get weighted percentage 
target_customer[,tran_profit_percent:=tran_profit/total_tran_sale]
target_customer[,cust_profit_percent:=tran_profit/sale_customer]

#delete the bad instance that has negative tran_prod_paid_amt
target_customer<-target_customer[!which(dis_unitprice==-0.55),]

#sanity check from a high level
summary(target_customer$min_price)
summary(target_customer$dis_unitprice)
summary(target_customer$profit)
summary(target_customer$tran_profit)
summary(target_customer$tran_profit_percent)
summary(target_customer$cust_profit_percent)
target_customer

#sanity check from a low level to see if the minimum price of a product looks right
target_customer[target_customer$min_price==0.89]
target_customer[target_customer$dis_unitprice==1.19]
target_customer[target_customer$prod_id==999191212]

target_customer[, customer_profit_percent:=sum(profit)/sum(total_tran_sale), by = "cust_id"]
summary(target_customer$customer_profit_percent)
#only select the relevant columns
profit<-target_customer[,c("cust_id","cust_profit_percent")]
profit<-unique(profit)

write.csv(target_customer,'target_customer.csv')
write.csv(profit,"profit.csv")
