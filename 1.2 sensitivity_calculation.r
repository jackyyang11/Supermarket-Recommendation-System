library(data.table)
transaction = fread("transaction_table.csv",header = T)
product = fread("product_table.csv",header = T)

#make table with all the transactions 
transaction_product = merge(transaction, product, by = "prod_id",all.x = T)

#Making a unique ID per transaction
transaction_product$unique_id=paste(transaction_product$cust_id+
                                      transaction_product$tran_id+
                                      transaction_product$store_id)

#Selecting only those transactions that are between 2017-01-01 and 2017-12-31
tr_2017=transaction_product[which(transaction_product$tran_dt>="2017-01-01" & transaction_product$tran_dt<="2017-12-31")]

#Removing Private and No lbael products
tr_2017=tr_2017[which(tr_2017$brand_desc!="NO LABEL")]
tr_2017=tr_2017[which(tr_2017$brand_desc!="PRIVATE LABEL")]
#Calculating total sales in 2017 for each customer and selecting only those
# with sales more than $5000. I call these people whales
customer_sales=tr_2017[,.(total_sale=sum(tran_prod_paid_amt)),by="cust_id"]
whales=customer_sales[(customer_sales$total_sale>5000),]

#Subsetting the transaction_product table by selecting only 'whales'
selected=transaction_product[transaction_product$cust_id %in% whales$cust_id]




all_customers=unique(selected$cust_id)
# creating a list of tables where each table corresponds to a customer
customer_profiles = lapply(seq_along(all_customers), function(i) selected[cust_id %in% all_customers[[i]]])

promo_table<-data.table()


promo_bag<-lapply(customer_profiles, function(x) {
  # 'x' refers to each single customer's transaction data  

  #Flagging products that are bought on promotion i.e non-zero discount 
  x<-x[,promo_flag:=ifelse(tran_prod_offer_cts>0,1,0)]
  #Vector with total sale value of each transaction
  sales_vec<-x[,.(sale_value=sum(tran_prod_paid_amt)),by="unique_id"]$sale_value
  # Vector with % discount on each sale
  promo_vec<-x[,.(disc_promo=sum(tran_prod_discount_amt)),by="unique_id"]$disc_promo
  ########################## Promotion Sensitivity ######################
  # Promotion sensitivity of a customer is defined as the correlation of (A,B):
  # A=Percentage discount obtained in each transaction (total_discount/total_sale)
  # B=Sale value of each transaction
  
  promo_sensitivity=cor(sales_vec,-1*promo_vec)
  promo_table<<-rbind(promo_table,cbind(x$cust_id[[2]],promo_sensitivity))
  
} )

write.csv(promo_table,'promo_sensitivity.csv')
