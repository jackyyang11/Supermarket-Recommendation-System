# Final product recommandation selection -----
# Process: Looking at unique brands, find out at most 5 brands whose products can cover over 500 customers

# Read list of unique products exist in the shortlists for customers -----
customer_product=fread("Customer_top5products.csv",header=T)

# Remove unused columns and Fruit&Vegetables -----
customer_product$V1 = NULL
customer_product$`Unnamed: 0` = NULL
setnames(customer_product,old="product",new="prod_id")
customer_product = merge(customer_product,product[,c("prod_id","brand_desc")],by="prod_id")
customer_product = customer_product[brand_desc!="FRUTAS&VEGETAIS",]
final_product = unique(customer_product[,prod_id])
final_brand=unique(customer_product[,brand_desc])

# Rank brands by product frequency -----
final_product_brand_by_count = customer_product[,.N,by="brand_desc"]
final_product_brand_by_count=final_product_brand[order(-N),]
# Rank brands by average expected value 
# Used in future evaluation but generate way lower total expected value
final_product_brand_by_EV = customer_product[,lapply(.SD,mean),by="brand_desc",.SDcol="ExpectedValue"]
final_product_brand_by_EV = final_product_brand_by_EV[order(-ExpectedValue),]

# Build function to find out how many customer these brand have covered -----
# and only leave those who have >=2 products in his recommandation product list
# b is a one column dataframe, which is brand_desc with top product count, cust is the customer imput
brand_coverage=function(b,cust){
  brand_coverage = merge(b,cust,by="brand_desc")
  customer_count = brand_coverage[,.N,by="customer"]
  customer_count = customer_count[N>1,]
  brand_coverage = merge(customer_count,brand_coverage,by = "customer")
  brand_coverage[is.na(brand_desc)]=NULL
  customer_coverage = nrow(unique(brand_coverage[,"customer"]))
  result = c(customer_coverage)
  return(result)
}

# Loop along the brand list by frequency, we look at most 5 brands -----

for (i in 1:5){
  brand_list = final_product_brand_by_count[1:i,]
  result = brand_coverage(brand_list,customer_product)
  print(result)
}

# Use the previous result, get the customer list. -----
# At this moment, every customer has >=2 products promoted to him.
# Looking at top brands, there are two brands in oil category. 
# To make the most effective promotion, we leave the one with higher expected value, which is FULA
# To shortlist the number of promoted products into 2, we keep the two products that have the highest expected value
brand_list = final_product_brand[c(1,2,4,5),]
brand_coverage = merge(brand_list,customer_product,by="brand_desc")
customer_count = brand_coverage[,.N,by="customer"]
customer_count = customer_count[N>1,]
brand_coverage = merge(customer_count,brand_coverage,by = "customer")
final_customers = brand_coverage[,.SD[order(-ExpectedValue),][1:2],by="customer"]
final_customers[,sum(ExpectedValue)]
length(unique(final_customers[,customer]))
final_product_list = as.data.frame(unique(final_customers[,prod_id]))
setnames(final_product_list,old="unique(final_customers[, prod_id])",new = "prod_id") 
final_product_list = merge(final_product_list,product[,c("prod_id","brand_desc","category_desc_eng")],by="prod_id")

# Generate final table of target customer and promoted products -----
# write to recommandation list
final_customers = final_customers[,c("customer","prod_id","brand_desc","ExpectedValue")]
final_customers = merge(final_customers,product[,c("prod_id","category_desc_eng")],by="prod_id")
setkey(final_customers,"customer") 
write.csv(final_customers,"Recommandation_List.csv")