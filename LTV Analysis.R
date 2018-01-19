# Loading libraries for LTV model building
library("dplyr")
library("BTYD")
library("plyr")
library("ggplot2")
library("reshape2")
library("BTYDplus")
library("sqldf")

#Dividing dataset for Android and iOS devices. Model has been built separately for different OS types
Game_Data_Set$event_date = as.Date(Game_Data_Set$event_time_utc)
Game_Data_Android = filter(Game_Data_Set,client_os_type=="android")
Game_Data_iOS = filter(Game_Data_Set,client_os_type=="ios")

#--------------Android OS type------------------------------------------------------------------------

Transaction_date_android = Game_Data_Android[,c(8,12,11)]
names(Transaction_date_android) = c("cust","date","sales")
Transaction_date = dc.MergeTransactionsOnSameDate(Transaction_date_android)

#Divide the data into calibration and holdout period for building the model (Calibration) and testing (Holdout)

(end.of.cal.period =
    min(Transaction_date$date)+as.numeric((max(Transaction_date$date)-min(Transaction_date$date))/2))

#Conversion of game data to gamer level for estimation of model parameter
#Attributes calculated - Frequency of transaction, Recency, time observed etc.

CustomerMatrix <- elog2cbs(Transaction_date, T.cal = end.of.cal.period, T.tot = max(Transaction_date$date))
#head(CustomerMatrix)

#Estimating Paretto/NBD model parameters
params = pnbd.EstimateParameters(CustomerMatrix)

# Predict transactions at gamer level with NBD model.
CustomerMatrix$xstar.nbd <- pnbd.ConditionalExpectedTransactions(
  params = params,
  T.star = CustomerMatrix$T.star,
  x      = CustomerMatrix$x,
  t.x    = CustomerMatrix$t.x,
  T.cal  = CustomerMatrix$T.cal)


# Main model params are :

# r          gamma parameter for NBD transaction 
# alpha      gamma parameter for NBD transaction 
# s          gamma parameter for Pareto (exponential gamma) dropout process
# beta       gammma parameter for Pareto (exponential gamma) dropout process


############################################
#
# PROBABILITY A Gamer IS ALIVE AT END OF CALIBRATION / TRAINING
#
############################################

# To visualize the distribution of P(Alive) across Gamers:
p.alives = pnbd.PAlive(params, CustomerMatrix$x, CustomerMatrix$t.x, CustomerMatrix$T.cal)

ggplot(as.data.frame(p.alives),aes(x=p.alives))+
  geom_histogram(colour="grey",fill="orange")+
  ylab("Number of Gamers")+
  xlab("Probability Gamer is 'Live'")+
  ggtitle("Distribution for Android OS")+
  theme_minimal()

# plot actual & expected Gamers binned by num of repeat transactions
pnbd.PlotFrequencyInCalibration(params, CustomerMatrix, 
                                censor=10, title="Model vs. Reality during Calibration")

############################################
#
# STATISTICAL MEASURE OF ACCURACY
#
############################################

colnames(CustomerMatrix) <- gsub("\\.","_",colnames(CustomerMatrix))
# root mean squared error
rmse = function(est, act) { return(sqrt(mean((est-act)^2))) }

# mean squared logarithmic error
msle = function(est, act) { return(mean((log1p(est)-log1p(act))^2)) }

rmse(act=CustomerMatrix$x_star,est=CustomerMatrix$xstar_nbd)
msle(act=CustomerMatrix$x_star,est=CustomerMatrix$xstar_nbd)

#-------------------Estimated Transaction Revenue---------------------------------

# calculate average spends per transaction
CustomerMatrix$sales_avg <- CustomerMatrix$sales / (CustomerMatrix$x + 1)

# Estimate expected average transaction value based on gamma-gamma spend model
spend.params <- BTYD::spend.EstimateParameters(CustomerMatrix$sales_avg, 
                                               CustomerMatrix$x + 1)
CustomerMatrix$sales_gamma_gamma_est <- BTYD::spend.expected.value(spend.params, 
                                                           CustomerMatrix$sales_avg, 
                                                           CustomerMatrix$x + 1)

CustomerMatrix$LTV_nbd_gamma_est=CustomerMatrix$xstar_nbd*CustomerMatrix$sales_gamma_gamma_est

# Estimate expected average transaction value based on linear regression

Customer_dataset = sqldf("select a.cust, a.x+1 as historic_transactions, a.t_x as recency,
                          a.sales_avg as historic_sales, b.max_days_retained_cal, 
                          b.avg_progression_level_cal, b.avg_session_bet_purchase,
                          a.x/b.max_days_retained_cal as activity_level, a.sales_star/a.x_star as future_avg_transaction 
                         from CustomerMatrix a left join 
                         (select player_id, max(days_retained) as max_days_retained_cal,
                         avg(progression_level) as avg_progression_level_cal,
                         (max(num_sessions)-min(num_sessions))/count(num_purchases) as avg_session_bet_purchase
                         from Game_Data_Android
                         where strftime('%Y-%m-%d', event_date * 3600 * 24, 'unixepoch') <='2017-09-16' group by player_id) b on a.cust = b.player_id")

Customer_dataset$activity_level[is.na(Customer_dataset$activity_level)] = 0
Customer_dataset$future_avg_transaction[is.na(Customer_dataset$future_avg_transaction)] = 0

model1= step(lm(future_avg_transaction~historic_sales+historic_transactions+recency+
                  activity_level+avg_progression_level_cal+avg_session_bet_purchase,
                data = Customer_dataset),direction="backward")
summary(model1)

CustomerMatrix$sales_linear_reg_est = model1$fitted.values
#CustomerMatrix[CustomerMatrix$sales_linear_reg_est<0,]$sales_linear_reg_est=0
CustomerMatrix$LTV_nbd_lm_est=CustomerMatrix$xstar_nbd*CustomerMatrix$sales_linear_reg_est

#LM and gamma gamma distribution model comparison using statistical techniques

#root mean square error and mean squared logarithmic error for nbd_gamma LTV
rmse(act=CustomerMatrix$sales_star,est=CustomerMatrix$LTV_nbd_gamma_est)
msle(act=CustomerMatrix$sales_star,est=CustomerMatrix$LTV_nbd_gamma_est)

#root mean square error and mean squared logarithmic error for nbd_linear regression LTV
rmse(act=CustomerMatrix$sales_star,est=CustomerMatrix$LTV_nbd_lm_est)
msle(act=CustomerMatrix$sales_star,est=CustomerMatrix$LTV_nbd_lm_est)

# Final table for Actual and forecasted result 

##NBD model - Actual and forecasted number of transactions

NBD_result=setNames(data.frame(CustomerMatrix$cust,CustomerMatrix$x,CustomerMatrix$x_star),c("cust","actual","predicted"))
NBD_result$class=""
NBD_result[NBD_result$actual>=14,]$class=">=14"
NBD_result[NBD_result$actual<14 & NBD_result$actual>=8,]$class="8 to 14"
NBD_result[NBD_result$actual<8 & NBD_result$actual>=3,]$class="3 to 8"
NBD_result[NBD_result$actual<3,]$class="<3"

mean_NBDresult_android = aggregate(actual~class, NBD_result, FUN=mean)
mean_NBDresult_android$mean_model1 = aggregate(predicted~class, NBD_result, FUN=mean)[,2]

## average transaction revenue - Actual and forecasted results

Avg_trans_rev_table=setNames(data.frame(CustomerMatrix$cust,(CustomerMatrix$sales_star/CustomerMatrix$x_star),
                                        CustomerMatrix$sales_gamma_gamma_est,CustomerMatrix$sales_linear_reg_est),
                                        c("cust","actual","predicted_gamma","predicted_lm"))
Avg_trans_rev_table[Avg_trans_rev_table$actual=="NaN",]$actual = 0
Avg_trans_rev_table$class=""
Avg_trans_rev_table[Avg_trans_rev_table$actual>=50,]$class=">=50"
Avg_trans_rev_table[Avg_trans_rev_table$actual<50 & Avg_trans_rev_table$actual>=20,]$class="20 to 50"
Avg_trans_rev_table[Avg_trans_rev_table$actual<20 & Avg_trans_rev_table$actual>=8,]$class="8 to 20"
Avg_trans_rev_table[Avg_trans_rev_table$actual<8,]$class="<8"

mean_avg_trans_android = aggregate(actual~class, Avg_trans_rev_table, FUN=mean)
mean_avg_trans_android$mean_gamma = aggregate(predicted_gamma~class, Avg_trans_rev_table, FUN=mean)[,2]
mean_avg_trans_android$mean_lm = aggregate(predicted_lm~class, Avg_trans_rev_table, FUN=mean)[,2]

# LVT - Actual and forecasted result

CustomerMatrix$Segment=""
CustomerMatrix[CustomerMatrix$sales_star>=300,]$Segment=">=300"
CustomerMatrix[CustomerMatrix$sales_star<300 & CustomerMatrix$sales_star>=80,]$Segment="80 to 300"
CustomerMatrix[CustomerMatrix$sales_star<80 & CustomerMatrix$sales_star>=20,]$Segment="20 to 80"
CustomerMatrix[CustomerMatrix$sales_star<20,]$Segment="<20"

mean_table_android = aggregate(sales_star~Segment, CustomerMatrix, FUN=mean)
mean_table_android$mean_gamma = aggregate(LTV_nbd_gamma_est~Segment, CustomerMatrix, FUN=mean)[,2]
mean_table_android$mean_lm = aggregate(LTV_nbd_lm_est~Segment, CustomerMatrix, FUN=mean)[,2]

