#Setting working directory
setwd("D:/RWD")

#Importing the input file
library(readxl)
pmnt_ops_tst <- read_excel("D:/RWD/pmnt_ops_tst.xlsx")
pmnt_ops_tst$OrderAmount <- ceiling(pmnt_ops_tst$OrderAmount)


#Assigning delivery boys
pmnt_ops_tst$del_boy_nm <- rep(1:25 , length.out = 991)


#Generating random numbers
pmnt_ops_tst$rand_nm <- runif(991)


#creating the probability table
pmnt_ops_tst_prob_tbl <-
  data.frame(
    "change" = c(1000, 500, 100, 50, 20, 10),
    "chance" = c(0.02, 0.4, 0.3, 0.2, 0.05, 0.03)
  )
pmnt_ops_tst_prob_tbl$cum_chnc <-
  cumsum(pmnt_ops_tst_prob_tbl$chance)


#Getting the least denomination
library(sqldf)

pmnt_ops_tst$mc_lst_denom <-
  unlist(
    sqldf(
      "select case when rand_nm < 0.02 then 1000
      when rand_nm < 0.42 then 500
      when rand_nm < 0.72 then 100
      when rand_nm < 0.92 then 50
      when rand_nm < 0.97 then 20
      when rand_nm < 1 then 10 end
      from pmnt_ops_tst"
    )
    )

#Check
#sqldf("select mc_lst_denom,count(*)/991.00 from pmnt_ops_tst group by mc_lst_denom")

#Calculating how much change needs to be tendered
pmnt_ops_tst$change <-
  unlist(
    sqldf(
      "select case when OrderAmount%mc_lst_denom = 0 then 0
      when OrderAmount < mc_lst_denom then mc_lst_denom-OrderAmount
      when mc_lst_denom = 20 then 20-(OrderAmount%50)%20
      else mc_lst_denom-(OrderAmount%mc_lst_denom) end
      from pmnt_ops_tst"
      )
      )

#Rounding the change to the closest multiple of 10
library(plyr)
pmnt_ops_tst$change <-
  round_any(pmnt_ops_tst$change, 10, f = ceiling)

#Calculating denomination for each change value

pmnt_ops_tst$denom_500 <-
  as.integer(unlist(
    sqldf(
      "select case when (change >= 500) then (change/500) else 0 end from pmnt_ops_tst"
    )
  ))
pmnt_ops_tst$denom_100 <-
  as.integer(unlist(
    sqldf(
      "select case when (change%500 >= 100) then ((change%500)/100) else 0 end from pmnt_ops_tst"
    )
  ))
pmnt_ops_tst$denom_50 <-
  as.integer(unlist(
    sqldf(
      "select case when (change%100 >= 50) then ((change%100)/50) else 0 end from pmnt_ops_tst"
    )
  ))
pmnt_ops_tst$denom_20 <-
  as.integer(unlist(
    sqldf(
      "select case when (change%50 >= 20) then ((change%50)/20) else 0 end from pmnt_ops_tst"
    )
  ))
pmnt_ops_tst$denom_10 <-
  as.integer(unlist(
    sqldf(
      "select case when ((change%50)%20 >= 10) then (((change%50)%20)/10) else 0 end from pmnt_ops_tst"
    )
  ))


#Taking a sum of denomination at a delivery boy level

pmnt_ops_tst_sol <- sqldf(
  "select del_boy_nm,
  sum(denom_500) as sum_denom_500,
  sum(denom_100) as sum_denom_100,
  sum(denom_50) as sum_denom_50,
  sum(denom_20) as sum_denom_20,
  sum(denom_10) as sum_denom_10
  from pmnt_ops_tst
  group by 1 "
)

#taking max to get one solution
pmnt_ops_tst_sol_fin <-
  sqldf(
    "select max(sum_denom_500) as max_denom_500,
    max(sum_denom_100) as max_denom_100,
    max(sum_denom_50) as max_denom_50,
    max(sum_denom_20) as max_denom_20,
    max(sum_denom_10) as max_denom_10
    from pmnt_ops_tst_sol"
  )



#running it multiple times to see what kind of results we get
#pmnt_ops_tst_sol_mul <- pmnt_ops_tst_sol_fin
pmnt_ops_tst_sol_mul <-
  rbind(pmnt_ops_tst_sol_mul, pmnt_ops_tst_sol_fin)

write.csv(pmnt_ops_tst_sol_mul, 'pmnt_ops_tst_sol_mul.csv')

