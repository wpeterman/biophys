# #' Calculate annual energy budget
# #'
# #'
# #'
# #'
# #'
# #Need to take average of consumption over a day (12 hrs of potential activity), This is the average of the hourly consumption calculations
# #Need to take average of active metabolic rate over a day (12 hrs of potential activity), This is the average of the hourly active metabolic rate calculations
# #Need to take average of PFT over a day as above
# #Need to calculate the total metabolic expenditure taking into account the active MR, inactive MR and PFT (see equation below)
# Mrate=(mean_mr_act*PFT)+(mr_inact*(24-PFT))
# #Need to calculate the total consumption taking into account PFT (see equation below)
# food=meanfood_sept*PFT_sept
# #Finally, need to calculate the monthly energy budget (EB)
# days=30 #number of days in the month
# EB_sept=(food-Mrate)*days
#
# #At the very end, we need to calculate the Annual Energy Budget (over two years), and subtract out the energy cost of a clutch of eggs
# repro=4.86 #energy (kJ) in 14 egg clutch (eggs 3.5 mm diameter)
#
# #Annual Discretionary Energy AFTER Reproduction
# annualEB=((EB_jan+EB_feb+EB_mar+EB_apr+EB_may+EB_jun+EB_jul+EB_aug+EB_sept+EB_oct+EB_nov+EB_dec)*2)-repro
