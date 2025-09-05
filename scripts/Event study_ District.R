
### Replication to my regression ### 
library(data.table)
library(fixest)

# Assuming 'OCOD_DATE' and 'reference_date' are character columns that need to be converted to Date objects
District_stock_OCOD$OCOD_DATE <- as.Date(District_stock_OCOD$OCOD_DATE)
reference_date <- as.Date("2022-02-01")

# Convert 'stock_OCOD' to a data.table if it's not already
District_stock_OCOD <- as.data.table(District_stock_OCOD)

# Calculate 'time_to_treat' based on the condition provided
District_stock_OCOD[, time_to_treat := as.numeric(difftime(OCOD_DATE, reference_date, units="days"))]

######## Event study for Haven and district for stock changes ######## 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_stock <- feols(asinh(properties_owned) ~ i(time_to_treat * RUSSIA) |
                             Country.Incorporated..1. + OCOD_DATE, 
                           data = stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_stock)

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_stock_Dis <- feols(asinh(properties_owned) ~ i(time_to_treat * RUSSIA * RUSSIAN_District) |
                                 Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                               data = District_stock_OCOD,
                               cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_stock_Dis)

iplot(list(mod_twfe_stock = mod_twfe_stock, mod_twfe_stock_Dis = mod_twfe_stock_Dis), 
      sep = 0.5, 
      ref.line = -1, 
      xlab = 'Time to treatment', 
      main = 'Properties Purchases Event study: Staggered treatment (TWFE)')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))


######## Event study for Haven and district for property purchases ######## 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase <- feols(asinh(properties_bought) ~ i(time_to_treat * RUSSIA) |
                             Country.Incorporated..1. + OCOD_DATE, 
                           data = stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_purchase)

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase_Dis <- feols(asinh(properties_bought) ~ i(time_to_treat * RUSSIA * RUSSIAN_District) |
                             Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                           data = District_stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_purchase_Dis)


iplot(list(mod_twfe_purchase = mod_twfe_purchase, mod_twfe_purchase_Dis = mod_twfe_purchase_Dis), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(purchase) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
abline(v = 180, col = "darkblue", lty = 2)
text(x = 180, y = par("usr")[3], labels = "Live Registry", pos = 3, col = "darkblue", cex = 0.8)
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))


######## Event study for Haven and district for property sales ######## 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold <- feols(asinh(properties_sold) ~ i(time_to_treat * RUSSIA) |
                             Country.Incorporated..1. + OCOD_DATE, 
                           data = stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_sold)

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold_Dis <- feols(asinh(properties_sold) ~ i(time_to_treat * RUSSIA * RUSSIAN_District) |
                                 Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                               data = District_stock_OCOD,
                               cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_sold_Dis)

iplot(list(mod_twfe_sold = mod_twfe_sold, mod_twfe_sold_Dis = mod_twfe_sold_Dis), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(sales) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
abline(v = 180, col = "darkblue", lty = 2)
text(x = 180, y = par("usr")[3], labels = "Live Registry", pos = 3, col = "darkblue", cex = 0.8)
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))

            

    ### Event study Robustness ### 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase <- feols(asinh(properties_bought) ~ i(time_to_treat * RUSSIA_Robust) |
                             Country.Incorporated..1. + OCOD_DATE, 
                           data = stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_purchase)

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase_Dis <- feols(asinh(properties_bought) ~ i(time_to_treat * RUSSIA_Robust * RUSSIAN_District_Robust) |
                                 Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District_Robust, 
                               data = District_stock_OCOD,
                               cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_purchase_Dis)

iplot(list(mod_twfe_purchase = mod_twfe_purchase, mod_twfe_purchase_Dis = mod_twfe_purchase_Dis), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(purchase) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))



# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold <- feols(asinh(properties_sold) ~ i(time_to_treat * RUSSIA_Robust) |
                         Country.Incorporated..1. + OCOD_DATE, 
                       data = stock_OCOD,
                       cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_sold)

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold_Dis <- feols(asinh(properties_sold) ~ i(time_to_treat * RUSSIA_Robust * RUSSIAN_District_Robust) |
                             Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                           data = District_stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_sold_Dis)

iplot(list(mod_twfe_sold = mod_twfe_sold, mod_twfe_sold_Dis = mod_twfe_sold_Dis), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(sales) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))