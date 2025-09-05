
### Replication to my regression ### 
install.packages("data.table")
library(data.table)
library(fixest)

# Assuming 'OCOD_DATE' and 'reference_date' are character columns that need to be converted to Date objects
stock_OCOD$OCOD_DATE <- as.Date(stock_OCOD$OCOD_DATE)
reference_date <- as.Date("2022-02-01")

# Convert 'stock_OCOD' to a data.table if it's not already
stock_OCOD <- as.data.table(stock_OCOD)

# Calculate 'time_to_treat' based on the condition provided
# Assuming stock_OCOD is your data.table and you want to calculate time_to_treat column
reference_date <- as.Date("2022-02-01")
stock_OCOD[, time_to_treat := as.numeric(difftime(OCOD_DATE, reference_date, units = "days"))]

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase <- feols(properties_bought ~ i(time_to_treat * RUSSIA) |
                    Country.Incorporated..1. + OCOD_DATE, 
                  data = stock_OCOD,
                  cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_purchase)

iplot(mod_twfe_purchase, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

mod_twfe_sale <- feols(properties_sold ~ i(time_to_treat * RUSSIA) |
                    Country.Incorporated..1. + OCOD_DATE, 
                  data = stock_OCOD,
                  cluster = ~ Country.Incorporated..1.)

summary(mod_twfe_sale)
iplot(mod_twfe_sale, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

library(fixest)

# Presuming mod_twfe_purchase and mod_twfe_sale are already fitted models
# Plot the results using iplot
iplot(list(mod_twfe_purchase = mod_twfe_purchase, mod_twfe_sale = mod_twfe_sale), 
      sep = 0.5, 
      ref.line = -1, 
      xlab = 'Time to treatment', 
      main = 'Properties Purchases/Sold Event study: Staggered treatment (TWFE)')
      legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
             legend = c("Property Purchase", "Property Sold"))
      
      
#### Event Study For each of the treatment groups #### 
      
      ### Havens ###
      mod_twfe_purchase_Haven <- feols(properties_bought ~ i(time_to_treat * Haven_dummy) |
                                   Country.Incorporated..1. + OCOD_DATE, 
                                 data = stock_OCOD,
                                 cluster = ~ Country.Incorporated..1.)

      mod_twfe_sale_Haven <- feols(properties_sold ~ i(time_to_treat * Haven_dummy) |
                               Country.Incorporated..1. + OCOD_DATE, 
                             data = stock_OCOD,
                             cluster = ~ Country.Incorporated..1.)
      
      iplot(list(mod_twfe_purchase_Haven = mod_twfe_purchase_Haven, mod_twfe_sale_Haven = mod_twfe_sale_Haven), 
            sep = 0.5, 
            ref.line = -1, 
            ylab = 'count(purchase & sale) and 95% Conf. Int.',
            xlab = 'Time to treatment', 
            main = '')
      abline(v = 180, col = "darkblue", lty = 2)
      text(x = 180, y = par("usr")[3], labels = "Live Registry", pos = 3, col = "darkblue", cex = 0.8)
      legend("topleft", col = c(1, 2), pch = c(20, 17), 
             legend = c("Purchase (DiD)", "Sold (DiD)"))      
      
      ### Havens Price volume ###
      mod_twfe_purchase_Haven <- feols(asinh(purchase_volume) ~ i(time_to_treat * Haven_dummy) |
                                         Country.Incorporated..1. + OCOD_DATE, 
                                       data = stock_OCOD,
                                       cluster = ~ Country.Incorporated..1.)

      mod_twfe_sale_Haven <- feols(asinh(sale_volume) ~ i(time_to_treat * Haven_dummy) |
                                     Country.Incorporated..1. + OCOD_DATE, 
                                   data = stock_OCOD,
                                   cluster = ~ Country.Incorporated..1.)
      
      iplot(list(mod_twfe_purchase_Haven = mod_twfe_purchase_Haven, mod_twfe_sale_Haven = mod_twfe_sale_Haven), 
            sep = 0.5, 
            ref.line = -1, 
            ylab = 'ihs(£purchase & sale) and 95% Conf. Int.',
            xlab = 'Time to treatment', 
            main = '')
      legend("topleft", col = c(1, 2), pch = c(20, 17), 
             legend = c("Purchase (DiD)", "Sold (DiD)")) 
      
      ### CPI ###
    ## The changes in count ### 
      mod_twfe_purchase_CPI <- feols(properties_bought ~ i(time_to_treat * CPI_dummy) |
                                         Country.Incorporated..1. + OCOD_DATE, 
                                       data = stock_OCOD,
                                       cluster = ~ Country.Incorporated..1.)
      
      summary(mod_twfe_purchase_CPI)
      
      mod_twfe_sale_CPI <- feols(properties_sold ~ i(time_to_treat * CPI_dummy) |
                                     Country.Incorporated..1. + OCOD_DATE, 
                                   data = stock_OCOD,
                                   cluster = ~ Country.Incorporated..1.)
      
      iplot(list(mod_twfe_purchase_CPI = mod_twfe_purchase_CPI, mod_twfe_sale_CPI = mod_twfe_sale_CPI), 
            sep = 0.5, 
            ref.line = -1, 
            ylab = 'count(purchase & sale) and 95% Conf. Int.',
            xlab = 'Time to treatment', 
            main = '')
      abline(v = 180, col = "darkblue", lty = 2)
      text(x = 180, y = par("usr")[3], labels = "Live Registry", pos = 3, col = "darkblue", cex = 0.8)
      legend("topleft", col = c(1, 2), pch = c(20, 17), 
             legend = c("Purchase (DiD)", "Sold (DiD)"))      
      
      ### Changes in price volume ###
      mod_twfe_purchase_CPI <- feols(asinh(purchase_volume) ~ i(time_to_treat * CPI_dummy) |
                                       Country.Incorporated..1. + OCOD_DATE, 
                                     data = stock_OCOD,
                                     cluster = ~ Country.Incorporated..1.)
      
  
      mod_twfe_sale_CPI <- feols(asinh(sale_volume) ~ i(time_to_treat * CPI_dummy) |
                                   Country.Incorporated..1. + OCOD_DATE, 
                                 data = stock_OCOD,
                                 cluster = ~ Country.Incorporated..1.)

            iplot(list(mod_twfe_purchase_CPI = mod_twfe_purchase_CPI, mod_twfe_sale_CPI = mod_twfe_sale_CPI), 
            sep = 0.5, 
            ref.line = -1, 
            ylab = 'log (purchase & sale) and 95% Conf. Int.',
            xlab = 'Time to treatment', 
            main = '')
      legend("topleft", col = c(1, 2), pch = c(20, 17), 
             legend = c("Purchase (DiD)", "Sold (DiD)"))     
      
      ### AEOI ###
      
      mod_twfe_purchase_AEOI <- feols(properties_bought ~ i(time_to_treat * AEOI_signatories_dummy) |
                                       Country.Incorporated..1. + OCOD_DATE, 
                                     data = stock_OCOD,
                                     cluster = ~ Country.Incorporated..1.)
      
      mod_twfe_sale_AEOI <- feols(properties_sold ~ i(time_to_treat * AEOI_signatories_dummy) |
                                   Country.Incorporated..1. + OCOD_DATE, 
                                 data = stock_OCOD,
                                 cluster = ~ Country.Incorporated..1.)
      
      iplot(list(mod_twfe_purchase_AEOI = mod_twfe_purchase_AEOI, mod_twfe_sale_AEOI = mod_twfe_sale_AEOI), 
            sep = 0.5, 
            ref.line = -1, 
            ylab = 'count (purchase & sale) and 95% Conf. Int.',
            xlab = 'Time to treatment', 
            main = '')
      abline(v = 180, col = "darkblue", lty = 2)
      text(x = 180, y = par("usr")[3], labels = "Live Registry", pos = 3, col = "darkblue", cex = 0.8)
      legend("topleft", col = c(1, 2), pch = c(20, 17), 
             legend = c("Purchase (DiD)", "Sold (DiD)"))      
      