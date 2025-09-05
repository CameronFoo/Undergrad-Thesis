#### Price Volume regressions Event studies for DID and DDD###

######## Event study for Haven and district for property purchases (Price Volume) ######## 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase_Price <- feols(asinh(purchase_volume) ~ i(time_to_treat * RUSSIA) |
                             Country.Incorporated..1. + OCOD_DATE, 
                           data = stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)


# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase_Dis_Price <- feols(asinh(Purchase_Price_volume) ~ i(time_to_treat * RUSSIA * RUSSIAN_District) |
                                 Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                               data = District_stock_OCOD,
                               cluster = ~ Country.Incorporated..1.)

iplot(list(mod_twfe_purchase_Price = mod_twfe_purchase_Price, mod_twfe_purchase_Dis_Price = mod_twfe_purchase_Dis_Price), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(£purchase) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))



######## Event study for Haven and district for property sales (Price Volume) ######## 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold_Price <- feols(asinh(sale_volume) ~ i(time_to_treat * RUSSIA) |
                         Country.Incorporated..1. + OCOD_DATE, 
                       data = stock_OCOD,
                       cluster = ~ Country.Incorporated..1.)


# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold_Dis_Price <- feols(asinh(Sale_Price_volume) ~ i(time_to_treat * RUSSIA * RUSSIAN_District) |
                             Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                           data = District_stock_OCOD,
                           cluster = ~ Country.Incorporated..1.)

iplot(list(mod_twfe_sold_Price = mod_twfe_sold_Price, mod_twfe_sold_Dis_Price = mod_twfe_sold_Dis_Price), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(£sales) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))

#### Robustness #### 

# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase_Price_Rob <- feols(asinh(purchase_volume) ~ i(time_to_treat * RUSSIA_Robust) |
                                   Country.Incorporated..1. + OCOD_DATE, 
                                 data = stock_OCOD,
                                 cluster = ~ Country.Incorporated..1.)


# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_purchase_Dis_Price_Rob <- feols(asinh(Purchase_Price_volume) ~ i(time_to_treat * RUSSIA_Robust * RUSSIAN_District_Robust) |
                                       Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                                     data = District_stock_OCOD,
                                     cluster = ~ Country.Incorporated..1.)
                                     
iplot(list(mod_twfe_purchase_Price_Rob = mod_twfe_purchase_Price_Rob, mod_twfe_purchase_Dis_Price_Rob = mod_twfe_purchase_Dis_Price_Rob), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(£purchase) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))


# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold_Price_Rob <- feols(asinh(sale_volume) ~ i(time_to_treat * RUSSIA_Robust) |
                               Country.Incorporated..1. + OCOD_DATE, 
                             data = stock_OCOD,
                             cluster = ~ Country.Incorporated..1.)


# Fit the fixed-effects model with only the interaction term, clustering standard errors
mod_twfe_sold_Dis_Price_Rob <- feols(asinh(Sale_Price_volume) ~ i(time_to_treat * RUSSIA_Robust * RUSSIAN_District_Robust) |
                                   Country.Incorporated..1. + OCOD_DATE + RUSSIAN_District, 
                                 data = District_stock_OCOD,
                                 cluster = ~ Country.Incorporated..1.)

iplot(list(mod_twfe_sold_Price_Rob = mod_twfe_sold_Price_Rob, mod_twfe_sold_Dis_Price_Rob = mod_twfe_sold_Dis_Price_Rob), 
      sep = 0.5, 
      ref.line = -1, 
      ylab = 'ihs(£sales) and 95% Conf. Int.',
      xlab = 'Time to treatment', 
      main = '')
legend("topleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("DiD", "DDD"))