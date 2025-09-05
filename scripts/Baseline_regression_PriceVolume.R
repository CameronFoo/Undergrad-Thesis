#### Regression Scripts for Price volume ###
### Total Price Volume ###

Russian_reg_total_price_volume <- plm(asinh(price_volume) ~ RUSSIA * POST, 
                         data = stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                         model = "within",
                         effect = "twoways")

summary(Russian_reg_total_price_volume)


# Define the triple difference-in-differences panel data model
triple_diff_model_1_Price <- plm(asinh(price_volume_district) ~ RUSSIA * POST * RUSSIAN_District, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")

# Obtain and print the summary of the triple difference-in-differences model
summary(triple_diff_model_1_Price)

 ### Purchase price volume ###

Russian_reg_purchase_price_volume <- plm(asinh(purchase_volume) ~ RUSSIA * POST, 
                                data = stock_OCOD, 
                                index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                model = "within",
                                effect = "twoways") 
summary(Russian_reg_purchase_price_volume)

triple_diff_model_2_Price <- plm(asinh(Purchase_Price_volume) ~ RUSSIA * POST * RUSSIAN_District, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")

summary(triple_diff_model_2_Price)

 ### Sale price volume ###

Russian_reg_sale_price_volume <- plm(asinh(sale_volume) ~ RUSSIA * POST, 
                        data = stock_OCOD, 
                        index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                        model = "within",
                        effect = "twoways")
summary(Russian_reg_sale_price_volume)

triple_diff_model_3_Price <- plm(asinh(Sale_Price_volume) ~ RUSSIA * POST * RUSSIAN_District, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")

summary(triple_diff_model_3_Price)

stargazer(triple_diff_model_3_Price)


#### Price volume for treatment groups #### 
##### Haven #####
Haven_reg_total_price_volume <- plm(asinh(price_volume) ~ Haven_dummy * POST, 
                                      data = stock_OCOD, 
                                      index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                      model = "within",
                                      effect = "twoways")

summary(Haven_reg_total_price_volume)

Haven_reg_purchase_price_volume <- plm(asinh(purchase_volume) ~ Haven_dummy * POST, 
                                         data = stock_OCOD, 
                                         index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                         model = "within",
                                         effect = "twoways") 
summary(Haven_reg_purchase_price_volume)

Haven_reg_sale_price_volume <- plm(asinh(sale_volume) ~ Haven_dummy * POST, 
                                     data = stock_OCOD, 
                                     index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                     model = "within",
                                     effect = "twoways")
summary(Haven_reg_sale_price_volume)

stargazer(Haven_reg_sale_price_volume)

#### CPI #####

CPI_reg_total_price_volume <- plm(asinh(price_volume) ~ CPI_dummy * POST, 
                                    data = stock_OCOD, 
                                    index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                    model = "within",
                                    effect = "twoways")

summary(CPI_reg_total_price_volume)

CPI_reg_purchase_price_volume <- plm(asinh(purchase_volume) ~ CPI_dummy * POST, 
                                       data = stock_OCOD, 
                                       index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                       model = "within",
                                       effect = "twoways") 
summary(CPI_reg_purchase_price_volume)

CPI_reg_sale_price_volume <- plm(asinh(sale_volume) ~ CPI_dummy * POST, 
                                   data = stock_OCOD, 
                                   index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                   model = "within",
                                   effect = "twoways")
summary(CPI_reg_sale_price_volume)
stargazer(CPI_reg_sale_price_volume)


##### AEOI #####

AEOI_reg_total_price_volume <- plm(asinh(price_volume) ~ AEOI_signatories_dummy * POST, 
                                  data = stock_OCOD, 
                                  index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                  model = "within",
                                  effect = "twoways")

summary(AEOI_reg_total_price_volume)

AEOI_reg_purchase_price_volume <- plm(asinh(purchase_volume) ~ AEOI_signatories_dummy * POST, 
                                     data = stock_OCOD, 
                                     index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                     model = "within",
                                     effect = "twoways") 
summary(AEOI_reg_purchase_price_volume)

AEOI_reg_sale_price_volume <- plm(asinh(sale_volume) ~ AEOI_signatories_dummy * POST, 
                                 data = stock_OCOD, 
                                 index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                 model = "within",
                                 effect = "twoways")
summary(AEOI_reg_sale_price_volume)

stargazer(AEOI_reg_sale_price_volume)

####Treatment robust ### 
CPI_reg_total_price_volume_Robust <- plm(asinh(price_volume) ~ CPI_dummy_Rob * POST, 
                                  data = stock_OCOD, 
                                  index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                  model = "within",
                                  effect = "twoways")

summary(CPI_reg_total_price_volume)

CPI_reg_purchase_price_volume_Robust <- plm(asinh(purchase_volume) ~ CPI_dummy_Rob * POST, 
                                     data = stock_OCOD, 
                                     index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                     model = "within",
                                     effect = "twoways") 
summary(CPI_reg_purchase_price_volume)

CPI_reg_sale_price_volume_Robust <- plm(asinh(sale_volume) ~ CPI_dummy_Rob * POST, 
                                 data = stock_OCOD, 
                                 index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                                 model = "within",
                                 effect = "twoways")
summary(CPI_reg_sale_price_volume_Robust)

stargazer(CPI_reg_total_price_volume_Robust, CPI_reg_purchase_price_volume_Robust)
