## Regression Script ###

Combined_OCOD <- read.csv("/Users/cameronfoo/Desktop/Diss data_export/Combined_OCOD.csv")
library(dplyr)
library(tidyr)


stock_OCOD <- Combined_OCOD %>%
  group_by(Country.Incorporated..1., OCOD_DATE) %>%
  summarise(
    price_volume = sum(Price.Paid, na.rm = TRUE),  # Calculate the price volume at the district level
    properties_owned = n_distinct(Title.Number)  # Calculate the number of properties owned in each district
  ) %>%
  ungroup()

stock_OCOD <- stock_OCOD %>%
  arrange(Country.Incorporated..1., OCOD_DATE) %>%
  group_by(Country.Incorporated..1.) %>%
  mutate(
    change_in_price_volume_district = c(NA, diff(price_volume)),  # Calculate the change in price volume at the district level
    change_in_properties_owned = c(0, diff(properties_owned)), 
    properties_bought = ifelse(change_in_properties_owned > 0, change_in_properties_owned, 0),
    properties_sold = ifelse(change_in_properties_owned < 0, abs(change_in_properties_owned), 0),
    purchase_volume = ifelse(change_in_price_volume_district > 0, change_in_price_volume_district, 0),  # Volume related to purchases in the district
    sale_volume = ifelse(change_in_price_volume_district < 0, abs(change_in_price_volume_district), 0)  # Volume related to sales in the district
  ) %>%
  ungroup()

## Properties owned Overseas jurisdiction in each London district ###

# Calculate the price and volume information for each district within a jurisdiction and date
District_stock_OCOD <- Combined_OCOD %>%
  group_by(Country.Incorporated..1., District, OCOD_DATE) %>%
  summarise(
    price_volume_district = sum(Price.Paid, na.rm = TRUE),  # Calculate the price volume at the district level
    properties_owned = n_distinct(Title.Number)  # Calculate the number of properties owned in each district
  ) %>%
  ungroup()

# Calculate the change in price volume and properties owned at the district level and determine purchases or sales
District_stock_OCOD <- District_stock_OCOD %>%
  arrange(Country.Incorporated..1., District, OCOD_DATE) %>%
  group_by(Country.Incorporated..1., District) %>%
  mutate(
    change_in_price_volume_district = c(NA, diff(price_volume_district)),  # Calculate the change in price volume at the district level
    change_in_properties_owned = c(0, diff(properties_owned)), 
    properties_bought = ifelse(change_in_properties_owned > 0, change_in_properties_owned, 0),
    properties_sold = ifelse(change_in_properties_owned < 0, abs(change_in_properties_owned), 0),
    Purchase_Price_volume = ifelse(change_in_price_volume_district > 0, change_in_price_volume_district, 0),  # Volume related to purchases in the district
    Sale_Price_volume = ifelse(change_in_price_volume_district < 0, abs(change_in_price_volume_district), 0)  # Volume related to sales in the district
  ) %>%
  ungroup()


# Create the dummy variables
RUSSIA<- c("BAHAMAS", "BELIZE", "CYPRUS", "GIBRALTAR", "HONG KONG", "JERSEY", "MAURITIUS", "BRITISH VIRGIN ISLANDS", "SEYCHELLES","LABUAN") ## Havens that are identified in the literature
RUSSIA_Robust <- c("GIBRALTAR", "CYPRUS", "BAHAMAS")

RUSSIAN_District <- c("CITY OF WESTMINSTER", "LAMBETH","BRENT", "CAMDEN","KENSINGTON AND CHELSEA", "SOUTHWARK", "CROYDON")
RUSSIAN_District_Robust <- c("CITY OF WESTMINSTER", "KENSINGTON AND CHELSEA", "ENFIELD", "RICHMOND UPON THAMES")


# Create dummy columns
stock_OCOD$RUSSIA <- as.numeric(stock_OCOD$Country.Incorporated..1. %in% RUSSIA)
stock_OCOD$RUSSIA_Robust <- as.numeric(stock_OCOD$Country.Incorporated..1. %in% RUSSIA_Robust)
stock_OCOD$POST <- as.numeric(as.Date(stock_OCOD$OCOD_DATE) > as.Date("2022-02-01"))

District_stock_OCOD$RUSSIA <- as.numeric(District_stock_OCOD$Country.Incorporated..1. %in% RUSSIA)
District_stock_OCOD$POST <- as.numeric(as.Date(District_stock_OCOD$OCOD_DATE) > as.Date("2022-02-01"))
District_stock_OCOD$RUSSIAN_District <- as.numeric(District_stock_OCOD$District %in% RUSSIAN_District)
District_stock_OCOD$RUSSIA_Robust <- as.numeric(District_stock_OCOD$Country.Incorporated..1. %in% RUSSIA_Robust)
District_stock_OCOD$RUSSIAN_District_Robust <- as.numeric(District_stock_OCOD$District %in% RUSSIAN_District_Robust)

library(plm)
library(stargazer) 

### Regression for Russian Stock ###
Russian_reg_stock <- plm(asinh(properties_owned) ~ RUSSIA * POST, 
                         data = stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                         model = "within",
                         effect = "twoways")

summary(Russian_reg_stock)


# Define the triple difference-in-differences panel data model
triple_diff_model_1 <- plm(asinh(properties_owned) ~ RUSSIA * POST * RUSSIAN_District, 
                         data = District_stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                         model = "within",
                         effect = "twoways")

summary(triple_diff_model_1)


 ### Purchases ###
Russian_reg_purchases <- plm(properties_bought ~ RUSSIA * POST, 
                       data = stock_OCOD, 
                       index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                       model = "within",
                       effect = "twoways")
summary(Russian_reg_purchases)

triple_diff_model_2 <- plm(asinh(properties_bought) ~ RUSSIA * POST * RUSSIAN_District, 
                         data = District_stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                         model = "within",
                         effect = "twoways")
summary(triple_diff_model_2)

    ### Sales ###

Russian_reg_sold <- plm(asinh(properties_sold) ~ RUSSIA * POST, 
                             data = stock_OCOD, 
                             index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                             model = "within",
                             effect = "twoways")
summary(Russian_reg_sold)

triple_diff_model_3 <- plm(asinh(properties_sold) ~ RUSSIA * POST * RUSSIAN_District, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")
summary(triple_diff_model_3)
stargazer(triple_diff_model_3)

stargazer(Russian_reg_stock, Russian_reg_purchases, Russian_reg_sold)
stargazer(triple_diff_model_1, triple_diff_model_2, triple_diff_model_3)

### Regressions for the alternative treatment groups ### 

Haven <- c("ANDORRA",
           "ANGUILLA",
           "ANTIGUA AND BARBUDA",
           "ARUBA",
           "AUSTRIA",
           "BAHAMAS",
           "BAHRAIN",
           "BARBADOS",
           "BELGIUM",
           "BELIZE",
           "BERMUDA",
           "BRITISH VIRGIN ISLANDS",
           "CAYMAN ISLANDS",
           "CHILE",
           "COOK ISLANDS",
           "COSTA RICA",
           "CURACAO",
           "CYPRUS",
           "DOMINICA",
           "GIBRALTAR",
           "GRENADA",
           "GUERNSEY",
           "HONG KONG SAR CHINA",
           "IRELAND",
           "ISLE OF MAN",
           "JERSEY",
           "JORDAN",
           "LEBANON",
           "LIBERIA",
           "LIECHTENSTEIN",
           "LUXEMBOURG",
           "MACAO SAR CHINA",
           "MALAYSIA",
           "MALDIVES",
           "MALTA",
           "MARSHALL ISLANDS",
           "MAURITIUS",
           "MONACO",
           "MONTSERRAT",
           "NAURU",
           "NETHERLANDS ANTILLES",
           "NIUE",
           "PANAMA",
           "SAMOA",
           "SAN MARINO",
           "SEYCHELLES",
           "SINGAPORE",
           "SINT MAARTEN",
           "ST. KITTS AND NEVIS",
           "ST. LUCIA",
           "ST. VINCENT AND GRENADINES",
           "SWITZERLAND",
           "TONGA",
           "TRINIDAD AND TOBAGO",
           "TURKS AND CAICOS ISLANDS",
           "U.S. VIRGIN ISLANDS",
           "URUGUAY",
           "VANUATU") ## These are the countries that are considered "Havens" in general from the paper is used

CPI <-c("LIBERIA","ST KITTS AND NEVIS", "GIBRALTAR", "CYPRUS","GUERNSEY","HONG KONG","BELIZE","BAHAMAS","MAURITIUS","MALTA")
AEOI_signatories <-c("GRENADA","TURKS AND CAICOS ISLANDS", "GUERNSEY", "ANGUILLA", "ISLE OF MAN", "CYPRUS", "COSTA RICA", "JERSEY", "GIBRALTAR", "BELIZE")

CPI_Robust <- c("LIBERIA","ST KITTS AND NEVIS", "GIBRALTAR")
AEOI_signatories_Robust <- c("GRENADA","TURKS AND CAICOS ISLANDS", "GUERNSEY")

stock_OCOD$Haven_dummy<- as.numeric(stock_OCOD$Country.Incorporated..1. %in% Haven)
stock_OCOD$CPI_dummy<- as.numeric(stock_OCOD$Country.Incorporated..1. %in% CPI)
stock_OCOD$AEOI_signatories_dummy<- as.numeric(stock_OCOD$Country.Incorporated..1. %in% AEOI_signatories)

stock_OCOD$CPI_dummy_Rob<- as.numeric(stock_OCOD$Country.Incorporated..1. %in% CPI_Robust)
stock_OCOD$AEOI_signatories_dummy_Rob<- as.numeric(stock_OCOD$Country.Incorporated..1. %in% AEOI_signatories_Robust)
 
### Treatment (1): Haven Regressions ###

model_panel_haven <- plm(asinh(properties_owned) ~ Haven_dummy*POST, 
                         data = stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                         model = "within",
                         effect = "twoways")
summary(model_panel_haven)

model_panel_haven_buy <- plm(asinh(properties_bought) ~ Haven_dummy*POST, 
                             data = stock_OCOD, 
                             index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                             model = "within",
                             effect = "twoways")
summary(model_panel_haven_buy)

model_panel_haven_sold <- plm(asinh(properties_sold) ~ Haven_dummy*POST, 
                              data = stock_OCOD, 
                              index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                              model = "within",
                              effect = "twoways")
summary(model_panel_haven_sold)

models_Havens <- list(model_panel_haven, model_panel_haven_buy, model_panel_haven_sold)
stargazer(models_Havens, title="Panel Data Model Summary", type="text")

stargazer(model_panel_haven, model_panel_haven_buy, model_panel_haven_sold)


### Treatment (2): CPI regressions ###

model_panel_CPI <- plm(properties_owned ~ CPI_dummy*POST, 
                         data = stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                         model = "within",
                         effect = "twoways")
summary(model_panel_CPI)

model_panel_CPI_buy <- plm(asinh(properties_bought) ~ CPI_dummy*POST, 
                             data = stock_OCOD, 
                             index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                             model = "within",
                             effect = "twoways")
summary(model_panel_CPI_buy)

model_panel_CPI_sold <- plm(asinh(properties_sold) ~ CPI_dummy*POST, 
                              data = stock_OCOD, 
                              index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                              model = "within",
                              effect = "twoways")
summary(model_panel_CPI_sold)

models_CPI <- list(model_panel_CPI, model_panel_CPI_buy, model_panel_CPI_sold)
stargazer(models_CPI, title="Panel Data Model Summary", type="text")
stargazer(model_panel_CPI, model_panel_CPI_buy, model_panel_CPI_sold)

### Treatment (3): Regression for AEOI signatories ###

model_panel_AEOI <- plm(properties_owned ~ AEOI_signatories_dummy*POST, 
                       data = stock_OCOD, 
                       index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                       model = "within",
                       effect = "twoways")
summary(model_panel_AEOI)

model_panel_AEOI_buy <- plm(asinh(properties_bought) ~ AEOI_signatories_dummy*POST, 
                           data = stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                           model = "within",
                           effect = "twoways")
summary(model_panel_AEOI_buy)

model_panel_AEOI_sold <- plm(asinh(properties_sold) ~ AEOI_signatories_dummy*POST, 
                            data = stock_OCOD, 
                            index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                            model = "within",
                            effect = "twoways")
summary(model_panel_AEOI_sold)


### Combined models 

# Load necessary library
library(plm)


#### Robustness regressions ####

### Regression for Russian Stock ###
Russian_reg_stock <- plm(asinh(properties_owned) ~ RUSSIA_Robust * POST, 
                         data = stock_OCOD, 
                         index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                         model = "within",
                         effect = "twoways")

summary(Russian_reg_stock)

### Purchases ###
Russian_reg_purchases <- plm(asinh(properties_bought) ~ RUSSIA_Robust * POST, 
                             data = stock_OCOD, 
                             index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                             model = "within",
                             effect = "twoways")
summary(Russian_reg_purchases)

Russian_reg_sold <- plm(asinh(properties_sold) ~ RUSSIA_Robust * POST, 
                        data = stock_OCOD, 
                        index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                        model = "within",
                        effect = "twoways")
summary(Russian_reg_sold)

stargazer(Russian_reg_stock, Russian_reg_purchases, Russian_reg_sold)
#### Russian District regressions robust ####

# Define the triple difference-in-differences panel data model
triple_diff_model_1 <- plm(asinh(properties_owned) ~ RUSSIA_Robust * POST * RUSSIAN_District_Robust, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")
summary(triple_diff_model_1)

triple_diff_model_2 <- plm(asinh(properties_bought) ~ RUSSIA_Robust * POST * RUSSIAN_District, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")
summary(triple_diff_model_2)

triple_diff_model_3 <- plm(asinh(properties_sold) ~ RUSSIA_Robust * POST * RUSSIAN_District_Robust, 
                           data = District_stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE", "District"), 
                           model = "within",
                           effect = "twoways")
summary(triple_diff_model_3)
stargazer(triple_diff_model_1, triple_diff_model_2, triple_diff_model_3)

### Treatment Robustness Checks #### 

model_panel_CPI_Rob <- plm(asinh(properties_owned) ~ CPI_dummy_Rob*POST, 
                       data = stock_OCOD, 
                       index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                       model = "within",
                       effect = "twoways")
summary(model_panel_CPI_Rob)

model_panel_CPI_buy_Rob <- plm(asinh(properties_bought) ~ CPI_dummy_Rob*POST, 
                           data = stock_OCOD, 
                           index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                           model = "within",
                           effect = "twoways")
summary(model_panel_CPI_buy_Rob)

model_panel_CPI_sold_Rob <- plm(asinh(properties_sold) ~ CPI_dummy_Rob*POST, 
                            data = stock_OCOD, 
                            index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                            model = "within",
                            effect = "twoways")
summary(model_panel_CPI_sold_Rob)

stargazer(model_panel_CPI_Rob, model_panel_CPI_buy_Rob, model_panel_CPI_sold_Rob)

### AEOI robust ### 

model_panel_AEOI_Rob <- plm(properties_owned ~ AEOI_signatories_dummy_Rob*POST, 
                        data = stock_OCOD, 
                        index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                        model = "within",
                        effect = "twoways")
summary(model_panel_AEOI_Rob)

model_panel_AEOI_buy_Rob <- plm(properties_bought ~ AEOI_signatories_dummy_Rob*POST, 
                            data = stock_OCOD, 
                            index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                            model = "within",
                            effect = "twoways")
summary(model_panel_AEOI_buy_Rob)

model_panel_AEOI_sold_Rob <- plm(asinh(properties_sold) ~ AEOI_signatories_dummy_Rob*POST, 
                             data = stock_OCOD, 
                             index = c("Country.Incorporated..1.", "OCOD_DATE"), 
                             model = "within",
                             effect = "twoways")
summary(model_panel_AEOI_sold_Rob)

stargazer(model_panel_AEOI_Rob, model_panel_AEOI_buy_Rob, model_panel_AEOI_sold)
