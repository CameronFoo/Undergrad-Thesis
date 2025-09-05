library(dplyr)

# Assuming Combined_OCOD has these columns: Country.Incorporated..1., OCOD_DATE, Title.Number, Property.Price

# Assuming you already have the 'stock_OCOD' data frame with 'price_volume' column

# Step 1: Calculate the change in price volume between months and determine if it's a purchase or sale
stock_OCOD <- stock_OCOD %>%
  arrange(Country.Incorporated..1., OCOD_DATE) %>%
  group_by(Country.Incorporated..1.) %>%
  mutate(
    change_in_price_volume = c(NA, diff(price_volume)),  # Calculate the difference in price volume
    purchase_volume = ifelse(change_in_price_volume > 0, change_in_price_volume, 0),  # Volume for purchases
    sale_volume = ifelse(change_in_price_volume < 0, abs(change_in_price_volume), 0)  # Volume for sales
  ) %>%
  ungroup()

# Step 2: Arrange by jurisdiction and OCOD_date to calculate the change in stock
stock_OCOD <- stock_OCOD %>%
  arrange(Country.Incorporated..1., OCOD_DATE)

# Step 3: Group by jurisdiction again to calculate the change within each jurisdiction
stock_OCOD <- stock_OCOD %>%
  group_by(Country.Incorporated..1.) %>%
  mutate(
    change_in_stock = c(NA, diff(stock_properties)),
    properties_bought = ifelse(change_in_stock > 0, change_in_stock, 0),
    properties_sold = ifelse(change_in_stock < 0, abs(change_in_stock), 0)
  ) %>%
  ungroup()

# Now stock_OCOD should have an additional column: price_volume, 
# which is the sum of Property.Price for each country per OCOD_DATE.


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
