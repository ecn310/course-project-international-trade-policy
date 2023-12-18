install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('tidyverse')
library(tidyverse)
df<- read.csv('United States Product Imports from China.csv')
# Filter the data according to the given conditions
##clothing
filtered_df<- df %>%
  filter(Reporter.Name == "United States", 
         Partner.Name == "China", 
         Product.Group == "Food Products")
filtered_df2 <- df %>%
  filter(Reporter.Name == "United States", 
         Partner.Name == "China", 
         Product.Group == "Chemicals")
filtered_df3 <- df %>%
  filter(Reporter.Name == "United States", 
         Partner.Name == "China", 
        Product.Group == " Capital goods")
#Textiles
filtered_df4 <- df %>%
  filter(Reporter.Name == "United States", 
         Partner.Name == "China", 
         Product.Group == "Textiles and Clothing")
# Select the time range from 2017 to 2021
##capital goods
#time_range_df2 <- filtered_df2 %>%
 
# select(X2017:X2021)
##textiles
time_range_df2 <- filtered_df3 %>%
  select(X2017:X2021)
time_range_df3 <- filtered_df4 %>%
  select(X2017:X2021)
# Convert the data from wide to long format for plotting
long_df2 <- pivot_longer(time_range_df2, 
                        cols = X2017:X2021, 
                        names_to = "Year", 
                        values_to = "Value")
##textiles
long_df2 <- pivot_longer(time_range_df2, 
                         cols = X2017:X2021, 
                         names_to = "Year", 
                         values_to = "Value")

long_df3 <- pivot_longer(time_range_df3, 
                         cols = X2017:X2021, 
                         names_to = "Year", 
                         values_to = "Value")
# Adjust the Year variable to be a simple year format (e.g., 2017)
long_df2$Year <- sub("x", "", long_df$Year)

# Create the visual comparison
##food
ggplot(long_df2, aes(x = Year, y = Value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Food Trade U.S. and China (2017-2021)",
       x = "Year",
       y = "Trade Volume") +
  theme_minimal()
##Capital goods
ggplot(long_df3, aes(x = Year, y = Value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Food Trade U.S. and China (2017-2021)",
       x = "Year",
       y = "Trade Volume") +
  theme_minimal()
##chemicals with china
filtered_China_Chem <- df %>%
  filter(Reporter.Name == "United States", 
         Partner.Name == "China", 
         Product.Group == "Chemicals")
time_range_df3 <- filtered_China_Chem %>%
  select(X2017:X2021)

long_df3 <- pivot_longer(time_range_df3, 
                         cols = X2017:X2021, 
                         names_to = "Year", 
                         values_to = "Value")
ggplot(long_df3, aes(x = Year, y = Value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Chemical Trade U.S. and China (2017-2021)",
       x = "Year",
       y = "Trade Volume") +
  theme_minimal()

library(ggplot2)
library(dplyr)

df2 <- read.csv('data-17844025.csv')
# Filter the data according to the given conditions

  # First, filter the data frame for the specified period labels

# Selecting the relevant data
##food
filtered_EU_food1<- df2 %>% filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB =='Hams, shoulders and cuts thereof of swine, salted, in brine, dried or smoked, with bone in',              
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))

# Create the visual
ggplot(filtered_EU_food1, aes(x = PERIOD_LAB, y = PRODUCT_LAB, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Food Trade E.U. and U.S.",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()

##Batteries 
filtered_batteries <- df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB == 'Lithium cells and batteries (excl. spent)',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))

# Create the visual
ggplot(filtered_batteries, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Capital Goods EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()

##textiles
filtered_textiles <- df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB == 'Articles of apparel and clothing accessories produced by the stitching or sticking together of plastic sheeting, incl. gloves, mittens and mitts (excl. goods of 9619)',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))
ggplot(filtered_textiles, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Textiles Goods EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()


##Chemicals to show non-tariff goods
filtered_chemicals1 <- df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB == 'DDT (ISO) "clofenotane (INN)", in packings of a net weight content <= 300 g',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))
ggplot(filtered_chemicals1, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Chemicals Goods EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()

filtered_chemicals2 <- df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US', 
         PRODUCT_LAB == 'Hydrogen peroxide, whether or not solidified with urea',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))
ggplot(filtered_chemicals2, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Chemicals Goods EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()


filtered_EU_Food1 <- df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB == 'Semi-milled or wholly milled rice, whether or not polished or glazed',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))
ggplot(filtered_EU_Food1, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Rice and Starch Goods EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()

filtered_EU_Food2 <- df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB == 'Uncooked pasta, not stuffed or otherwise prepared, containing eggs',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))
ggplot(filtered_EU_Food2, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Non-Perishable EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()

filtered_EU_Food3<-df2 %>%
  filter(REPORTER_LAB == 'European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)',
         PARTNER == 'US',
         PRODUCT_LAB == 'Fresh, chilled or frozen meat and edible offal of pigeons, game, reindeer and other animals (excl. bovine animals, swine, sheep, goats, horses, asses, mules, hinnies, poultry "fowls of the species Gallus domesticus, ducks, geese, turkeys, guinea fowl", rabbits, hares, primates, whales, dolphins and porpoises "mammals of the order Cetacea", manatees and dugongs "mammals of the order Sirenia", seals, sea lions and walruses "mammals of the suborder Pinnipedia" and reptiles)',
         PERIOD_LAB %in% c('Jan.-Dec. 2017', 'Jan.-Dec. 2018', 'Jan.-Dec. 2019', 'Jan.-Dec. 2020', 'Jan.-Dec. 2021'))
ggplot(filtered_EU_Food3, aes(x = PERIOD_LAB, y = INDICATOR_VALUE, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Poultry EU and US",
       x = "PERIOD",
       y = "TRADE VOLUME") +
  theme_minimal()
