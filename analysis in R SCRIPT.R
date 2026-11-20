##########Install the R packages
install.packages(c(
  "tidyverse",
  "janitor",
  "corrplot",
  "ggpubr",
  "broom"
))


library(tidyverse)
library(janitor)
library(corrplot)
library(ggpubr)
library(broom)

# Set your working directory
setwd("D:/PADDY _PROJECT")

# Import dataset
paddy <- read_csv("paddydataset.csv")

# Check dataset dimensions
dim(paddy)

# Check column names
colnames(paddy)

# Examine structure
str(paddy)

# View first six observations
head(paddy)

# Check summary
summary(paddy)


############Clean the dataset
library(tidyverse)
library(janitor)

paddy_clean <- paddy %>%
  clean_names()

# Check cleaned column names
colnames(paddy_clean)

# Check dimensions
dim(paddy_clean)

# Convert categorical variables into factors
paddy_clean <- paddy_clean %>%
  mutate(
    agriblock = as.factor(agriblock),
    variety = as.factor(variety),
    soil_types = as.factor(soil_types),
    nursery = as.factor(nursery)
  )

######################## 2nd Overall yield statistics
paddy_clean %>%
  summarise(
    Mean = mean(yield_kg_ha, na.rm = TRUE),
    Median = median(yield_kg_ha, na.rm = TRUE),
    SD = sd(yield_kg_ha, na.rm = TRUE),
    Minimum = min(yield_kg_ha, na.rm = TRUE),
    Maximum = max(yield_kg_ha, na.rm = TRUE)
  )




##############1st We need to calculate yield per hectare first.
Step 1. Create the new column
paddy_clean <- paddy_clean %>%
  mutate(
    yield_kg_ha = paddy_yield_in_kg / hectares
  )
##################Check whether it was created
colnames(paddy_clean)

overall_yield <- paddy_clean %>%
  summarise(
    Mean = mean(yield_kg_ha, na.rm = TRUE),
    Median = median(yield_kg_ha, na.rm = TRUE),
    SD = sd(yield_kg_ha, na.rm = TRUE),
    Minimum = min(yield_kg_ha, na.rm = TRUE),
    Maximum = max(yield_kg_ha, na.rm = TRUE)
  )

###############Save the result as a file
print(overall_yield)

write.csv(
  overall_yield,
  "overall_yield_statistics.csv",
  row.names = FALSE
)

# Yield according to soil type
soil_summary <- paddy_clean %>%
  group_by(soil_types) %>%
  summarise(
    Mean_Yield = mean(yield_kg_ha, na.rm = TRUE),
    SD = sd(yield_kg_ha, na.rm = TRUE),
    Samples = n(),
    .groups = "drop"
  )

print(soil_summary)

# Yield according to variety
variety_summary <- paddy_clean %>%
  group_by(variety) %>%
  summarise(
    Mean_Yield = mean(yield_kg_ha, na.rm = TRUE),
    SD = sd(yield_kg_ha, na.rm = TRUE),
    Samples = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Yield))

print(variety_summary)

# Yield according to agricultural block
block_summary <- paddy_clean %>%
  group_by(agriblock) %>%
  summarise(
    Mean_Yield = mean(yield_kg_ha, na.rm = TRUE),
    SD = sd(yield_kg_ha, na.rm = TRUE),
    Samples = n(),
    .groups = "drop"
  )

print(block_summary)


########Check missing values
missing_values <- paddy_clean %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

print(missing_values)
####################check duplicate records:
sum(duplicated(paddy_clean))


ggplot(variety_summary,
       aes(x = variety, y = Mean_Yield,
           fill = variety)) +
  
  geom_col(width = 0.6) +
  
  geom_text(
    aes(label = round(Mean_Yield, 0)),
    vjust = -0.5,
    size = 4
  ) +
  
  labs(
    title = "Mean Paddy Yield Across Rice Varieties",
    x = "Rice Variety",
    y = "Mean Yield (kg/ha)"
  ) +
  
  theme_minimal() +
  
  theme(legend.position = "none")

########################################
ggplot(block_summary,
       aes(x = agriblock, y = Mean_Yield,
           fill = agriblock)) +
  
  geom_col(width = 0.7) +
  
  geom_text(
    aes(label = round(Mean_Yield, 0)),
    vjust = -0.5,
    size = 3.5
  ) +
  
  labs(
    title = "Mean Paddy Yield Across Agricultural Blocks",
    x = "Agricultural Block",
    y = "Mean Yield (kg/ha)"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  )

##########################''
ggplot(variety_summary,
       aes(x = variety, y = Mean_Yield,
           fill = variety)) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(
      ymin = Mean_Yield - SD,
      ymax = Mean_Yield + SD
    ),
    width = 0.2
  ) +
  geom_text(
    aes(label = round(Mean_Yield)),
    vjust = -1.5
  ) +
  labs(
    title = "Mean Paddy Yield Across Rice Varieties",
    x = "Rice Variety",
    y = "Mean Yield (kg/ha)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#######Check total observations
nrow(paddy_clean)

############################
total_rows <- nrow(paddy_clean)

duplicate_count <- sum(duplicated(paddy_clean))

duplicate_percentage <- (duplicate_count / total_rows) * 100

print(duplicate_percentage)

#######display duplicate
duplicate_records <- paddy_clean %>%
  filter(duplicated(paddy_clean))

View(duplicate_records)


head(duplicate_records, 10)

########First, check whether your dataset contains any unique identification column.
colnames(paddy_clean)

