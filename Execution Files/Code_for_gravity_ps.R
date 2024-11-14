#---------------------------------------#
#Author: BIJOY RATAN GHOSH
#Project: Gravity (International Trade)
#Last upated: 11/10/24
#---------------------------------------#
#Package Installation#
getwd()
R.Version()
# $platform
# [1] "aarch64-apple-darwin20"
# 
# $arch
# [1] "aarch64"
# 
# $os
# [1] "darwin20"
# 
# $system
# [1] "aarch64, darwin20"
# 
# $status
# [1] ""
# 
# $major
# [1] "4"
# 
# $minor
# [1] "4.2"
# 
# $year
# [1] "2024"
# 
# $month
# [1] "10"
# 
# $day
# [1] "31"
# 
# $`svn rev`
# [1] "87279"
# 
# $language
# [1] "R"
# 
# $version.string
# [1] "R version 4.4.2 (2024-10-31)"
# 
# $nickname
# [1] "Pile of Leaves"

install.packages("dplyr")
install.packages("data.table")  # all data manipulation (essential for all scripts)
# upon installation, you will be asked Do you want to install from sources the package which needs compilation? (Yes/no/cancel) 
# we will test with answering "no", to use multiple threads, consider the following more complex installation:
# or: install.packages("data.table", type = "source",repos = "https://Rdatatable.gitlab.io/data.table")
#https://github.com/Rdatatable/data.table/wiki/Installation
install.packages("devtools")  # needed to install from Github repositories
# for both of the installations below, choose "3"
devtools::install_github("julianhinz/gravity.distances")  #distances based on satellite lights
devtools::install_github("ckhead/HeadR")  # author-created functions 
install.packages("fixest")  #Berge estimator for lm and glm with high dimensional fixed effects.
install.packages("countrycode") # gets ISO codes for country names, among other things
install.packages("geodist")
install.packages("geosphere")
install.packages("maps")
install.packages("pwt9") # Penn World Tables
install.packages("OECD")  #API to OECD data
install.packages("WDI") # World Bank's World Development Indicators
packageurl <- "https://cran.r-project.org/src/contrib/Archive/imfr/imfr_0.1.9.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")  # IMF API
install.packages("readxl")
install.packages("stringi")  # string manipulation for car_prices.R
install.packages("RStata")  # for the gravity goods data
install.packages("rjson")  # for downloading comtrade
install.packages("comtradr") # for downloading comtrade
install.packages("tictoc")  # for timing various operations
install.packages("haven") # for reading and writing Stata dta files
install.packages("latex2exp")  # for put LaTeX math characters into labels in figures
install.packages("ggplot2")
install.packages("patchwork")
lapply(installed.packages()[, "Package"], require, character.only = TRUE)
#---------------------------------------#
#Reading Data#
data <- readRDS("/Users/bijoyratanghosh/Library/CloudStorage/Box-Box/sem 3/Int Trade/Empirical homework/Gravity/Data/Gravity_rds_V202211/Gravity_V202211.rds")
#---------------------------------------#
#Q1.
data %>%
  colnames()
# [1] "year"                   "country_id_o"           "country_id_d"           "iso3_o"                 "iso3_d"                 "iso3num_o"              "iso3num_d"             
# [8] "country_exists_o"       "country_exists_d"       "gmt_offset_2020_o"      "gmt_offset_2020_d"      "distw_harmonic"         "distw_arithmetic"       "distw_harmonic_jh"     
# [15] "distw_arithmetic_jh"    "dist"                   "main_city_source_o"     "main_city_source_d"     "distcap"                "contig"                 "diplo_disagreement"    
# [22] "scaled_sci_2021"        "comlang_off"            "comlang_ethno"          "comcol"                 "col45"                  "legal_old_o"            "legal_old_d"           
# [29] "legal_new_o"            "legal_new_d"            "comleg_pretrans"        "comleg_posttrans"       "transition_legalchange" "comrelig"               "heg_o"                 
# [36] "heg_d"                  "col_dep_ever"           "col_dep"                "col_dep_end_year"       "col_dep_end_conflict"   "empire"                 "sibling_ever"          
# [43] "sibling"                "sever_year"             "sib_conflict"           "pop_o"                  "pop_d"                  "gdp_o"                  "gdp_d"                 
# [50] "gdpcap_o"               "gdpcap_d"               "pop_source_o"           "pop_source_d"           "gdp_source_o"           "gdp_source_d"           "gdp_ppp_o"             
# [57] "gdp_ppp_d"              "gdpcap_ppp_o"           "gdpcap_ppp_d"           "pop_pwt_o"              "pop_pwt_d"              "gdp_ppp_pwt_o"          "gdp_ppp_pwt_d"         
# [64] "gatt_o"                 "gatt_d"                 "wto_o"                  "wto_d"                  "eu_o"                   "eu_d"                   "fta_wto"               
# [71] "fta_wto_raw"            "rta_coverage"           "rta_type"               "entry_cost_o"           "entry_cost_d"           "entry_proc_o"           "entry_proc_d"          
# [78] "entry_time_o"           "entry_time_d"           "entry_tp_o"             "entry_tp_d"             "tradeflow_comtrade_o"   "tradeflow_comtrade_d"   "tradeflow_baci"        
# [85] "manuf_tradeflow_baci"   "tradeflow_imf_o"        "tradeflow_imf_d"       

data1 <-data %>%
  select(year,iso3_o,iso3_d,fta_wto,gdp_o)

data2 <- data1 %>%
  filter(!is.na(gdp_o))# %>%
  #group_by(year, iso3_o, fta_wto)

data3 <- data.frame(data2 %>%
  filter(fta_wto == 1) %>%
  select(year,iso3_o,gdp_o) %>%
  distinct() %>%
  group_by(year) %>%
  mutate(
    gdp_fta=sum(gdp_o)
  ) %>%
  select(year,gdp_fta) %>%
  distinct())

data4 <- data.frame(data2 %>%
  select(year,iso3_o,gdp_o) %>%
  distinct() %>%
  group_by(year) %>%
  mutate(
    gdp_world=sum(gdp_o)
  ) %>%
  select(year,gdp_world) %>%
  distinct())

final_data <- data.frame(data3 %>%
  full_join(data4, by="year") %>%
  mutate(
    gdp_share = gdp_fta/gdp_world,
    gdp_fta = replace_na(gdp_fta, 0),  # Replace NA with 0 for years with no FTAs
    gdp_share = replace_na(gdp_share, 0)
  ) %>%
  arrange(year))  # Order by year for easier inspection



# Basic line plot with improvements
ggplot(final_data, aes(x = year, y = gdp_share)) +
  geom_line(color = "blue", size = 1) +  # Connected line
  geom_point(color = "red", size = 2) +  # Points for each observation
  labs(
    title = "Share of World GDP Covered by Free Trade Agreements",
    subtitle = "Evolution over time",
    x = "Year",
    y = "Share of World GDP",
    caption = "Source: Your dataset"
  ) +
  scale_y_continuous(
    labels = scales::percent,  # Format as percentage
    limits = c(0, 1)  # Assuming share is between 0 and 1
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# For an even more informative plot, you could add:
ggplot(final_data, aes(x = year, y = gdp_share)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  # Add smoothed trend line
  geom_smooth(method = "loess", color = "darkred", alpha = 0.2) +
  # Add text labels for key events/years
  labs(
    title = "FTA Coverage of World GDP",
    subtitle = "Share of global GDP in countries with at least one FTA",
    x = "Year",
    y = "Share of World GDP",
    caption = "Source: CEPII Gravity Dataset"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  # Rotate x-axis labels if needed
  theme_minimal() +
  geom_vline(xintercept = 1995, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 1996, y = 0.1, label = "WTO Formation", angle = 90) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Q2
print(summary(data$tradeflow_comtrade_d))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#  0        56      1372    369637     23815 563200374   3733458 

# Check for truly zero values
zero_check <- data %>%
  filter(tradeflow_comtrade_d == 0) %>%
  count()
print(zero_check)
#  n
#1 0

# Look at the distribution of very small values
small_values <- data %>%
  filter(tradeflow_comtrade_d > 0 & tradeflow_comtrade_d < 0.002) %>%
  count(tradeflow_comtrade_d) %>%
  arrange(tradeflow_comtrade_d)

print(small_values)
# # A tibble: 1 × 2
# tradeflow_comtrade_d     n
# *                <dbl> <int>
#   1                0.001   176

# Alternative check:
value_check <- data %>%
  summarise(
    exact_zeros = sum(tradeflow_comtrade_d == 0, na.rm = TRUE),
    min_trades = sum(tradeflow_comtrade_d == 0.001, na.rm = TRUE),
    nas = sum(is.na(tradeflow_comtrade_d))
  )

print(value_check)
#     exact_zeros min_trades  nas
# 1           0        176  3733458

# Look at the smallest values
data %>%
  filter(tradeflow_comtrade_d >= 0) %>%  # Non-negative values
  arrange(tradeflow_comtrade_d) %>%
  select(year, iso3_o, iso3_d, tradeflow_comtrade_d) %>%
  head(10)

# 
#    year iso3_o iso3_d tradeflow_comtrade_d
# 1  2002    ABW    MYS                0.001
# 2  2005    AGO    NER                0.001
# 3  2003    AIA    ALB                0.001
# 4  2015    AIA    UGA                0.001
# 5  2019    AND    KAZ                0.001
# 6  2005    ANT    UKR                0.001
# 7  2006    ANT    VCT                0.001
# 8  2012    ARG    BWA                0.001
# 9  2013    ARM    LKA                0.001
# 10 2011    ASM    BRB                0.001

# Count zero and nonzero values by year
# Calculate zero, nonzero counts, and their ratio by year

data_counts <- data.frame(data %>%
  select(year, iso3_o, iso3_d, tradeflow_comtrade_d) %>%
  group_by(year) %>%
  summarise(
    zero_count = sum(is.na(tradeflow_comtrade_d)),
    nonzero_count = sum(tradeflow_comtrade_d > 0, na.rm = TRUE),
    total_count = zero_count + nonzero_count,
    nonzero_ratio = nonzero_count / total_count
  ) %>%
  ungroup())

print(data_counts,n=74)

ggplot(data_counts, aes(x = year, y = nonzero_ratio)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  # Add vertical line for WTO formation
  geom_vline(xintercept = 1995, linetype = "dashed", alpha = 0.5) +
  # Adjust annotation position and size
  annotate("text", x = 1997, y = 0.05, # Adjusted x and y coordinates
           label = "WTO Formation", angle = 90, size = 3) + # Reduced text size
  labs(
    title = "Evolution of Non-Zero Trade Flows Over Time",
    subtitle = "Proportion of reported trade flows (> 0.001)",
    x = "Year",
    y = "Share of Non-Zero Trade",
    caption = "Source: CEPII Gravity Dataset"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, max(data_counts$nonzero_ratio) * 1.1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Add some margin to ensure text fits
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  )


#Q3
# Filtering for positive trade flows only and take logs
gravity_data <- data %>%
  filter(tradeflow_comtrade_d > 0) %>%  # only positive flows
  mutate(
    ln_trade = log(tradeflow_comtrade_d),
    ln_dist = log(dist),
    across(c(contig, fta_wto, comlang_off, col_dep_ever, gatt_o, gatt_d, wto_o, wto_d), ~replace_na(., 0)),
    gatt_wto = if_else((gatt_d==1 & gatt_o==1) | (wto_d==1 & wto_o==1), 1, 0)
  )

print(summary(gravity_data$tradeflow_comtrade_d))

# models
model1 <- feols(ln_trade ~ ln_dist | iso3_o^year + iso3_d^year, data = gravity_data)

model2 <- feols(ln_trade ~ ln_dist + contig | iso3_o^year + iso3_d^year, data = gravity_data)

model3 <- feols(ln_trade ~ ln_dist + contig + fta_wto | iso3_o^year + iso3_d^year, data = gravity_data)

model4 <- feols(ln_trade ~ ln_dist + contig + fta_wto + gatt_wto | iso3_o^year + iso3_d^year, data = gravity_data)

model5 <- feols(ln_trade ~ ln_dist + contig + fta_wto + gatt_wto + comlang_off | iso3_o^year + iso3_d^year, data = gravity_data)

model6 <- feols(ln_trade ~ ln_dist + contig + fta_wto + gatt_wto + comlang_off + col_dep_ever | iso3_o^year + iso3_d^year, data = gravity_data)

# comparison table
etable(model1, model2, model3, model4, model5, model6,
       title = "Gravity Equation Estimates")


#Q4
# 1. Baseline (log-log without zeros)
baseline_data <- data %>%
  filter(year==2019, dist > 0, tradeflow_comtrade_d > 0) %>%  # Note: only positive trades
  mutate(
    ln_trade = log(tradeflow_comtrade_d),
    ln_dist = log(dist),
    across(c(contig, fta_wto, comlang_off, col_dep_ever, gatt_o, gatt_d, wto_o, wto_d), 
           ~replace_na(., 0)),
    gatt_wto = if_else((gatt_d==1 & gatt_o==1) | (wto_d==1 & wto_o==1), 1, 0)
  )

# 2. Method i) ln(trade + 1)
plus_one_data <- data %>%
  filter(year==2019, dist > 0) %>%
  mutate(
    tradeflow_nonNA_0 = if_else(is.na(tradeflow_comtrade_d), 0, tradeflow_comtrade_d),
    ln_trade = log(tradeflow_nonNA_0 + 1),
    ln_dist = log(dist),
    across(c(contig, fta_wto, comlang_off, col_dep_ever, gatt_o, gatt_d, wto_o, wto_d), 
           ~replace_na(., 0)),
    gatt_wto = if_else((gatt_d==1 & gatt_o==1) | (wto_d==1 & wto_o==1), 1, 0)
  )

# Create safe minimum function
safe_min <- function(x, default = NULL) {
  if (length(x) == 0 || all(is.na(x))) {
    return(default)
  }
  min(x, na.rm = TRUE)
}

# Method ii) Winsorization with global minimum fallback
winsor_data <- data %>%
  filter(year == 2019, dist > 0) %>%
  # First calculate global minimum (from all non-zero trades)
  mutate(
    global_min = safe_min(tradeflow_comtrade_d[tradeflow_comtrade_d > 0])
  ) %>%
  group_by(iso3_o) %>%
  mutate(
    # Check if country has any non-zero trades
    has_trades = any(tradeflow_comtrade_d > 0, na.rm = TRUE),
    # Use safe_min for country-specific minimum
    country_min = safe_min(tradeflow_comtrade_d[tradeflow_comtrade_d > 0], global_min),
    # Apply winsorization with guaranteed non-NA minimum
    tradeflow_winsor = if_else(
      is.na(tradeflow_comtrade_d) | tradeflow_comtrade_d == 0,
      country_min,
      tradeflow_comtrade_d
    ),
    ln_trade = log(tradeflow_winsor)
  ) %>%
  ungroup() %>%
  mutate(
    ln_dist = log(dist),
    across(c(contig, fta_wto, comlang_off, col_dep_ever, gatt_o, gatt_d, wto_o, wto_d), 
           ~replace_na(., 0)),
    gatt_wto = if_else((gatt_d==1 & gatt_o==1) | (wto_d==1 & wto_o==1), 1, 0)
  )

# 4. Method iii) PPML
ppml_data <- data %>%
  filter(year==2019, dist > 0) %>%
  mutate(
    trade = if_else(is.na(tradeflow_comtrade_d), 0, tradeflow_comtrade_d),
    ln_dist = log(dist),
    across(c(contig, fta_wto, comlang_off, col_dep_ever, gatt_o, gatt_d, wto_o, wto_d), 
           ~replace_na(., 0)),
    gatt_wto = if_else((gatt_d==1 & gatt_o==1) | (wto_d==1 & wto_o==1), 1, 0)
  )

# Estimate all models
model_baseline <- feols(ln_trade ~ ln_dist + contig + fta_wto + gatt_wto + comlang_off + 
                          col_dep_ever | iso3_o + iso3_d, data = baseline_data)

model_plus_one <- feols(ln_trade ~ ln_dist + contig + fta_wto + gatt_wto + comlang_off + 
                          col_dep_ever | iso3_o + iso3_d, data = plus_one_data)

model_winsor <- feols(ln_trade ~ ln_dist + contig + fta_wto + gatt_wto + comlang_off + 
                        col_dep_ever | iso3_o + iso3_d, data = winsor_data)

model_ppml <- feglm(trade ~ ln_dist + contig + fta_wto + gatt_wto + comlang_off + 
                      col_dep_ever | iso3_o + iso3_d,
                    family = quasipoisson(),
                    data = ppml_data)
#NOTE: 9/83 fixed-effects (20,872 observations) removed because of only 0 outcomes.

# Create comparison table
etable(model_baseline, model_plus_one, model_winsor, model_ppml,
       cluster = ~iso3_o + iso3_d,
       headers = c("Baseline\n(no zeros)", "ln(trade+1)", "Winsorized", "PPML"),
       title = "Gravity Model Estimations for 2019",
       notes = c(
         "Standard errors clustered at origin and destination level",
         "Baseline: Traditional log-log excluding zero trades",
         "Method 1: Adding 1 before taking logarithm",
         "Method 2: Winsorizing zero flows with origin country minimum",
         "Method 3: PPML estimation (Santos Silva and Tenreyro, 2006)",
         paste("Sample sizes:",
               "\nBaseline:", nobs(model_baseline),
               "\nln(trade+1):", nobs(model_plus_one),
               "\nWinsorized:", nobs(model_winsor),
               "\nPPML:", nobs(model_ppml))
       ))

# Save results
saveRDS(list(
  baseline = model_baseline,
  plus_one = model_plus_one,
  winsor = model_winsor,
  ppml = model_ppml
), "gravity_results_comparison.rds")

#5
# Prepare data with proper logging
model_data <- data %>%
  filter(tradeflow_comtrade_d > 0) %>%
  mutate(
    ln_trade = log(tradeflow_comtrade_d),
    ln_dist = log(dist),  # Log the distance variable
    year_factor = as.factor(year)
  )

# Estimate the model with logged distance
gravity_model <- feols(
  ln_trade ~ ln_dist:year_factor | iso3_o^year + iso3_d^year,
  data = model_data
)

# Extract coefficients and standard errors
distance_effects <- coef(gravity_model)
se_distance <- sqrt(diag(vcov(gravity_model)))

# Create plot data
plot_data <- data.frame(
  year = as.numeric(gsub("ln_dist:year_factor", "", names(distance_effects))),
  coefficient = distance_effects,
  se = se_distance
) %>%
  mutate(
    ci_lower = coefficient - 1.96 * se,
    ci_upper = coefficient + 1.96 * se
  )

# Create the plot
ggplot(plot_data, aes(x = year, y = coefficient)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
              alpha = 0.2, fill = "blue") +
  geom_point(color = "blue", size = 3) +
  theme_minimal() +
  labs(
    title = "Evolution of Distance Effect in Gravity Model Over Time",
    x = "Year",
    y = "Elasticity of Trade to Distance (with 95% CI)",
    caption = "Note: More negative values indicate stronger distance effects"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# Print summary to verify reasonable coefficients
print(summary(distance_effects))


#6
# Data preparation
prepare_data <- function(data, origin_country = "KOR", base_country = "PRT", year_select = 2019) {
  
  # Filter for selected year and origin country
  data_filtered <- data %>%
    filter(year == year_select)
  
  # Prepare EU data (Figure 1)
  eu_countries <- data_filtered %>%
    filter(eu_d == 1) %>%
    pull(iso3_d) %>%
    unique()
  
  # For Figure 1 (normalized to Portugal)
  eu_trade_data <- data_filtered %>%
    filter(
      iso3_o == origin_country,
      iso3_d %in% eu_countries
    ) %>%
    # Get Portugal's GDP and trade for normalization
    left_join(
      data_filtered %>%
        filter(iso3_o == origin_country, iso3_d == base_country) %>%
        select(base_gdp = gdp_d, base_trade = tradeflow_comtrade_d),
      by = character()
    ) %>%
    mutate(
      gdp_share = gdp_d / base_gdp,
      trade_share = tradeflow_comtrade_d / base_trade
    ) %>%
    select(iso3_d, gdp_share, trade_share)
  
  # For Figure 2
  distance_data <- data_filtered %>%
    filter(iso3_o == origin_country) %>%
    mutate(
      trade_gdp_share = (tradeflow_comtrade_d / gdp_d) * 100,
      # Create group indicators
      group = case_when(
        eu_d == 1 ~ "EU",  # If destination is EU
        fta_wto == 1 ~ "FTA", # If has FTA
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(trade_gdp_share), trade_gdp_share > 0) # Remove zeros and NAs
  
  list(eu_trade = eu_trade_data, distance = distance_data)
}

# Plot functions
eu_trade_plot <- function(data, type = "exports") {
  ggplot(data, aes(x = gdp_share, y = trade_share)) +
    geom_point(color = "blue") +
    geom_text(aes(label = iso3_d), hjust = -0.2, color = "blue") +
    geom_smooth(method = "lm", color = "black") +
    scale_x_log10(breaks = c(0.05, 0.1, 0.5, 1, 5, 10)) +
    scale_y_log10(breaks = c(0.05, 0.1, 0.5, 1, 5, 10)) +
    labs(
      title = paste("South Korea's", type, "to EU, 2019"),
      x = "GDP (PRT = 1)",
      y = paste(type, "(PRT = 1)")
    ) +
    theme_minimal() +
    # Add regression statistics
    annotate("text", x = 0.1, y = max(data$trade_share),
             label = paste("slope =", 
                           round(coef(lm(log(trade_share) ~ log(gdp_share), data))[2], 3)),
             hjust = 0) +
    annotate("text", x = 0.1, y = max(data$trade_share)/1.5,
             label = paste("fit =", 
                           round(summary(lm(log(trade_share) ~ log(gdp_share), data))$r.squared, 2)),
             hjust = 0)
}

distance_plot <- function(data, type = "exports") {
  ggplot(data, aes(x = dist)) +
    geom_point(aes(y = trade_gdp_share, color = group, shape = group)) +
    geom_smooth(aes(y = trade_gdp_share), method = "lm", color = "black") +
    scale_x_log10(breaks = c(500, 1000, 2000, 5000, 10000, 20000),
                  labels = scales::comma) +
    scale_y_log10() +
    labs(
      title = paste("South Korea's", type, "(2019)"),
      x = "Distance in kms",
      y = paste(type, "/Partner's GDP (%, log scale)")
    ) +
    theme_minimal() +
    # Add regression statistics
    annotate("text", x = min(data$dist), y = max(data$trade_gdp_share),
             label = paste("slope =", 
                           round(coef(lm(log(trade_gdp_share) ~ log(dist), data))[2], 3)),
             hjust = 0) +
    scale_color_manual(name = "",
                       values = c("black", "blue", "darkgreen"),
                       labels = c("Other", "EU", "FTA")) +
    scale_shape_manual(name = "",
                       values = c(15, 16, 17),
                       labels = c("Other", "EU", "FTA"))
}

# Create plots
# For exports
prepared_data_exports <- prepare_data(data, "KOR", "PRT", 2019)
p1 <- eu_trade_plot(prepared_data_exports$eu_trade, "exports")
p3 <- distance_plot(prepared_data_exports$distance, "exports")

# For imports
prepared_data_imports <- prepare_data(data %>% 
                                        mutate(tradeflow_comtrade_d = tradeflow_comtrade_o), 
                                      "KOR", "PRT", 2019)
p2 <- eu_trade_plot(prepared_data_imports$eu_trade, "imports")
p4 <- distance_plot(prepared_data_imports$distance, "imports")

# Combine plots
figure1 <- p1 + p2
figure2 <- p3 + p4

# Display plots
figure1
figure2

















