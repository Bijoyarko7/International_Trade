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

data_counts <- data %>%
  select(year, iso3_o, iso3_d, tradeflow_comtrade_d) %>%
  group_by(year) %>%
  summarise(
    zero_count = sum(tradeflow_comtrade_d == 0, na.rm = TRUE),
    nonzero_count = sum(tradeflow_comtrade_d > 0, na.rm = TRUE),
    total_count = zero_count + nonzero_count,
    nonzero_ratio = nonzero_count / total_count
  ) %>%
  ungroup()

print(data_counts,n=74)

# # A tibble: 74 × 5
# year zero_count nonzero_count total_count nonzero_ratio
# <int>      <int>         <int>       <int>         <dbl>
#   1  1948          0             0           0           NaN
# 2  1949          0             0           0           NaN
# 3  1950          0             0           0           NaN
# 4  1951          0             0           0           NaN
# 5  1952          0             0           0           NaN
# 6  1953          0             0           0           NaN
# 7  1954          0             0           0           NaN
# 8  1955          0             0           0           NaN
# 9  1956          0             0           0           NaN
# 10  1957          0             0           0           NaN
# 11  1958          0             0           0           NaN
# 12  1959          0             0           0           NaN
# 13  1960          0             0           0           NaN
# 14  1961          0             0           0           NaN
# 15  1962          0          5588        5588             1
# 16  1963          0          6025        6025             1
# 17  1964          0          6377        6377             1
# 18  1965          0          7159        7159             1
# 19  1966          0          7585        7585             1
# 20  1967          0          8020        8020             1
# 21  1968          0          8194        8194             1
# 22  1969          0          8255        8255             1
# 23  1970          0          9586        9586             1
# 24  1971          0          9825        9825             1
# 25  1972          0         10172       10172             1
# 26  1973          0         10483       10483             1
# 27  1974          0         11218       11218             1
# 28  1975          0         11572       11572             1
# 29  1976          0         11729       11729             1
# 30  1977          0         11759       11759             1
# 31  1978          0         11575       11575             1
# 32  1979          0         11735       11735             1
# 33  1980          0         11629       11629             1
# 34  1981          0         11451       11451             1
# 35  1982          0         10947       10947             1
# 36  1983          0         11215       11215             1
# 37  1984          0         10609       10609             1
# 38  1985          0         10959       10959             1
# 39  1986          0         10818       10818             1
# 40  1987          0         10930       10930             1
# 41  1988          0         11201       11201             1
# 42  1989          0         11679       11679             1
# 43  1990          0         12150       12150             1
# 44  1991          0         11888       11888             1
# 45  1992          0         13139       13139             1
# 46  1993          0         13682       13682             1
# 47  1994          0         14041       14041             1
# 48  1995          0         16061       16061             1
# 49  1996          0         17052       17052             1
# 50  1997          0         18198       18198             1
# 51  1998          0         18441       18441             1
# 52  1999          0         19475       19475             1
# 53  2000          0         22180       22180             1
# 54  2001          0         22784       22784             1
# 55  2002          0         23228       23228             1
# 56  2003          0         23684       23684             1
# 57  2004          0         23860       23860             1
# 58  2005          0         23959       23959             1
# 59  2006          0         24895       24895             1
# 60  2007          0         25829       25829             1
# 61  2008          0         26058       26058             1
# 62  2009          0         25645       25645             1
# 63  2010          0         26347       26347             1
# 64  2011          0         26425       26425             1
# 65  2012          0         26530       26530             1
# 66  2013          0         26799       26799             1
# 67  2014          0         26658       26658             1
# 68  2015          0         27033       27033             1
# 69  2016          0         27346       27346             1
# 70  2017          0         27548       27548             1
# 71  2018          0         27275       27275             1
# 72  2019          0         25975       25975             1
# 73  2020          0         23358       23358             1
# 74  2021          0             0           0           NaN

#plot(data_counts$year,data_counts$nonzero_ratio)

check_data <- data %>%
  select(year, iso3_o, iso3_d, tradeflow_comtrade_d) %>%
  group_by(year) %>%
  summarise(
    zero_count = sum(tradeflow_comtrade_d == 0, na.rm = TRUE),
    nonzero_count = sum(tradeflow_comtrade_d > 0, na.rm = TRUE),
    na_count = sum(is.na(tradeflow_comtrade_d)),
    total_rows = n()
  ) %>%
  ungroup()

print(check_data,n=74)

# # A tibble: 74 × 5
# year zero_count nonzero_count na_count total_rows
# <int>      <int>         <int>    <int>      <int>
#   1  1948          0             0    63504      63504
# 2  1949          0             0    63504      63504
# 3  1950          0             0    63504      63504
# 4  1951          0             0    63504      63504
# 5  1952          0             0    63504      63504
# 6  1953          0             0    63504      63504
# 7  1954          0             0    63504      63504
# 8  1955          0             0    63504      63504
# 9  1956          0             0    63504      63504
# 10  1957          0             0    63504      63504
# 11  1958          0             0    63504      63504
# 12  1959          0             0    63504      63504
# 13  1960          0             0    63504      63504
# 14  1961          0             0    63504      63504
# 15  1962          0          5588    57916      63504
# 16  1963          0          6025    57479      63504
# 17  1964          0          6377    57127      63504
# 18  1965          0          7159    56345      63504
# 19  1966          0          7585    55919      63504
# 20  1967          0          8020    55484      63504
# 21  1968          0          8194    55310      63504
# 22  1969          0          8255    55249      63504
# 23  1970          0          9586    53918      63504
# 24  1971          0          9825    53679      63504
# 25  1972          0         10172    53332      63504
# 26  1973          0         10483    53021      63504
# 27  1974          0         11218    52286      63504
# 28  1975          0         11572    51932      63504
# 29  1976          0         11729    51775      63504
# 30  1977          0         11759    51745      63504
# 31  1978          0         11575    51929      63504
# 32  1979          0         11735    51769      63504
# 33  1980          0         11629    51875      63504
# 34  1981          0         11451    52053      63504
# 35  1982          0         10947    52557      63504
# 36  1983          0         11215    52289      63504
# 37  1984          0         10609    52895      63504
# 38  1985          0         10959    52545      63504
# 39  1986          0         10818    52686      63504
# 40  1987          0         10930    52574      63504
# 41  1988          0         11201    52303      63504
# 42  1989          0         11679    51825      63504
# 43  1990          0         12150    51354      63504
# 44  1991          0         11888    51616      63504
# 45  1992          0         13139    50365      63504
# 46  1993          0         13682    49822      63504
# 47  1994          0         14041    49463      63504
# 48  1995          0         16061    47443      63504
# 49  1996          0         17052    46452      63504
# 50  1997          0         18198    45306      63504
# 51  1998          0         18441    45063      63504
# 52  1999          0         19475    44029      63504
# 53  2000          0         22180    41324      63504
# 54  2001          0         22784    40720      63504
# 55  2002          0         23228    40276      63504
# 56  2003          0         23684    39820      63504
# 57  2004          0         23860    39644      63504
# 58  2005          0         23959    39545      63504
# 59  2006          0         24895    38609      63504
# 60  2007          0         25829    37675      63504
# 61  2008          0         26058    37446      63504
# 62  2009          0         25645    37859      63504
# 63  2010          0         26347    37157      63504
# 64  2011          0         26425    37079      63504
# 65  2012          0         26530    36974      63504
# 66  2013          0         26799    36705      63504
# 67  2014          0         26658    36846      63504
# 68  2015          0         27033    36471      63504
# 69  2016          0         27346    36158      63504
# 70  2017          0         27548    35956      63504
# 71  2018          0         27275    36229      63504
# 72  2019          0         25975    37529      63504
# 73  2020          0         23358    40146      63504
# 74  2021          0             0    63504      63504



















