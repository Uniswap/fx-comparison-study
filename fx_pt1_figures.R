library("tidyverse")
library(ggrepel)
library(ggthemes)
library("lubridate")
set.seed(42)
library(scales)
library(RColorBrewer)
library(zoo)
library("elementalist")

setwd("/Users/austin/code/fx")
### uni light theme
colors <- c(
  "#FA3492", # uni pink
  "#A5AFFF", # pastel blue
  "#AD218E", # maroon
  "#5769FF", #navy blue
  "#CDD0E8", # light grey
  "#FFA6D1" # light pink
)

uni_black <- "#3D3D3D"
uni_lightgrey <- "#F1F1F1"
uni_grey <- "#D0D0D0"
uni_pink <- '#FA3492'
uni_darkgrey <- "#747474"
uni_white <- "#FFFFFF"
uni_black <- "#000000"

uni_theme <-     theme(
  text = element_text(size = 8, family = 'Inter', color = "black"),
  rect = element_rect(fill = uni_white), # all rectangles
  axis.title = element_text(size = 15, color = uni_black, family = 'Inter'), 
  plot.title = element_text(size = 20, color = uni_darkgrey, family = 'Inter'),
  legend.title = element_text(size = 18, color = uni_black, family = 'Inter'),
  legend.text = element_text(size = 11, color = uni_darkgrey, family = 'Inter', margin = margin(l=-3, r = 4)),
  axis.text.x = element_text(size = 11, color = uni_darkgrey, family = 'Inter', margin = margin(t=10, b=10)),
  axis.text.y = element_text(size = 11, color = uni_darkgrey, family = 'Inter', margin = margin(l=10, r=10)),
  legend.box.background = element_blank(),
  
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_line(color = uni_darkgrey, size = rel(.25)),
  axis.ticks.length.x.bottom = unit(.5, "cm"),
  panel.background = element_rect_round(linetype = 0, fill = uni_white),
  plot.background = element_rect(color = uni_white),
  panel.spacing = unit(-1, "lines"),
  panel.grid.major.y = element_line(colour = uni_grey, size = rel(.25)),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position ="top", 
  legend.box = "horizontal",
  legend.margin = margin(5,5,5,5),
  legend.box.spacing = margin(b=20),
  legend.justification = 'right',
  legend.box.just = 'right',
  legend.key = element_rect(fill = "transparent", colour = "transparent"),
  plot.margin = margin(1, 1, 1, 1, unit = "cm")
)
# -------------------------------
# Plot 1 - Level of Remittance
# -------------------------------

df <- read_csv('remitance_data.csv') %>% 
        filter(`Migrant remittance inflows (US$ million)` %in% c('Low-and Middle-Income Countries', 'China'))
df <- df %>%
        gather(key = var_name, value = value, 2:ncol(df)) %>% 
        spread(key = names(df)[1],value = 'value') 

plot_df <- df %>% 
        rename(low_middle = `Low-and Middle-Income Countries`) %>% 
        mutate(remit = as.numeric(low_middle)  - as.numeric(China))  %>%  
      mutate(var_name = year(as.Date(as.character(var_name), format = "%Y"))) %>% 
      mutate(value = as.numeric(remit) / 1e3) %>% 
      filter(!is.na(value))

p<-ggplot(data=plot_df, aes(x=var_name, y=value)) +
  geom_line(stat="identity", color = uni_pink) +
  labs(x=" ", y="Billions ($)", title = "") +
  uni_theme + 
  #scale_y_continuous(label = comma) +
  theme(plot.title = element_text(size = 15, color = uni_darkgrey, family = 'Inter'))
p

ggsave("remittance_flows.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

# -------------------------------
# Plot 2 - Remittance vs. Exports
# -------------------------------

df <- read_csv('chart2_exports.csv')

data <- c()
for (country in unique(df['Country'])[['Country']]) {
  country_df <- df %>% filter(Country == country)
  rem <- c(country, country_df[['Remittance Share']], 'Remittances (% of GDP, 2021)')
  ex <- c(country, country_df[['Exports / GDP']], 'Merchandise Exports (% of GDP, 2021)')
  
  if (length(data) == 0) {
    data <- rem
    data <- rbind(data, ex)
  } else {
    data <- rbind(data, rem)
    data <- rbind(data, ex)
  }
}
data <- as_tibble(data, .name_repair = 'minimal')
colnames(data) <- c("country", "share", 'name')

rem <- data %>% filter(name == 'Remittances (% of GDP, 2021)')

data <- data %>% 
  mutate(share = as.numeric(share)) %>% 
  mutate(country = factor(country, levels = unique(rem$country[order(rem$share)])))

p <- ggplot(data, aes(factor(country), y = share, fill=name))+
  geom_bar(position='dodge',stat='identity') +
  scale_fill_manual(values=c('#A5AFFF', '#747474')) +
  labs(x = " ", y = "Share of GDP") +
  guides(fill=guide_legend(title="")) +
  uni_theme +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position ="bottom") + 
  coord_flip()
p

ggsave("merchandise_vs_remitt.png", width = 9, height = 6, dpi = 300)#, dpi = 125)


# -------------------------------
# Plot 3 - Remittance Costs
# -------------------------------

df1 <- read_csv('rpw_dataset_2011_2022_q2.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 total cost %`)
  
df2 <- read_csv('rpw_dataset_2011_2016.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 total cost %`)

combined_df <- bind_rows(df1, df2)


df <- combined_df %>% 
  mutate(period = yq(period)) %>% 
  mutate(year = year(period)) %>% 
  mutate(firm_category = ifelse(firm_type %in% c('Bank', 'Bank / Money Transfer Operator', 'Bank/Post office'), 'Bank',
                                ifelse(firm_type %in% c('Money Transfer Operator', 'Money Transfer Operator / Building Society', 
                                                        'Money Transfer Operator / Post office'), 'Money Transfer Operator',
                                       ifelse(firm_type %in% c('Non-Bank FI'), 'Financial Technology', firm_type)))) %>% 
  group_by(firm_category, year) %>% 
  summarize(median = median(`cc1 total cost %`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(firm_category %in% c('Bank', 'Money Transfer Operator'))#, 'Financial Technology')) 
  


p <- ggplot(df, aes(factor(year), y = median, fill=factor(firm_category)))+
  geom_bar(position='dodge',stat='identity') +
  scale_fill_manual(values=c("#747474", '#A5AFFF', '#AD218E')) +
  labs(y = "Median fee of money remmitance (%)", x = "", title = '') +
  guides(fill=guide_legend(title="")) +
  uni_theme 
p

ggsave("cost_of_remmitance.png", width = 9, height = 6, dpi = 300)#, dpi = 125)
  
# -------------------------------
# Plot 4 - Remittance Costs and v3 200$
# -------------------------------


df1 <- read_csv('rpw_dataset_2011_2022_q2.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 total cost %`)

df2 <- read_csv('rpw_dataset_2011_2016.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 total cost %`)

combined_df <- bind_rows(df1, df2)


df <- combined_df %>% 
  mutate(period = yq(period)) %>% 
  mutate(year = year(period)) %>% 
  mutate(firm_category = ifelse(firm_type %in% c('Bank', 'Bank / Money Transfer Operator', 'Bank/Post office'), 'Bank',
                                ifelse(firm_type %in% c('Money Transfer Operator', 'Money Transfer Operator / Building Society', 
                                                        'Money Transfer Operator / Post office'), 'Money Transfer Operator',
                                       ifelse(firm_type %in% c('Non-Bank FI'), 'Financial Technology', firm_type)))) %>% 
  group_by(firm_category, year) %>% 
  summarize(median = median(`cc1 total cost %`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(firm_category %in% c('Bank', 'Money Transfer Operator'))#, 'Financial Technology')) 


mt <- df %>% 
    filter(year == max(year)) %>% 
    mutate(median = median/100 * 200)

### https://etherscan.io/tx/0x49b043032a803b1f97e54d68435117714dab5db70e903d6b763ba51b29cab2df
### 1.46 * 2 + 2.02
# 5.04 = (1.46 * 2 + 2.02)  + .0503 / 100 * 200
# .43 = (.1 * 2 + .13) + .0503 / 100 * 200
mt <- rbind(mt, c('Uniswap v3 (ETH)', '2022', 5.04))
mt <- rbind(mt, c('Uniswap v3 (L2)', '2022', 0.43))

p <- ggplot(mt, aes(factor(firm_category), y = as.numeric(median), 
                    fill = factor(firm_category))) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=c('#747474', '#A5AFFF', '#FA3492', '#AD218E')) +
  labs(y = "Total Remmitance Fee ($)", x = "", title = '') +
  guides(fill=guide_legend(title="")) +
  uni_theme 
p

ggsave("cost_of_remmitance_vs_v3_200.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

# -------------------------------
# Plot 5 - Remittance Costs and v3 500$
# -------------------------------


df1 <- read_csv('rpw_dataset_2011_2022_q2.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 lcu fee`, `cc2 lcu fee`, `cc2 fx margin`, `cc2 total cost %`)

df2 <- read_csv('rpw_dataset_2011_2016.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 lcu fee`, `cc2 lcu fee`, `cc2 fx margin`, `cc2 total cost %`)

combined_df <- bind_rows(df1, df2)  %>% 
  mutate(fixed_fee = if_else(`cc1 lcu fee` == `cc2 lcu fee`, 
                             `cc2 total cost %` - `cc2 fx margin`, 0)) %>% 
  mutate(variable_fee = if_else(`cc1 lcu fee` == `cc2 lcu fee`, 
                                `cc2 fx margin`, `cc2 total cost %`))

df <- combined_df %>% 
  mutate(period = yq(period)) %>% 
  mutate(year = year(period)) %>% 
  mutate(firm_category = ifelse(firm_type %in% c('Bank', 'Bank / Money Transfer Operator', 'Bank/Post office'), 'Bank',
                                ifelse(firm_type %in% c('Money Transfer Operator', 'Money Transfer Operator / Building Society', 
                                                        'Money Transfer Operator / Post office'), 'Money Transfer Operator',
                                       ifelse(firm_type %in% c('Non-Bank FI'), 'Financial Technology', firm_type)))) %>% 
  group_by(firm_category, year) %>% 
  summarize(mean_fixed_fee = mean(fixed_fee, na.rm = TRUE),
            mean_variable_fee = mean(variable_fee, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(firm_category %in% c('Bank', 'Money Transfer Operator'))#, 'Financial Technology')) 


mt <- df %>% 
  filter(year == max(year)) %>% 
  mutate(mean_fixed_fee = mean_fixed_fee/100 * 500) %>% 
  mutate(mean_variable_fee = mean_variable_fee/100 * 500)

mt_fixed <- mt %>% 
              select(firm_category, year, mean_fixed_fee) %>% 
            rename(fee = mean_fixed_fee)
mt_fixed['type'] = 'Fixed'

mt_variable <- mt %>% 
  select(firm_category, year, mean_variable_fee) %>% 
      rename(fee = mean_variable_fee )
mt_variable['type'] = 'Variable'

mt <- rbind(mt_variable, mt_fixed)

### https://etherscan.io/tx/0x49b043032a803b1f97e54d68435117714dab5db70e903d6b763ba51b29cab2df
### 1.46 * 2 + 2.02
# 5.04 = (1.46 * 2 + 2.02)  + .0508 / 100 * 500
# .43 = (.1 * 2 + .13) + .0508 / 100 * 500

#Exchange on-ramp costs $4.77 compared to  28.01 for banks and 19.04 for Money Transfer operators

# .18 + .168 * 2 = .52
mt <- rbind(mt, c('DeFi (L2)\nStand-alone on-ramp', '2022', 0.52, 'Fixed'))
mt <- rbind(mt, c('DeFi (L2)\nStand-alone on-ramp', '2022', 10.25, 'Variable'))

# .18 + .168 * 2 + 4
mt <- rbind(mt, c('DeFi (L2)\nExchange on-ramp', '2022', 4.52, 'Fixed'))
mt <- rbind(mt, c('DeFi (L2)\nExchange on-ramp', '2022', .25, 'Variable'))

p <- ggplot(mt, aes(factor(firm_category, levels = c('Bank', 'Money Transfer Operator', 'DeFi (L2)\nStand-alone on-ramp', 'DeFi (L2)\nExchange on-ramp')), y = as.numeric(fee), 
                    fill = factor(type, levels = c('Fixed', 'Variable')))) +
  geom_bar(position = 'stack', stat='identity') +
  scale_fill_manual(values=c( '#747474', '#A5AFFF' )) +
  labs(y = "Total remmitance fee ($)", x = "", title = '',) +
  guides(fill=guide_legend(title="")) +
  uni_theme +
  theme(plot.caption = element_text(size = 12, color = uni_black, family = 'Inter'))
p

ggsave("cost_of_remmitance_vs_v3_500.png", width = 9, height = 6, dpi = 300)#, dpi = 125)




# -------------------------------
# Plot 6 - 
# -------------------------------

df1 <- read_csv('rpw_dataset_2011_2022_q2.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 lcu fee`, `cc2 lcu fee`, `cc2 fx margin`, `cc2 total cost %`, `speed actual`)

df2 <- read_csv('rpw_dataset_2011_2016.csv') %>% 
  filter(transparent == 'yes') %>% 
  select(period, firm_type, `cc1 lcu fee`, `cc2 lcu fee`, `cc2 fx margin`, `cc2 total cost %`, `speed actual`)


combined_df <- bind_rows(df1, df2) %>% 
  mutate(speed = ifelse(`speed actual` %in% c("1-3 days", "Next day", "2 days"), '1-3 days', `speed actual`)) %>% 
  mutate(period = yq(period)) %>% 
  mutate(year = year(period)) %>% 
  group_by(year, speed) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  filter(speed != 'N/A') %>% 
  group_by(year) %>% 
  mutate(per =  100 *count/sum(count)) %>% 
  ungroup() %>% 
  mutate(speed = factor(speed, levels = c('Less than one hour', 'Same day', '1-3 days', '3-5 days', '6 days or more') ))

p <- ggplot(combined_df, aes(factor(year), y = per, 
                    fill = factor(speed))) +
  geom_bar(stat='identity', position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values=c('#A5AFFF', '#747474',   "#FFA6D1", "#3D3D3D" , "#AD218E")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(y = "Speed of remmitances by count (%)", x = "", title = '') +
  guides(fill=guide_legend(title="")) +
  uni_theme 
p

ggsave("speed_of_remmitance.png", width = 9, height = 6, dpi = 300)#, dpi = 125)


# -------------------------------
# Plot 6 - Remittance vs. Exports Total
# -------------------------------


df <- read_csv('remitance_data.csv') %>% 
  filter(`Migrant remittance inflows (US$ million)` %in% c('Low-and Middle-Income Countries', 'China')) %>% 
  select(-"Remittances as a share of GDP in 2021 (%)")
df <- df %>%
  gather(key = var_name, value = value, 2:ncol(df)) %>% 
  spread(key = names(df)[1],value = 'value') 
df <- df %>% 
  mutate(China = gsub(",","",China)) %>% 
  mutate(`Low-and Middle-Income Countries` = gsub(",","",`Low-and Middle-Income Countries`))

plot_df <- df %>% 
  rename(low_middle = `Low-and Middle-Income Countries`) %>% 
  mutate(value = as.numeric(low_middle)  - as.numeric(China))  %>%  
  mutate(year = year(as.Date(as.character(var_name), format = "%Y"))) %>% 
  filter(!is.na(year)) %>% 
  select(year, value)
plot_df['series'] <- 'Remmitances'
rem_df <- plot_df

df <- read_csv('trade_stats.csv')
data <- c()
country <- "Low & middle income"
country_df <- df %>% filter(`Country Name` == country)
trade_low <- country_df %>% filter(`Series Code` == 'NE.TRD.GNFS.ZS')
gdp_low <- country_df %>% filter(`Series Code` == 'NY.GDP.MKTP.CD')
country_df_china <- df %>% filter(`Country Name` == 'China')
trade_china <- country_df_china %>% filter(`Series Code` == 'NE.TRD.GNFS.ZS')
gdp_china <- country_df_china %>% filter(`Series Code` == 'NY.GDP.MKTP.CD')

years <- c("1999 [YR1999]",	"2000 [YR2000]",	"2001 [YR2001]",	"2002 [YR2002]",	"2003 [YR2003]",	
  "2004 [YR2004]", "2005 [YR2005]",	"2006 [YR2006]",	"2007 [YR2007]",	"2008 [YR2008]",	
  "2009 [YR2009]",	"2010 [YR2010]", "2011 [YR2011]",	"2012 [YR2012]",	"2013 [YR2013]",	
  "2014 [YR2014]", "2015 [YR2015]",	"2016 [YR2016]",	"2017 [YR2017]",	"2018 [YR2018]",	
  "2019 [YR2019]",	"2020 [YR2020]",	"2021 [YR2021]")

data <- c()
data_flat <- c()
for (year in years) {
  year_num <- as.numeric(substr(year, 1, 4))
  low_high <- as.numeric(gdp_low[[year]]) / 1e9
  china <- as.numeric(gdp_china[[year]]) / 1e9
  rem <- rem_df %>% filter(year == year_num)
  
  share_values <- c("Pct", year_num, rem[['value']] / 1e3 * 100 / (low_high - china))
  level_values <- c("Level", year_num, rem[['value']] / 1e3)
  flat <- c(year_num, rem[['value']] / 1e3, rem[['value']] / 1e3 * 100 / (low_high - china))
  if (length(data) == 0) {
    data <- share_values
    data <- rbind(data, level_values)
    data_flat <- flat
  } else {
    data <- rbind(data, share_values)
    data <- rbind(data, level_values)
    data_flat <- rbind(data_flat, flat)
  }
}

data <- as_tibble(data, .name_repair = 'minimal')
colnames(data) <- c("series", "year", 'value')

data_flat <- as_tibble(data_flat, .name_repair = 'minimal')
colnames(data_flat) <- c("year", "level", 'pct')

scale <- .0075

p<-ggplot(data=data_flat, aes(x=year, y = level)) +
  geom_line(aes(color = uni_pink)) +
  geom_line(aes(y=pct/scale), color=uni_black) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Share of GDP ")) +
  labs(x=" ", y="Remmitances", title = "") +
  uni_theme + 
  scale_y_continuous(label = comma) +
  theme(plot.title = element_text(size = 15, color = uni_darkgrey, family = 'Inter'))
p

ggsave("merchandise_vs_remitt.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

