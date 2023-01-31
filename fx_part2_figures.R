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

## --------------------------------------------------------
## Marketdepth
## --------------------------------------------------------

df <- read_csv('python_output/md.csv') %>% 
    mutate(time = ymd_hms(dt))
          
low <- 0
high <- 120
p <- ggplot(df, aes(x=time, y = normalized_md)) +
    geom_col(aes(color = color, fill = color)) +
    scale_color_manual(values=c("#FA3492", '#747474')) +
    scale_fill_manual(values=c("#FA3492", '#747474')) +
    scale_x_datetime(breaks = '1 days', labels = date_format("%A")) + 
    scale_y_continuous(limits=c(low,high), oob = rescale_none, breaks = c(0, 20, 40, 60, 80, 100, 120)) +
    labs(y = "Market depth over mean (%)", x = "", title = '') +
  annotate("rect", 
           xmin = ymd_hms("2022-11-07 00:00:00"), 
           xmax = ymd_hms("2022-11-07 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
    annotate("rect", 
             xmin = ymd_hms("2022-11-07 20:00:00"), 
             xmax = ymd_hms("2022-11-08 04:00:00"), 
             ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-08 20:00:00"), 
           xmax = ymd_hms("2022-11-09 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-09 20:00:00"), 
           xmax = ymd_hms("2022-11-10 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-10 20:00:00"), 
           xmax = ymd_hms("2022-11-11 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-11 23:00:00"), 
           xmax = ymd_hms("2022-11-14 00:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
    uni_theme + 
   theme(legend.position = "none") 
p

ggsave("marketdepth_over_time.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

## --------------------------------------------------------
## Volume Normalized
## --------------------------------------------------------


df <- read_csv('python_output/volume_normalized.csv') %>% 
  mutate(time = ymd_hms(dt))

low <- 0
high <- 350
p <- ggplot(df, aes(x=time, y = normalized_rolling)) +
  geom_line() +
  scale_color_manual(values=c( '#747474')) +
  #scale_fill_manual(values=c('#747474')) +
  scale_x_datetime(breaks = '1 days', labels = date_format("%A")) + 
  scale_y_continuous(limits=c(low,high), oob = rescale_none) + #, breaks = c(0, 20, 40, 60, 80, 100, 120)) +
  labs(y = "Volume over mean (%)", x = "", title = '') +
  annotate("rect", 
           xmin = ymd_hms("2022-11-07 00:00:00"), 
           xmax = ymd_hms("2022-11-07 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-07 20:00:00"), 
           xmax = ymd_hms("2022-11-08 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-08 20:00:00"), 
           xmax = ymd_hms("2022-11-09 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-09 20:00:00"), 
           xmax = ymd_hms("2022-11-10 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-10 20:00:00"), 
           xmax = ymd_hms("2022-11-11 04:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  annotate("rect", 
           xmin = ymd_hms("2022-11-11 23:00:00"), 
           xmax = ymd_hms("2022-11-14 00:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  uni_theme + 
  theme(legend.position = "none") 
p

ggsave("volume_over_time.png", width = 9, height = 6, dpi = 300)#, dpi = 125)


## --------------------------------------------------------
## Price over time
## --------------------------------------------------------

df <- read_csv('python_output/px.csv')

uniswap <- df %>% select(block_ts, px) %>% 
      rename(Price = px)

uniswap['Asset'] <- "EUROC/USDC"

bfix <- df %>% select(block_ts, Price)
bfix['Asset'] <- "EUR/USD"


df <- bind_rows(uniswap, bfix) %>% 
  mutate(weekend = wday(block_ts, week_start = getOption("lubridate.week.start", 1)) > 5)
  
low <- .95
high <- 1.075
p <- ggplot(df, aes(x=block_ts, y = Price, colour = Asset)) +
  geom_line() +
  scale_color_manual(values=c('#747474', "#FA3492")) +
  #scale_fill_manual(values=c('#747474')) +
  #scale_x_datetime(breaks = '1 days', labels = date_format("%A")) + 
  scale_y_continuous(limits=c(low,high), oob = rescale_none) + #, breaks = c(0, 20, 40, 60, 80, 100, 120)) +
  labs(y = "Price", x = "", title = '', guide = "", color = "") +  
  annotate("rect", 
           xmin = ymd_hms("2022-09-03 00:00:00"), 
           xmax = ymd_hms("2022-09-04 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-09-10 00:00:00"), 
           xmax = ymd_hms("2022-09-11 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-09-17 00:00:00"), 
           xmax = ymd_hms("2022-09-18 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-09-24 00:00:00"), 
           xmax = ymd_hms("2022-09-25 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-10-02 00:00:00"), 
           xmax = ymd_hms("2022-10-03 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-10-08 00:00:00"), 
           xmax = ymd_hms("2022-10-09 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-10-15 00:00:00"), 
           xmax = ymd_hms("2022-10-16 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-10-22 00:00:00"), 
           xmax = ymd_hms("2022-10-23 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-10-29 00:00:00"), 
           xmax = ymd_hms("2022-10-30 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-11-05 00:00:00"), 
           xmax = ymd_hms("2022-11-06 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-11-12 00:00:00"), 
           xmax = ymd_hms("2022-11-13 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-11-19 00:00:00"), 
           xmax = ymd_hms("2022-11-20 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-11-26 00:00:00"), 
           xmax = ymd_hms("2022-11-27 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-12-03 00:00:00"), 
           xmax = ymd_hms("2022-12-04 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) + 
  annotate("rect", 
           xmin = ymd_hms("2022-12-10 00:00:00"), 
           xmax = ymd_hms("2022-12-11 24:00:00"), 
           ymin = low, ymax = high, alpha = .2) +
  uni_theme +
  guides(color = guide_legend(override.aes = list(size = 6)))
 
ggsave("px_over_time.png", width = 9, height = 6, dpi = 300)#, dpi = 125)

