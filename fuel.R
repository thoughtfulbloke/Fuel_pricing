library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggthemes)
MBIE <- read_csv("weekly_table.csv")

filexl <- "fuel price data.xlsx"
sheets <- excel_sheets(filexl)

exldt <- function(x,y = filexl){
  firstread <- read_xlsx(y, sheet=x)
  datablock <- firstread[,1:7]
  datablock$region <- x
  return(datablock)
}

allsheets <- lapply(sheets[2:length(sheets)], exldt)
sheets <- bind_rows(allsheets)
names(sheets) <- c("company", "x98", "x9596" , "x91" , "xDeisel" , "xLPG" , "Date" , "region" , "x98b")
reorgs <- sheets %>% mutate(x98 = ifelse(is.na(x98), x98b,x98)) %>% select(-x98b) %>%
  gather(Product, Price, x98:xLPG) %>%
  mutate(Product = gsub("^x", "", Product),
         Price = as.numeric(Price)) %>%
  filter(!is.na(Price)) %>%
  mutate(Price = round(Price,2),
         dyr = ifelse(wday(Date, label=TRUE) != "Fri", NA, year(Date)),
         dmn = ifelse(wday(Date, label=TRUE) != "Fri", NA, month(Date)),
         ddy = ifelse(wday(Date, label=TRUE) != "Fri", NA, day(Date))) %>%
  arrange(Date) %>%
  fill(dyr, .direction = "up") %>%
  fill(dmn, .direction = "up") %>%
  fill(ddy, .direction = "up") %>%
  mutate(Week_ending_Friday= as.Date(paste(dyr,dmn,ddy, sep="-"))) %>%
  left_join(MBIE, by="Week_ending_Friday")
  
write.csv(reorgs, "price_and_MBIE.csv", row.names = FALSE)     

table(reorgs$company)
reorgs %>% filter(Product == "91", region=="Auckland") %>%
  mutate(tax = case_when(
    Date >= as.Date("2018-06-20") & Date <= as.Date("2018-06-28") ~ "pre",
    Date >= as.Date("2018-07-03") & Date <= as.Date("2018-07-11") ~ "post",
    TRUE ~ "Ignore"
  )) %>% 
  filter(tax != "Ignore") %>%
  group_by(tax) %>%
  summarise(mp = mean(Price)) %>%
  mutate(difference = mp - mp[1])
#tax is indees 0.11 so to get overall pattern will subtract 0.11 from Auck from June

reorgs %>% filter(Product == "91") %>%
  group_by(region) %>%
  mutate(competition = ifelse(sum(company == "GULL") > 0, "GULL region", "non-GULL region"),
         competition = ifelse(region == "Auckland", "Auckland pre-tax", competition),
         competition = ifelse(region == "Auckland" & Date >= as.Date("2018-07-01"), "Auckland post-tax", competition)) %>%
  ungroup() %>%
  ggplot(aes(x=Date, y=Price, colour=competition)) + 
  geom_smooth() + 
  theme_minimal() + ggtitle("Typical petrol pump price (91) + Dubai crude pattern (black)") +
  xlab("Pricewatch pump price") +
  scale_colour_manual(values = c("blue", "blue", "green", "purple")) +
  geom_smooth(aes(y=Dubai_crude_NZD.p.bbl/50), col="black")






  
preTax <- reorgs %>% filter(Product == "91" & Date >= as.Date("2018-06-20") & Date <= as.Date("2018-06-28")) %>%
  group_by(region) %>%
  mutate(competition = ifelse(sum(company == "GULL") > 0, "GULL non-Auckland", "non-GULL region"),
         competition = ifelse(region == "Auckland", "Auckland", competition)) %>%
  ungroup() %>% group_by(competition) %>% summarise(baseline = mean(Price))

dts <- seq.Date(from=as.Date("2018-07-11"), to=as.Date("2018-08-12"), by="day")

how_we_roll <- function(x, df = reorgs){
  end_date = x
  start_date = x - days(7)
  df_out <- df %>% filter(Product == "91" & Date >= start_date & Date <= end_date) %>% group_by(region) %>%
    mutate(competition = ifelse(sum(company == "GULL") > 0, "GULL non-Auckland", "non-GULL region"),
           competition = ifelse(region == "Auckland", "Auckland", competition)) %>%
    ungroup() %>% group_by(competition) %>% summarise(rolling_av = mean(Price))
  df_out$date_to <- x
  return(df_out)
}

bind_rows(lapply(dts, how_we_roll)) %>% inner_join(preTax) %>% 
  mutate(change_in_price = rolling_av - baseline) %>%
  ggplot(aes(x=date_to, y=change_in_price, colour=competition)) + geom_line() + geom_point() +
  theme_tufte() + ggtitle("Change in price over 7 day rolling average\ncompared to average for Jun 20-28") +
  geom_hline(yintercept = 0.11) + annotate("text",x=as.Date("2018-07-30"), y=0.113, label=".11")
