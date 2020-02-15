
library(dplyr)
library(scales)

url <- "https://www.ons.gov.uk/generator?uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/averageweeklyearningsingreatbritain/november2019/79abd031&format=csv"


median_growth <- read.csv(url,header=T,skip = 6)

colnames(median_growth) <- gsub("\\.+","_",colnames(median_growth))

median_growth$Year <- unlist(lapply(median_growth$Period,FUN=function(x){substr(x,8,12)}))

summary_median_growth <- median_growth %>% group_by(Year) %>% summarise(average_regular_real=mean(Regular_pay_real_))

# Percentage average across 18 years
percent(mean(summary_median_growth$average_regular_real)/100,accuracy = 0.01)

# Compund effect over 18 years
percent(prod(summary_median_growth$average_regular_real/100+1)-1,accuracy = 0.01)

# Percentage after subprime crsis
percent(mean(summary_median_growth$average_regular_real[as.numeric(summary_median_growth$Year)>=2008])/100,accuracy = 0.01)

# Compund after subprime crisis
percent(prod(summary_median_growth$average_regular_real[as.numeric(summary_median_growth$Year)>=2008]/100+1)-1,accuracy = 0.01)
