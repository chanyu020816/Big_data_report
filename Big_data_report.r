library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(ggalt)
library(ggfortify)
library(dplyr)
library(sp)
library(tmap)
gapminder_alcohol <- read.csv("C:/Users/chenyu/Downloads/gapminder_alcohol.csv", header = TRUE)
country_to_continent <- read.csv("C:/Users/chenyu/Downloads/continents2.csv", header = TRUE, sep = ";")
alcohol <- gapminder_alcohol %>%
    inner_join(country_to_continent, by = c("country" = "name"))
alcohol_continent <- alcohol %>%
    group_by(region)
View(alcohol_continent)
# heat map function
heat_map <- function(data, title = FALSE){
    data_withoutna <- data %>% drop_na()
alc_to_emp <- data_withoutna[, c(2:5)]
cor_mat <- round(cor(alc_to_emp),2)
cor_table <- melt(cor_mat, value.name = "correlation")
rn_table <- cor_table %>%
    rename(X = Var1,
           Y = Var2)
plot_n_title <- ggplot(data = rn_table, aes(x= X , y= Y , fill = correlation)) + 
  geom_tile() +
  scale_fill_gradient() 
if (is.character(title)) { 
plot_n_title +
  ggtitle(title)}  
else {
    plot_n_title}
}
#continent separate function
sep_region <- function(data, continent) {
    data %>%
        filter(region == continent)
}


ggplot(alcohol_continent) +
 stat_summary(
 mapping = aes(x = region, y = alcconsumption),
 fun.ymin = min,
 fun.ymax = max,
 fun.y = median
 ) +
 coord_flip()

#boxplot
ggplot(alcohol_continent) +
    geom_boxplot(mapping = aes(reorder(alcohol_continent$region, alcohol_continent$alcconsumption, mean, na.rm = TRUE), alcconsumption), na.rm = TRUE)

ggplot(alcohol_continent, aes(region, alcconsumption)) +
    geom_point()

#linear regression, alccon-suirate, by each continent
ggplot(alcohol_continent, aes(alcconsumption, suicideper100th)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~ region)

ggplot(alcohol_continent, aes(alcconsumption, suicideper100th, color = region)) + 
    geom_point()
#N
ggplot(alcohol_continent, aes(x = alcconsumption, y = suicideper100th, color = region)) +
 geom_point() +
 geom_smooth(se = FALSE, method = "lm")

alcohol_asia <- alcohol_continent %>%
    filter(region == "Asia")
heat_map(gapminder_alcohol)
heat_map(alcohol_continent, "Continent") 
heat_map(alcohol_asia, "Asia") 
alcohol_americas <- sep_region(alcohol_continent, "Americas")
heat_map(alcohol_americas, "Americas")
alcohol_europe <- sep_region(alcohol_continent, "Europe")
cor(alcohol_europe$alcconsumption, alcohol_europe$suicideper100th, use = "complete.obs")

alccon_mean_continent <- alcohol_continent %>%
    summarize(alccon_mean = mean(alcconsumption, na.rm = TRUE))
x <- alcohol_continent %>% 
    count(alc_amount)
View(alc_con_am_num)
alc_con_am_num <- x %>% drop_na()
ggplot(data = alc_con_am_num) +
 geom_bar(
 mapping = aes(x = region, fill = alc_amount),
 position = "fill"
 )
boxplot(alcohol_continent$suicideper100th, alcohol_continent$employrate, alcohol_continent$employrate)
# Plot ----------------------------------------------------
alc_Ame <- alcohol_continent[alcohol_continent$region == "Americas", ]  
alc_Eur <- alcohol_continent[alcohol_continent$region == "Europe", ] 
alc_Asi <- alcohol_continent[alcohol_continent$region == "Asia", ]  
alc_Afi <- alcohol_continent[alcohol_continent$region == "Africa", ]
alc_Oce <- alcohol_continent[alcohol_continent$region == "Oceania", ]
ggplot(alcohol_continent, aes(alcconsumption, suicideper100th, col=region)) + 
  geom_point() + 
  geom_encircle(data = alc_Ame, aes(alcconsumption, suicideper100th)) +   
  geom_encircle(data = alc_Eur, aes(alcconsumption, suicideper100th)) + 
  geom_encircle(data = alc_Asi, aes(alcconsumption, suicideper100th)) + 
  geom_encircle(data = alc_Afi, aes(alcconsumption, suicideper100th)) + 
  geom_encircle(data = alc_Oce, aes(alcconsumption, suicideper100th))

#FIXME
alcohol_continent$alc_amount <- ifelse(alcohol_continent$alcconsumption > 6.08, "high", "low")
alcohol_continent$alc_amount <- ifelse(alcohol_continent$alcconsumption > 9.99, "large", FALSE)

View(alcohol_continent)
alcohol_continent %>% 
    count(alc_amount)
boxplot(alcohol_continent$suicideper100th, alcohol_continent$employrate, alcohol_continent$employrate)
?boxplot()

#FIXME
if (alcohol_continent$alc_amount == "high") {
  if(alcohol_continent$alcconsumption > 9.99){
    alcohol_continent$alc_amount = "large"
} 
} else {
  if (alcohol_continent$alcconsumption > 2.47) {
    alcohol_continent$alc_amount = "low"
} else {
    alcohol_continent$alc_amount = "small"
}
}
quantile(alcohol_continent$alcconsumption, probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE, type = 7)

sample(c("go", "stay"), 5, replace = TRUE)
mapdata <- map_data("world")
mapdata <- left_join(map_data, alcohol_continent, by = "country" = )
mapdata1 <- mapdata %>%
                left_join(alcohol_continent, by = c("region" = "country"))
mapdata_final <- mapdata1 %>% filter(!is.na(mapdata1$alcconsumption))
View(mapdata_final)
ggplot(mapdata_final, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = alcconsumption), color = "white", alpha = 2)

# heat map function
data_withoutna <- alcohol_continent %>% drop_na()
alc_to_emp <- data_withoutna[, c(2:5)]
cor_mat <- round(cor(alc_to_emp),2)
cor_table <- melt(cor_mat, value.name = "correlation")
rn_table <- cor_table %>%
    rename(X = Var1,
           Y = Var2)
plot_n_title <- ggplot(data = rn_table, aes(x= X , y= Y , fill = correlation)) + 
  geom_tile() +
  facet_wrap(~ region)  


covid <- read.csv("C:/Users/chenyu/Downloads/owid-covid-data.csv", header = TRUE)
View(covid)
str(covid)
head(covid, 0)
covid_TW <- covid %>%
    filter(location == "Taiwan") %>%
    select(location, total_cases, people_vaccinated, people_fully_vaccinated, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)
View(covid_TW)
