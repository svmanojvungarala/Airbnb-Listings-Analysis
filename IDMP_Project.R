# Importing Libraries
library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(modelr)
library(maps)
library(mapdata)
library(party)
library(curl)

# Importing the dataset
df <- read.csv("train.csv")
df$log_price <- exp(df$log_price)
names(df)[2] <- "price"

unique(df$city)

df$state <- ""
for (i in 1:nrow(df)){
  if ( df$city[i] == 'NYC') {
    df$state[i] <- 'new york'
    
  } else if ( df$city[i] == 'Boston') {
    df$state[i] <- 'massachusetts'
    
  } else if ( df$city[i] == 'LA' |  df$city[i] == 'SF') {
    df$state[i] <- 'california'
    
  } else if ( df$city[i] == 'DC') {
    df$state[i] <- 'district of columbia'
    
  } else if ( df$city[i] == 'Chicago') {
    df$state[i] <- 'illinois'
    
  }
  
}

# The data
head(df)

# Count of each Property Type
df %>% count(property_type, sort=TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(property_type,n),y=n,label=n))+
  geom_col(fill='#F18464')+
  coord_flip()+
  geom_text(size=3.5,hjust=-0.1)+
  labs(x='Property Type',y='Count',title="Count of each Property Type")+
  theme_economist() +
  scale_fill_economist()

# Plotting the count of different Room Types

df %>% count(room_type, sort=TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(room_type,n),y=n,label=n))+
  geom_col(fill='#F18464')+
  geom_text(size=4.5,vjust=-0.2)+
  labs(x='Room Type',y='Count',title="Count of each Room Type")+
  theme_economist() +
  scale_fill_economist()

# Plotting the average price of each property type
df %>% 
  group_by(property_type) %>% 
  summarise(MedPrice = median(price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(property_type,MedPrice), y = MedPrice,label=MedPrice,fill=property_type)) +
  geom_col()+
  coord_flip()+
  geom_text(size=4.0,hjust=-0.5)+
  labs(x="Average Price", y= "Property Type", title="Average Price of each Property Type")+
  theme_minimal()


# Plotting the most common Amenities
df$amenities <- substr(df$amenities, start=2, stop=nchar(df$amenities)-1)
df_tz <- unnest_tokens(df, Amenities, amenities, token = 'regex', pattern=",")
df_tz %>%
  count(Amenities, sort=TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(Amenities, n), y=n)) +
  geom_col(fill='#023E62') +
  coord_flip() +
  labs(x="Amenities", y="Count",
       title="Top 10 Common Amenities ") +
  theme_minimal()

# Cancellation Policy in the six Cities
ggplot(df,aes(x=cancellation_policy,fill=cancellation_policy))+
  geom_bar()+
  facet_wrap(~city)+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  labs(x = 'City', y = "No. of Airbnb's with their Cancellation Policy", 
       title = "Cancellation Policy in different Cities" )+
  theme_solarized()

#Instant Booking in the six Cities
df %>% group_by(city) %>%
  count( instant_bookable = factor(instant_bookable)) %>% 
  mutate(pct = prop.table(n))%>%
  ggplot(aes(x=instant_bookable, y= pct, fill = instant_bookable, label = scales::percent(pct))) + 
  geom_bar(position = 'dodge',stat='identity') +
  facet_wrap(~city)+ 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Instantly Bookable (True or False)', y = " No. of Airbnb's that are/are not Instanty Bookable", 
       title = "Instant Booking in different Cities" )+
  theme_minimal()


# No. of Airbnb's
ggplot(df,aes(x=as.factor(city),fill=city))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  labs(x = "City",y = "Count of Airbnb's", title = "No. of Airbnb's")+
  theme_economist() +
  scale_fill_economist()

# Avg Ratings of Airbnb's

plot1 <- ggplot(df,aes(x=city,y=review_scores_rating,color=city))+
  stat_summary(fun="mean")+
  labs(x="City",y="Ratings")+
  theme_minimal()

plot2 <- ggplot(df,aes(x=city,y=review_scores_rating,color=city))+
  geom_boxplot()+
  labs(x="City",y="Ratings")+
  theme_minimal()

fig <- ggarrange(plot1,plot2)
annotate_figure(fig, top = text_grob("c"))

# Bedrooms/Bathrooms

ddf<- df %>% mutate(concated_column = paste(bedrooms, bathrooms, sep = '/'))
ddf<-as.data.frame(table(ddf$concated_column,ddf$city))  %>%
  group_by(Var2) %>% 
  slice_max(order_by = Freq, n = 5) %>% 
  group_by(Var2) %>% slice_max(order_by = Freq, n = 5)

ggplot(ddf, aes(x = Var1, y = Freq,fill=Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  facet_wrap(~Var2)+
  labs(x = 'Type of Accommodations', y = " No. of Airbnb's", 
       title = "Type of Accommodation in different Cities" )+
  theme_minimal()

# Maps

map_df <- map_data("state")
map_data <- left_join(map_df, df, by = c("region"="state"))

states <- map_data("state")
counties <- map_data("county")

# California Map
ca_df <- subset(states, region == "california")
ca_county <- subset(counties, region == "california")
city_ca <- map_data("state",region='california')
df_ca <- filter(df,df$state == 'california')

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

p<-c(0,300,600,900,1200,1500)

ca_base + 
  geom_polygon(data = ca_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ca,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.1)+
  labs(title = "Airbnb's in California (SF & LA) and their Price")+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  theme_minimal()

ca_base + 
  geom_polygon(data = ca_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ca,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.5)+
  xlim(-123, -121.5) + ylim(37.5, 38)+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in San Francisco and their Price")+
  theme_minimal()

ca_base + 
  geom_polygon(data = ca_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ca,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.5)+
  xlim(-119, -117) + ylim(33, 35)+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in Los Angeles and their Price")+
  theme_minimal()

# Boston Map
ma_df <- subset(states, region == "massachusetts")
ma_county <- subset(counties, region == "massachusetts")
city_ma <- map_data("state",region='massachusetts')
df_ma <- filter(df,df$state == 'massachusetts')

ma_base <- ggplot(data = ma_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ma_base <- ggplot(data = ma_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ma_base + 
  geom_polygon(data = ma_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ma,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.1)+
  labs(title = "Airbnb's in Massachusetts (Boston) and their Price")+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  theme_minimal()

ma_base + 
  geom_polygon(data = ma_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ma,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.5)+
  xlim(-71.5, -70.9) + ylim(42.22, 42.5)+
  labs(title = "Airbnb's in Boston and their Price")+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  theme_minimal()

# New York Map
ny_df <- subset(states, region == "new york")
ny_county <- subset(counties, region == "new york")
city_ny <- map_data("state",region='new york')
df_ny <- filter(df,df$state == 'new york')

ny_base <- ggplot(data = ny_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ny_base <- ggplot(data = ny_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ny_base + 
  geom_polygon(data = ny_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ny,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.1)+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in New York and their Price") +
  theme_minimal()

ny_base + 
  geom_polygon(data = ny_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_ny,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.5)+
  xlim(-74.3, -73.5) + ylim(40.5, 41)+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in New York and their Price")+
  theme_minimal()

# Chicago Map
chi_df <- subset(states, region == "illinois")
chi_county <- subset(counties, region == "illinois")
city_chi <- map_data("state",region='illinois')
df_chi <- filter(df,df$state == 'illinois')

chi_base <- ggplot(data = chi_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

chi_base <- ggplot(data = chi_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

chi_base + 
  geom_polygon(data = chi_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_chi,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.1)+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in Illinois (Chicago) and their Price")+
  theme_minimal()

chi_base + 
  geom_polygon(data = chi_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_chi,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.5)+
  xlim(-86.9, -88) + ylim(41.6, 42.2)+
  labs(title="Chicago")+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in Chicago and their Price")+
  theme_minimal()

# DC Map
dc_df <- subset(states, region == "district of columbia")
dc_county <- subset(counties, region == "district of columbia")
city_dc <- map_data("state",region='district of columbia')
df_dc <- filter(df,df$state == 'district of columbia')

dc_base <- ggplot(data = dc_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

dc_base <- ggplot(data = dc_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

dc_base + 
  geom_polygon(data = dc_county,fill='#C7DEE5', color = "black") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data=df_dc,aes(x=longitude, y=latitude,color=price),inherit.aes = FALSE,size=0.5)+
  scale_color_gradientn(limits = c(0,1500),
                        colours=c("navyblue", "#EEB1AA", "darkorange1",'green','pink',"red"),
                        breaks=p, labels=format(p))+
  labs(title = "Airbnb's in DC and their Price")+
  theme_minimal()

# Model
set.seed(3)
df_part <- resample_partition(df,
                              p=c(train=0.6,
                                  valid=0.2,
                                  test=0.2))
step1 <- function(response, predictors, candidates, partition)
{
  rhs <- paste0(paste0(predictors, collapse="+"), "+", candidates)
  formulas <- lapply(paste0(response, "~", rhs), as.formula)
  rmses <- sapply(formulas,
                  function(fm) rmse(lm(fm, data=partition$train),
                                    data=partition$valid))
  names(rmses) <- candidates
  attr(rmses, "best") <- rmses[which.min(rmses)]
  rmses
}
model <- NULL

# Step-1
preds <- "1"
cands <- c("property_type","room_type", "accommodates", "bathrooms",
           "city", "bedrooms", "review_scores_rating")
s1 <- step1("log_price", preds, cands, df_part)

model <- c(model, attr(s1, "best"))
s1

# Step-2
preds <- "room_type"
cands <- c("property_type","accommodates", "bathrooms",
           "city", "bedrooms", "review_scores_rating")
s1 <- step1("log_price", preds, cands, df_part)

model <- c(model, attr(s1, "best"))
s1

# Step-3
preds <- c("room_type", "review_scores_rating")
cands <- c("property_type","accommodates", "bathrooms",
           "city", "bedrooms")
s1 <- step1("log_price", preds, cands, df_part)

model <- c(model, attr(s1, "best"))
s1

# Step-4
preds <- c("room_type", "review_scores_rating", "bedrooms")
cands <- c("property_type","accommodates", "bathrooms",
           "city")
s1 <- step1("log_price", preds, cands, df_part)

model <- c(model, attr(s1, "best"))
s1

# Step-4
preds <- c("room_type", "review_scores_rating", "bedrooms","city")
cands <- c("property_type","accommodates", "bathrooms")
s1 <- step1("log_price", preds, cands, df_part)

model <- c(model, attr(s1, "best"))
s1

# Step-5
preds <- c("room_type", "review_scores_rating", "bedrooms","city","accommodates")
cands <- c("property_type","bathrooms")
s1 <- step1("log_price", preds, cands, df_part)

model <- c(model, attr(s1, "best"))
s1

step_model <- tibble(index=seq_along(model),
                     variable=factor(names(model), levels=names(model)),
                     RMSE=model)

ggplot(step_model, aes(y=RMSE)) +
  geom_point(aes(x=variable)) +
  geom_line(aes(x=index)) +
  labs(title="Stepwise model selection") +
  theme_minimal()

fit_model <- lm(log_price ~ room_type + review_scores_rating + bedrooms + city ,
                data = df)

fit5 <- lm(log_price ~ room_type + review_scores_rating + bedrooms + city ,
           data = df_part$train)
rmse(fit5,df_part$train)
rmse(fit5,df_part$valid)
rmse(fit5,df_part$test)

# Random Forest

rf <- randomForest(log_price ~ room_type + review_scores_rating + bedrooms + city,
                   data = df_part$train, mtry=3,importance = TRUE, na.action = na.omit)

print(rf)

# Step 1 - predicting and evaluating the model on train data
train_pred = predict(rf, newdata = df_part$train)
rmse(rf, df_part$train)

# Step 2 - predicting and evaluating the model on test data
test_pred = predict(rf, newdata = df_part$test)
rmse(rf, df_part$test)

# SVM
library(e1071)

msvm <- svm(log_price ~ bedrooms+city+room_type+review_scores_rating, data = df_part$train)

# Step 1 - predicting and evaluating the model on train data
train_pred = predict(msvm, newdata = df_part$train)
rmse(msvm, df_part$train)

# Step 2 - predicting and evaluating the model on test data
test_pred = predict(msvm, newdata = df_part$test)
rmse(msvm, df_part$test)














cf1 <- cforest(log10(price) ~ as.factor(room_type) + review_scores_rating + bedrooms +
                 as.factor(city) + accommodates + bathrooms + as.factor(property_type),
               data= df_part$train, control=cforest_unbiased(mtry=2,ntree=50))

train_pred = predict(cf1, newdata = df_part$train)
rmse(cf1, df_part$train)
summary(train_pred)


