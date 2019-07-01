library(tidyverse)
library(readxl)
library(modelr)

shirt_data <- read_excel("data/kawhi.xlsx")

######

shirt_data_datetime <- shirt_data %>% 
  mutate(time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M")) %>% 
  select(-date)

ggplot(shirt_data_datetime) +
  geom_bar(aes(x = factor(size, levels = c("S", "M", "L", "XL", "XXL")))) +
  xlab("size") +
  theme_light()

ggplot(shirt_data_datetime) +
  geom_bar(aes(x = slogan)) +
  theme_light()

ggplot(shirt_data_datetime) +
  geom_bar(aes(x = color)) +
  theme_light()

ggplot(shirt_data_datetime) +
  geom_boxplot(aes(x = factor(size, levels = c("S", "M", "L", "XL", "XXL")), y = price)) +
  labs(x = "size") +
  facet_wrap(~ slogan, nrow = 2)

ggplot(shirt_data_datetime) +
  geom_boxplot(aes(x = factor(size, levels = c("S", "M", "L", "XL", "XXL")), y = price)) +
  labs(x = "size") +
  facet_wrap(~ color, nrow = 2)


ggplot(filter(shirt_data_datetime, size %in% c("M", "L", "XL"))) +
  geom_point(aes(x = time, y = price)) +
  geom_smooth(aes(x = time, y = price), formula = y ~ x, se = T, method = "lm") +
  facet_grid(vars(size), vars(slogan)) +
  theme_light()

ggplot(shirt_data_datetime) +
  geom_point(aes(x = time, y = price)) +
  geom_smooth(aes(x = time, y = price), formula = y ~ x, se = T, method = "lm") +
  theme_light()

model <- lm(price ~ time + size + slogan + color, shirt_data_datetime)

shirt_data_pred_resid <- shirt_data_datetime %>% 
  filter(size %in% c("M", "L", "XL")) %>% 
  add_predictions(model) %>% 
  add_residuals(model)

rmse <- shirt_data_pred_resid %>% 
  summarize(rmse = sqrt(mean((resid)^2)))
rmse

test_data <- tribble(
  ~time, ~size, ~color, ~slogan,
  mdy_hm("07-01-2019 12:00"), "XL", "black", "fun guy",
  mdy_hm("07-01-2019 12:00"), "XL", "black", "board man",
  mdy_hm("07-01-2019 12:00"), "XL", "white", "fun guy",
  mdy_hm("07-01-2019 12:00"), "XL", "white", "board man",
  mdy_hm("07-01-2019 12:00"), "M", "black", "fun guy",
  mdy_hm("07-01-2019 12:00"), "M", "black", "board man",
  mdy_hm("07-01-2019 12:00"), "M", "white", "fun guy",
  mdy_hm("07-01-2019 12:00"), "M", "white", "board man",
  mdy_hm("07-01-2019 12:00"), "L", "black", "fun guy",
  mdy_hm("07-01-2019 12:00"), "L", "black", "board man",
  mdy_hm("07-01-2019 12:00"), "L", "white", "fun guy",
  mdy_hm("07-01-2019 12:00"), "L", "white", "board man"
)

test_data_pred <- test_data %>% add_predictions(model)

ggplot(filter(shirt_data_datetime, slogan == "board man" & color == "black")) +
  + geom_point(aes(x = time, y = price))

ggplot(filter(shirt_data_datetime, (slogan == "board man" & color == "black") | slogan == "fun guy")) +
  + geom_point(aes(x = time, y = price))

View(test_data_pred)
