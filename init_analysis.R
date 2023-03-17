library(dplyr)
library(lubridate)
library(plotly)
library(fable)
library(feasts)
library(tsibble)
raw <- read.csv("./data/TTLCON.csv") 


head(raw)
str(raw)


df <- raw |>
  mutate(date = ymd(DATE)) |>
  select(date, y = TTLCON) |>
  arrange(date)


df <- raw |>
  mutate(date = ymd(DATE),
         year_month = yearmonth(date)) |>
  select(date, year_month, y = TTLCON) |>
  as_tsibble(index = year_month)

str(df)

p <- plot_ly(data = df,
             x = ~ date,
             y = ~ y,
             type = "scatter",
             mode = "line",
             name = "Actual")

p


# Create features
df <- df %>%
  mutate(seg1 = 1:nrow(df),
         seg2 = pmax(0, seg1 - min(seg1[seg1[which(date > as.Date("2007-08-01"))]])),
         seg3 = pmax(0, seg1 - min(seg1[seg1[which(date > as.Date("2011-01-01"))]])),
         month = month(date, label = TRUE))




md1 <- lm(y ~ seg1, data = df)
summary(md1)

df$fit1 <- predict(md1, data = df)


p |> 
  add_lines(x = df$date,
            y = df$fit1,
            line = list(color = "black", dash = "dash"),
            name = "Fit1") |> 
  add_trace(x = mean(df$date),
            y = mean(df$y),
            marker = list(color = "pink", width = 3),
            mode = "marker",
            name = "Mean(X,Y)")


md2 <- lm(y ~ seg1 + seg2 + seg3, data = df)
summary(md2)

df$fit2 <- predict(md2, data = df)



p_fit2 <- p |> 
  add_lines(x = df$date,
            y = df$fit1,
            line = list(color = "black", dash = "dash"),
            name = "Fit1") |> 
  add_lines(x = df$date,
            y = df$fit2,
            line = list(color = "black", dash = "dash"),
            name = "Fit2") 


md3 <- lm(y ~ seg1 + seg2 + seg3 + month, data = df)
summary(md3)

df$fit3 <- predict(md3, data = df)



p |> 
  add_lines(x = df$date,
            y = df$fit3,
            line = list(color = "black", dash = "dash"),
            name = "Fit3") 


p1 <- df |>
  model(stl = STL(formula = y)) |>
  components() |>
  plot_ly(x = ~ as.Date(year_month),
          y = ~ y,
          type = "scatter",
          mode = "line",
          name = "Actual") |>
  add_lines(x = ~ as.Date(year_month),
            y = ~ trend,
            line = list(color = "black"),
            name = "Trend")



p_seg1 <- plot_ly(data = df,
                  x = ~ as.Date(year_month),
                  y = ~ seg1,
                  type = "scatter",
                  mode = "line",
                  name = "Segment 1")

p_seg2 <- plot_ly(data = df,
                  x = ~ as.Date(year_month),
                  y = ~ seg2,
                  type = "scatter",
                  mode = "line",
                  name = "Segment 2")

p_seg3 <- plot_ly(data = df,
                  x = ~ as.Date(year_month),
                  y = ~ seg3,
                  type = "scatter",
                  mode = "line",
                  name = "Segment 3")

subplot(p1, p_seg1, p_seg2, p_seg3, nrows = 4,
        shareX = TRUE)



subplot(p_fit2, p_seg1, p_seg2, p_seg3, nrows = 4,
        shareX = TRUE)
