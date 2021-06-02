library(dplyr)
library(jsonlite)
library(plotly)

df <- fromJSON("matches.JSON")

likes <- df %>% filter(like != "NULL")
removed <- df %>% filter(block != "NULL")
matches <- df %>% filter(match != "NULL")

# total match percentage--matches / total interactions
match_pct <- round(nrow(matches) / nrow(df) * 100, 2)



### OUTGOING LIKES ###

outgoing_likes <- nrow(likes)

# unrequited likes :(--sent out a like but they didn't like you back (no match)
outgoing_nomatch <- nrow(likes %>% filter(match == "NULL"))
outgoing_nomatch_pct <- round(outgoing_nomatch / outgoing_likes * 100, 2)

# outgoing matches--you liked them first, they liked you back
outgoing_matches <- nrow(likes %>% filter(match != "NULL"))
# alternate: outgoing_matches <- outgoing_likes - outgoing_unrequited
outgoing_matches_pct <- round(outgoing_matches / outgoing_likes * 100, 2)
# alternate: outgoing_matches_pct <- 1 - outgoing_unrequited_pct

outgoing_likes_pct <- round(outgoing_likes / nrow(df) * 100, 2)


### INCOMING LIKES ###

# incoming matches--they liked you first, you liked them back
incoming_matches <- nrow(matches %>% filter(like == "NULL"))
# incoming likes, removed--they liked you, you didn't like them back
incoming_removed <- nrow(removed %>% filter(like == "NULL" & match == "NULL"
                                            & chats == "NULL"))
# total # of incoming likes
incoming_total <- incoming_matches + incoming_removed

# incoming match %
incoming_matches_pct <- round(incoming_matches / incoming_total * 100, 2)

#removed_pct
incoming_removed_pct <- round(incoming_removed / incoming_total * 100, 2)
# alt: incoming_removed_pct <- 1 - incoming_matches_pct

incoming_likes_pct <- round(incoming_total / nrow(df) * 100, 2)


### MATCHES ###
match_total <- nrow(matches)

# match but no messaging
match_nochat <- nrow(matches %>% filter(chats == "NULL"))
match_nochat_pct <- round(match_nochat / match_total * 100, 2)

# match with messaging
match_chat <- match_total - match_nochat
match_chat_pct <- 100 - match_nochat_pct

# match percentages
match_outgoing_pct <- round(outgoing_matches / match_total * 100, 2)
match_incoming_pct <- round(incoming_matches / match_total * 100, 2)



                            ### DATA VIS ###
# df$like[[1]][["timestamp"]] gets timestamp, type: char
  # df$col_name[[col_num]][["inner_col_name"]]


match_times <- unlist(matches$match)
match_times <- match_times[c(TRUE, FALSE)]
match_times <- as.data.frame(match_times)

dates <- as.Date(sapply(match_times, substring, 1, 10))
dates <- sort(dates)

freq <- as.data.frame(table(dates))
freq <- freq %>% 
  mutate(sum = cumsum(Freq))

fig1 <- plot_ly(freq,
                x = ~dates, y = ~sum, type = 'scatter', mode = 'lines') %>% 
  add_trace(
    text = paste("Date:", freq$dates,
                 "<br>Total Matches:", freq$sum),
    showlegend = F,
    line = list(color = 'purple'),
    hoverinfo = "text")
# font <- list(
#   family = "Courier New,
#   size = 18,
#   color = "yellow"
# )
x1 <- list(
  title = "Time",
  #showticklabels = FALSE,
  showgrid = FALSE,
  type = "date",
  tickformat = "%B <br>%Y"
)
y1 <- list(
  title = "Total Matches"
)
fig1 <- fig1 %>%
  layout(xaxis = x1, yaxis = y1,
         title = "Total Matches Over Time",
         plot_bgcolor = "transparent", paper_bgcolor = "transparent")
fig1


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


likes <- df %>% filter(like != "NULL")
matches <- df %>% filter(match != "NULL")

like_times <- unlist(likes$like)
like_times <- as.data.frame(like_times)
like_times <- like_times[like_times$like_times != "like",]
like_times <- as.data.frame(like_times)
like_times <- data.frame(lapply(like_times, function(x) {
  gsub("T", " ", x)
}))

like_times <- like_times[!is.na(strptime(like_times$like_times,
                                         format = "%Y-%m-%d %H:%M:%S")),]
like_times <- sapply(like_times, substring, 12, 13) # good
times <- sort(like_times)
times <- strtoi(times)
freq <- as.data.frame(table(factor(times, levels = 0:23)))

match_times <- unlist(matches$match)
match_times <- match_times[c(TRUE, FALSE)]
match_times <- as.data.frame(match_times)
match_times <- sapply(match_times, substring, 12, 13)
m_times <- strtoi(sort(match_times))
m_freq <- as.data.frame(table(factor(m_times, levels = 0:23)))
freq <- freq %>% mutate(match_freq = m_freq$Freq)
colnames(freq) = c("time_24h", "like_freq", "match_freq")

freq <- freq %>%
  mutate(time = 
           list("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM",
                "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM",
                "6PM", "7PM", "8PM", "9PM", "10PM", "11PM"))

fig2 <- plot_ly(freq,
                x = ~time_24h, y = ~like_freq, name = "Like Frequency",
                type = "scatter", mode = "lines", hoverinfo = "none",
                line = list(color = "violet")) %>%
  add_trace(
    text = paste("Time:", freq$time,
                 "<br>Sent Likes:", freq$like_freq),
    showlegend = F,
    line = list(color = "violet"),
    hoverinfo = "text")

x2 <- list(
  title = "Times of Day",
  showgrid = FALSE,
  ticktext = list("12AM", "1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM",
                  "9AM", "10AM", "11AM", "12PM", "1PM", "2PM", "3PM", "4PM", "5PM",
                  "6PM", "7PM", "8PM", "9PM", "10PM", "11PM"),
  tickvals = freq$time_24h,
  tickmode = "array",
  showspikes = TRUE, # dotted vertical line
  spikesnap = "cursor", # line stays with cursor
  spikemode = "across+toaxis", # vertical line on whole graph instead of to data point
  showline = TRUE,
  showgrid = TRUE,
  spikedash = "solid"
)

y2 <- list(
  title = "Frequency",
  rangemode = "tozero"
)

font <- list(
  # family = "Courier New",
  # size = 18,
  color = "white"
)

fig2 <- fig2 %>%
  add_trace(y = ~match_freq, name = "Match Frequency", mode = "lines",
            text = paste("Time:", freq$time,
                         "<br>Matches:", freq$match_freq),
            line = list(color = "pink"),
            hoverinfo = "text")

fig2 <- fig2 %>%
  layout(xaxis = x2, yaxis = y2, title =
           "Likes & Matches Throughout the Day (based on Hinge server times)",
         font = font, spikedistance = -1, showlegend = TRUE, hovermode = "x",
         plot_bgcolor = "transparent", paper_bgcolor = "transparent")

fig2





#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

x <- c("Likes Sent", "Likes Received", "Matches")
y <- c(outgoing_likes, incoming_total, match_total)

fig3_data <- data.frame(x, y)
fig3_data$x <- factor(fig3_data$x,
                      levels = c("Likes Sent", "Likes Received", "Matches"))

fig3 <- plot_ly(
  fig3_data, x = ~x, y = ~y,
  type = "bar",
  text = y, textposition = "auto",
  marker = list(color = "purple",
                line = list(color = "purple", width = 1.5))
)

font <- list(
  # family = "Courier New",
  # size = 18,
  color = "white"
)

x3 <- list(
  title = ""
)

y3 <- list(
  title = ""
)

fig3 <- fig3 %>% layout(xaxis = x3, yaxis = y3, font = font,
                        title =  "The Numbers")

fig3



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#



match_times <- unlist(matches$match)
match_times <- match_times[c(TRUE, FALSE)]
match_times <- as.data.frame(match_times)

dates <- as.Date(sapply(match_times, substring, 1, 10))
dates <- sort(dates)

freq <- as.data.frame(table(dates))
freq <- freq %>% 
  mutate(sum = cumsum(Freq))

fig4 <- plot_ly(freq,
                x = ~dates, y = ~Freq, type = "bar",
                text = paste("Date:", freq$dates,
                             "<br>Matches that Day:", freq$Freq),
                marker = list(color = "violet"))
x4 <- list(
  title = "Time",
  #showticklabels = FALSE,
  showgrid = FALSE,
  type = "date",
  tickformat = "%B <br>%Y",
  range = c(format(input$date_range[1]), format(input$date_range[2]))
)
y4 <- list(
  title = "Matches on a Day"
)
fig4 <- fig4 %>%
  layout(xaxis = x4, yaxis = y4,
         title = "Total Matches Per Day")#,
         #plot_bgcolor = "transparent", paper_bgcolor = "transparent")
fig4


#                             ### WE MET ###
# met_total <- df %>% filter(we_met != "NULL")
