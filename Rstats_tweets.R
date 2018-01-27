# fetch #rstats mentions

library(rtweet)
library(dplyr)
# tweets_rstats <- search_tweets(q = "#rstats",
#                                 include_rts = FALSE,
#                                 # type = "mixed",
#                                 n = 300
#                                 )
# 
# tweets_rstats <- tweets_rstats %>%
#   distinct()

# saveRDS(tweets_rstats, "tweets_rstats_20180127")

tweets_rstats <- readRDS("tweets_rstats_20180127")

# original ones 

orig_tweets <- tweets_rstats %>% 
  filter(is.na(reply_to_status_id),
         favorite_count > 1) %>%     # at least 1 faves
  select(status_id, screen_name, text, favorite_count, retweet_count) %>%
  distinct()

# bajo las menciones de cada uno de estos individuos
library(purrr)
library(glue)
library(tidyr)
# 
# orig_tweets_mentions <- orig_tweets %>%
#   mutate(status_id = as.numeric(status_id)) %>%
#   group_by(screen_name) %>%
#   mutate(status_id = min(status_id)) %>%    # keep the min so I don't fetch the timeline for the same user multiple times
#   ungroup() %>%
#   distinct(status_id, screen_name) %>%
#   mutate(query = paste0("@", screen_name, " OR ", "to:", screen_name, " OR ", screen_name)) %>%
#   mutate(tweets = pmap(list(q = .$query,
#                             n = 1000,
#                             retryonratelimit = TRUE),
#                        rtweet::search_tweets)) %>%
#   select(tweets) %>%
#   unnest()

# saveRDS(orig_tweets_mentions, "orig_tweets_mentions_20180127")
orig_tweets_mentions <- readRDS("orig_tweets_mentions_20180127")

orig_tweets_mentions <- distinct(orig_tweets_mentions) 

get_replies_chain <- function(id) {
  diff <- 1
  while (diff != 0) {
    id_next <- orig_tweets_mentions %>%
      filter(reply_to_status_id %in% id) %>%
      pull(status_id)
    id_new <- unique(c(id, id_next))
    diff <- length(id_new) - length(id)
    id <- id_new
  }
  orig_tweets_mentions %>% 
    filter(reply_to_status_id %in% id)
}

# retreive replies and count them

replies <- orig_tweets %>% 
  mutate(replies = map(.$status_id, 
                       get_replies_chain)) %>% 
  unnest(replies) %>% 
  select(status_id, screen_name, text, status_id_reply = status_id1) %>% 
  distinct()

replies_count <- replies %>% 
  group_by(status_id) %>% 
  summarise(reply_count = n()) %>% 
  ungroup

tweets_tern <- orig_tweets %>%  
  left_join(replies_count, by = "status_id") %>% 
  mutate(reply_count = coalesce(reply_count, 0L)) %>%    
  select(screen_name, status_id, replies = reply_count, RTs = retweet_count, faves = favorite_count)

# Make the pyramid!

tweets_tern_mean <- tweets_tern %>% 
  summarize(mean_replies = mean(replies),
            mean_rt      = mean(RTs),
            mean_fave    = mean(faves))

library(ggtern)

lines <- data.frame(x = c(1, 0, 0),
                    y = c(0, 1, 0),
                    z = c(0, 0, 1),
                    xend = c(0, 1, 1),
                    yend = c(1, 0, 1),
                    zend = c(1, 1, 0))

pyramid <- ggtern(data = tweets_tern, aes(x = replies, y = RTs, z = faves)) +
  geom_mask() +
  geom_point(color = viridis(5)[1], alpha = 0.3) +
  geom_point(data = tweets_tern_mean,
             aes(mean_replies, mean_rt, mean_fave),
             color = viridis(5)[1], alpha = 0.8, size = 5) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_segment(data = lines,
               aes(x, y, z,
                   xend = xend, yend = yend, zend = zend),
               color = "grey",
               size = .2) +
  theme_showarrows()

tweets_tern_prop <- tweets_tern %>%
  mutate(prop_replies = replies / (replies + faves)) %>%
  arrange(desc(prop_replies)) %>%
  filter(prop_replies > .5)

pyramid +
geom_point(data = tweets_tern_prop[1,], aes(x = replies, y = RTs, z = faves),
           color = viridis(5)[2], alpha = 0.8, size = 5) +
  geom_point(data = tweets_tern_prop[2,], aes(x = replies, y = RTs, z = faves),
             color = viridis(5)[2], alpha = 0.8, size = 5)






# Twitter Tree

for (i in 1:nrow(tweets_tern_prop)) {
  replies_1 <- get_replies_chain(tweets_tern_prop[i,]$status_id) %>%
    distinct(screen_name, text, status_id, reply_to_status_id, favorite_count)
  # https://twitter.com/albz_marocchino/status/954693172805894144

  tweet_0 <- orig_tweets %>%
    filter(status_id == tweets_tern_prop[i,]$status_id) %>%
    select(screen_name, text, favorite_count)
  

# replies_1 <- get_replies_chain(tweets_tern_prop[1,]$status_id) %>% 
#   distinct(screen_name, text, status_id, reply_to_status_id, favorite_count)
# # https://twitter.com/albz_marocchino/status/954693172805894144
# 
# tweet_0 <- orig_tweets %>% 
#   filter(status_id == tweets_tern_prop[1,]$status_id) %>% 
#   select(screen_name, text, favorite_count)



# replies_1 <- get_replies_chain(tweets_tern_prop[2,]$status_id) %>%
#   distinct(screen_name, text, status_id, reply_to_status_id, favorite_count)
# # https://twitter.com/ikashnitsky/status/954757199372398592
# 
# tweet_0 <- orig_tweets %>%
#   filter(status_id == tweets_tern_prop[2,]$status_id) %>%
#   select(screen_name, text, favorite_count)

# replies_1 <- get_replies_chain(tweets_tern_prop[3,]$status_id) %>%
#   distinct(screen_name, text, status_id, reply_to_status_id, favorite_count)
# # https://twitter.com/milos_agathon/status/954753320882720769
# 
# tweet_0 <- orig_tweets %>%
#   filter(status_id == tweets_tern_prop[3,]$status_id) %>%
#   select(screen_name, text, favorite_count)

# replies_1 <- get_replies_chain(tweets_tern_prop[4,]$status_id) %>%
#   distinct(screen_name, text, status_id, reply_to_status_id, favorite_count)
# # https://twitter.com/milos_agathon/status/954753320882720769
# 
# tweet_0 <- orig_tweets %>%
#   filter(status_id == tweets_tern_prop[4,]$status_id) %>%
#   select(screen_name, text, favorite_count)

# replies_1 <- get_replies_chain(tweets_tern_prop[5,]$status_id) %>%
#   distinct(screen_name, text, status_id, reply_to_status_id, favorite_count)
# # https://twitter.com/brenborbon/status/954643386333519872
# 
# tweet_0 <- orig_tweets %>%
#   filter(status_id == tweets_tern_prop[5,]$status_id) %>%
#   select(screen_name, text, favorite_count)

from_text <- replies_1 %>%
  select(reply_to_status_id) %>%
  left_join(replies_1, c("reply_to_status_id" = "status_id")) %>%
  # filter(!is.na(screen_name)) %>%
  select(screen_name, text, favorite_count) %>% 
  mutate(favorite_count = coalesce(favorite_count, 0L))

tweet_0 <- paste0(tweet_0$screen_name, ": ", tweet_0$text, "\nLikes: ", tweet_0$favorite_count)

to_text <- paste0(replies_1$screen_name, ": ", replies_1$text, "\nLikes: ", replies_1$favorite_count)
to_text <- gsub("'", "`", to_text)
from_text <- paste0(from_text$screen_name, ": ", from_text$text, "\nLikes: ", from_text$favorite_count)
from_text <- gsub("'", "`", from_text)

# Create the edges
edges <- tibble::tibble(
  from = from_text,
  to = to_text
) %>%
  mutate(from = ifelse(
    from == "NA: NA\nLikes: 0",
    tweet_0,
    from)
  )

library("ggraph")
library("igraph")
library("ggiraph")

graph <- graph_from_data_frame(edges, directed = TRUE)
V(graph)$tooltip <- V(graph)$name
V(graph)$tooltip <- gsub("'", "`", V(graph)$tooltip)

library(stringr)
V(graph)$size <- str_extract(V(graph)$name, "[0-9*]$")

set.seed(52)
p <- ggraph(graph, layout = "nicely") + 
  geom_edge_link(edge_colour = viridis(5)[3]) + 
  geom_point_interactive(aes(x, y, 
                             tooltip = tooltip,
                             size = size),
                         color = ifelse(V(graph)$name == tweet_0, viridis(5)[2], viridis(5)[1]), 
                         alpha = 0.8) +
                         # , size = 4) 
  scale_size_discrete(range = c(3,12)) +
  theme_void() + 
  theme(legend.position = "none")

print(tweet_0)
print(ggiraph(code = print(p),
        width_svg = 10,
        zoom_max = 4)
)
}