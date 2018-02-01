library(rvest)
library(tidyverse)


# Pull data from game summary -------------------------------------------------
game_id <- 2990972

# pull data from a game history
html <- read_html("http://www.boiteajeux.net/jeux/agr/historique.php?id=2990972")

# select first player, first round
t <- html_nodes(html, '.clHistoFonce:nth-child(15) .clHisto:nth-child(2)')
hs <- as.character(xml_contents(t))
clean_html(hs)

# get_round <- function(html, round_num) {
round_num <- 1
  res = c()
  round_html <- c(15:1)[round_num]
  for (i in c(2,3,4)) {
    t <- html_nodes(html,
                    paste0('.clHistoFonce:nth-child(', round_html, ') .clHisto:nth-child(', i, ')'))
    hs <- as.character(xml_contents(t))
    clean <- clean_html(hs)
    res[str_length(clean) != 1] <- clean[str_length(clean) != 1]
  }
  res <- rev(res)
# }



# gets an image name (wood = bois in french)
i <- html_nodes(html, ".clHistoFonce:nth-child(15) img")


games <- c(
  "2097152",
  "2216597",
  "2289311",
  "2612686",
  "2841506",
  "2879452",
  "2884706",
  "2893836",
  "2909986",
  "2930316",
  "2942212",
  "2944181",
  "2944318",
  "2950053",
  "2950286",
  "2962750",
  "2971737",
  "2971938",
  "2977650",
  "2978704",
  "2990972")
big <- read_3p_game(games[1])
for (g in games[-1]) {
  big <- bind_rows(big, read_3p_game(g))
}

d <- purrr::map_df(games, read_3p_game)

read_3p_game <- function(game_id) {
  html <- read_html(paste0("http://www.boiteajeux.net/jeux/agr/partie.php?id=", game_id))

  res <- data.frame(game_id = NULL,
                    player_name = NULL,
                    fields = NULL,
                    pastures = NULL,
                    grain = NULL,
                    vegetables = NULL,
                    sheeps = NULL,
                    wild_boars = NULL,
                    cattles = NULL,
                    unused_spaces = NULL,
                    fenced_stables = NULL,
                    rooms = NULL,
                    family_members = NULL,
                    card_points = NULL,
                    bonus_points = NULL,
                    card_id = NULL)

  for (i in c(2,3,4)) {
    # Get player name
    t <- html_nodes(html,
                    paste0('tr:nth-child(', i, ') th:nth-child(1)'))
    player_name <-  as.character(xml_contents(t))[2]


    # Get cards used
    t <- html_nodes(html,
                    paste0('tr:nth-child(', i, ') span'))
    cards <- as.character(xml_contents(t))
    cards <- strsplit(cards, " =")
    cards <- as.numeric(purrr::map(cards, `[[`, 1))
    if (is_empty(cards)) {
      cards <- NA
      num_cards <- 0
    } else {
      num_cards <- length(cards)
    }

    # Get player score
    t <- html_nodes(html,
                    paste0('.clScore tr:nth-child(', i, ') td'))
    cats <- as.numeric(as.character(xml_contents(t)))
    cats <- cats[!is.na(cats)]

    # Combine
    temp <- data.frame(game_id = game_id,
                       player_name = player_name,
                       fields = cats[1],
                       pastures = cats[2],
                       grain = cats[3],
                       vegetables = cats[4],
                       sheeps = cats[5],
                       wild_boars = cats[6],
                       cattles = cats[7],
                       unused_spaces = cats[8],
                       fenced_stables = cats[9],
                       rooms = cats[10],
                       family_members = cats[11],
                       card_points = cats[12],
                       bonus_points = cats[13],
                       card_id = cards,
                       order = i-1)
    temp$total_score <- rowSums(temp[, 3:15])

    res <- bind_rows(res, temp)
  }
  res$place <- dense_rank(desc(res$total_score))

  return(res)
}

clean_html <- function(html_string) {
  h <- gsub("<.*?>", "", html_string) # trim html tags
  h <- gsub("^[ \t]", "", h) # trim whitespace
  h <- gsub("\n", "", h)
  return(h)
}
