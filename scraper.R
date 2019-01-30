library(rvest)
library(tidyverse)


# Pull data from game summary -------------------------------------------------
game_id <- "2990972"

game_summary <- read_3p_game(game_id = game_id)


game_history <- map_df(1:14, get_round, html = html, game_summary = game_summary)



html <- read_html(paste0("http://www.boiteajeux.net/jeux/agr/historique.php?id=", game_id))
round <- get_round(html = html, round_num = 1, game_summary = game)









# gets an image name (wood = bois in french)
i <- html_nodes(html, ".clHistoFonce:nth-child(15) img")


game <- read_3p_game("2990972")
round <- get_round(html, 1)




# Read several 3p games ---------------------
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
d <- purrr::map_df(games, read_3p_game)
















