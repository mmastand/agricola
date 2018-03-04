# Get turn history from a single game, single round ---------------
get_game_history <- function(game_id, game_summary) {  
  # Pull turn history
  html <- read_html(paste0("http://www.boiteajeux.net/jeux/agr/historique.php?id=", game_id))
  game_history <- map_df(1:14, get_round, html = html, game_summary = game) 
}

# Single round history ------------- 
get_round <- function(html, round_num, game_summary) { 
  round_html <- c(15:1)[round_num]
  res <- list()
  for (i in c(2,3,4)) {
    if (round_num %% 2 == 1) {
      t <- html_nodes(html,
                      paste0('.clHistoFonce:nth-child(', round_html, ') .clHisto:nth-child(', i, ')'))
    } else {
      t <- html_nodes(html,
                      paste0('.clHistoClair:nth-child(', round_html, ') .clHisto:nth-child(', i, ')'))
    }
    hs <- as.character(xml_contents(t))
    clean <- clean_html(hs)
    
    inds <- str_length(clean) != 1 # Actions for that player
    
    # build round output
    res$player_name[inds] <- game_summary$player_names[i - 1]
    res$round_num[inds] <- round_num
    res$action[inds] <- clean[inds]
  }
  
  res <- data.frame(res)
  res$action <- rev(res$action)
  
  res$turn_num <- as.numeric(str_extract(res$action, "^[0-9]+"))
  res$action <- str_replace(res$action, "^[0-9]+", "")
  
  return(res)
}

# Function to get summary of 3p game -------------------
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
  
  players <- get_player_names(html)
  
  for (i in c(2,3,4)) {
    # Get player score
    t <- html_nodes(html,
                    paste0('.clScore tr:nth-child(', i, ') td'))
    cats <- as.numeric(as.character(xml_contents(t)))
    cats <- cats[!is.na(cats)]
    
    # Combine
    temp <- data.frame(game_id = game_id,
                       player_name = players[i - 1],
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
                       order = i - 1)
    temp$total_score <- rowSums(temp[, 3:15])
    
    res <- bind_rows(res, temp)
  }
  res$place <- dense_rank(desc(res$total_score))
  
  return(res)
}

# Clean dirty html tags --------------------
clean_html <- function(html_string) {
  h <- gsub("<.*?>", "", html_string) # trim html tags
  h <- gsub("^[ \t]", "", h) # trim whitespace
  h <- gsub("\n", "", h)
  return(h)
}

# Get player names from a game --------------------
get_player_names <- function(html) {
  player_names <- c(NA, NA, NA)
  for (i in c(2, 3, 4)) {
    t <- html_nodes(html,
                    paste0('tr:nth-child(', i, ') th:nth-child(1)'))
    player_names[i - 1] <-  as.character(xml_contents(t))[2]
  }
  return(player_names)
}

# Functions to parse game history----------------
find_occupations <- function(gh) {
  occs <- str_detect(gh$action, "occupation")
  gh$occ_flag <- as.numeric(occs)
  return(gh)
}

get_occ_name <- function(gh) {
  gh$occ_name <- str_extract(gh$action, "\"(.*?)\"") %>% 
    str_sub(start = 2) %>% 
    str_sub(end = -2)
  gh <- gh %>%
    mutate(occ_name = replace(gh$occ_name, gh$occ_flag != 1, NA))
  return(gh)
}

get_occ_cost <- function(gh) {
  gh$occ_cost <- str_extract(gh$action, "(\\(.*?)\\)") %>% 
    str_sub(start = 2) %>% 
    str_sub(end = -2)
  gh <- gh %>%
    mutate(occ_cost = replace(occ_cost, occ_flag != 1, NA)) %>%
    mutate(occ_cost = replace(occ_cost, occ_cost == "free", "0x")) %>%
    mutate(occ_cost = str_extract(occ_cost, "^[0-9]+"))
  return(gh)
}

parse_occupations <- function(gh) {
  gh <- gh %>%
    find_occupations() %>%
    get_occ_name() %>%
    get_occ_cost()
  return(gh)
}
