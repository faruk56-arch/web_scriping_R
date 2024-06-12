library(rvest)
library(dplyr)
library(purrr)

# Function to get Pokémon data from the website
get_pokemon_data <- function(id) {
  url <- paste0("https://www.pokemon.com/fr/pokedex/", id)
  webpage <- read_html(url)
  
  # Debugging prints to inspect HTML nodes
  print(url)
  print(webpage %>% html_nodes(".pokedex-pokemon-pagination-title div"))
  print(webpage %>% html_nodes(".attribute-value:nth-child(2)"))
  print(webpage %>% html_nodes(".attribute-value:nth-child(3)"))
  print(webpage %>% html_nodes(".dtm-type .attribute-value"))
  
  name <- webpage %>% html_node(".pokedex-pokemon-pagination-title div") %>% html_text(trim = TRUE)
  base_experience <- NA # Not available directly on the website
  height <- webpage %>% html_node(".attribute-value:nth-child(2)") %>% html_text(trim = TRUE)
  weight <- webpage %>% html_node(".attribute-value:nth-child(3)") %>% html_text(trim = TRUE)
  order <- id
  types <- webpage %>% html_nodes(".dtm-type .attribute-value") %>% html_text(trim = TRUE)
  type1 <- if (length(types) >= 1) types[1] else NA
  type2 <- if (length(types) >= 2) types[2] else NA
  
  tibble(
    name = name,
    base_experience = base_experience,
    height = height,
    weight = weight,
    order = order,
    type1 = type1,
    type2 = type2
  )
}

# Scrape data for the first 151 Pokémon
pokemon_data_list <- lapply(1:151, get_pokemon_data)

# Filter NULL entries
pokemon_data_list <- keep(pokemon_data_list, ~ !is.null(.x))

# Extract relevant information using map_dfr
pokemon_data <- bind_rows(pokemon_data_list)

# Check scraped data
print(head(pokemon_data))

# Save scraped data to a CSV file
write.csv(pokemon_data, "data/raw_pokemon_data.csv", row.names = FALSE)
