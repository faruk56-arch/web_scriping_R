# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(mongolite)


# Function to extract champion details from the provided URL
get_champion_details <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) {
    print(paste("Error reading the champion page:", url, "Error:", e))
    NULL
  })
  
  if (is.null(page)) {
    return(data.frame(
      Name = NA,
      Image_URL = NA,
      Description = NA,
      Role = NA,
      Difficulty = NA,
      #URL = url,
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract champion name
  name_node <- page %>% html_node('div[data-testid="title"]')
  name <- if (!is.null(name_node)) html_text(name_node, trim = TRUE) else 'NA'
  
  # Extract image URL
  img_node <- page %>% html_node('img[data-testid="mediaImage"]')
  img <- if (!is.null(img_node)) html_attr(img_node, 'src') else 'NA'
  
  # Extract description
  description_node <- page %>% html_node('div.richText')
  description <- if (!is.null(description_node)) html_text(description_node, trim = TRUE) else 'NA'
  
  # Extract role
  role_div <- page %>% html_node('div[data-testid="roles"]')
  role <- if (!is.null(role_div)) html_node(role_div, 'p[data-testid="meta-details"]') %>% html_text(trim = TRUE) else 'NA'
  
  # Extract difficulty
  difficulty_div <- page %>% html_node('div[data-testid="difficulty"]')
  difficulty <- if (!is.null(difficulty_div)) html_node(difficulty_div, 'p[data-testid="meta-details"]') %>% html_text(trim = TRUE) else 'NA'
  
  data.frame(
    Name = name,
    Image_URL = img,
    Description = description,
    Role = role,
    Difficulty = difficulty,
    #URL = url,
    stringsAsFactors = FALSE
  )
}

# URL of the champions page
base_url <- 'https://www.leagueoflegends.com/fr-fr/champions/'

# Get the HTML content
webpage <- tryCatch(read_html(base_url), error = function(e) {
  print(paste("Error reading the main page:", e))
  NULL
})

if (is.null(webpage)) {
  stop("Failed to read the main page")
}

# Find all champion links
champion_links <- webpage %>%
  html_nodes('a[role="button"][aria-label]') %>%
  html_attr('href') %>%
  unique()

# Check if any champion links were found
if (length(champion_links) == 0) {
  stop("No champion links found")
}

# Extract details for each champion
champions_data <- bind_rows(lapply(champion_links, function(link) {
  champion_url <- paste0('https://www.leagueoflegends.com', link)
  get_champion_details(champion_url)
}))

# Clean and format the data
champions_data <- champions_data %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct() %>%
  filter(!is.na(Name) & Name != 'NA')

# Print the cleaned data
print(champions_data)

# Connect to MongoDB
mongo_conn <- tryCatch(mongo(collection = "champions", db = "league_of_legends", url = "mongodb://localhost"), error = function(e) {
  print(paste("Error connecting to MongoDB:", e))
  NULL
})

if (is.null(mongo_conn)) {
  stop("Failed to connect to MongoDB")
}

# Insert the cleaned data into MongoDB
tryCatch(
  {
    mongo_conn$insert(champions_data)
    print(mongo_conn$count())
    print("Cleaned champion information has been saved to MongoDB")
  },
  error = function(e) {
    print(paste("Error inserting data into MongoDB:", e))
  }
)
