movies = ratings %>% 
  group_by(MovieID) %>% 
  summarise(avg.rating = mean(Rating)) %>% 
  right_join(movies)

# A
movies %>% 
  top_n(1, wt = avg.rating) %>% 
  select(-MovieID) %>% 
  rename(`Average Rating` = avg.rating) %>% 
  kable(caption = "Highest Rated Movie")

#B
ratings %>% 
  group_by(MovieID) %>% 
  summarise(count = n()) %>% 
  top_n(1, wt = count) %>% 
  left_join(movies) %>% 
  select(-MovieID) %>% 
  rename(`Average Rating` = avg.rating) %>% 
  kable(caption = "Highest Rating Count")

#C
movies %>% 
  top_n(1, wt = -avg.rating) %>% 
  select(-MovieID)%>% 
  rename(`Average Rating` = avg.rating) %>% 
  kable(caption = "lowest Rated Movie")

#D
movies = movies %>% 
  mutate(year = str_extract(Title, "\\([[:digit:]]{4}\\)")) %>% 
  mutate(year = str_replace(year, "\\(", "")) %>% 
  mutate(year = str_replace(year, "\\)", "")) %>% 
  mutate(year = as.numeric(year))

movieCountByYear = movies %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  drop_na() %>% 
  filter(year < 2008)

hchart(movieCountByYear, type = 'line', hcaes(x = year, y = count), name = "Number of Movies") %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "Number of Movies"))


#E
movies.sep.gen = movies %>% 
  separate_rows(Genres, sep = "\\|") %>% 
  filter(Genres != '(no genres listed)')

ratings %>% 
  left_join(movies.sep.gen) %>% 
  group_by(Genres, year) %>% 
  summarise(avg.rating = mean(Rating, na.rm = T)) %>% 
  drop_na() %>% 
  group_by(year) %>% 
  top_n(1, wt = avg.rating) %>% 
  select(year, `Popular Genre` = Genres)%>% 
  arrange(-year) %>% 
  kable(caption = "Popular Genres In Years")

