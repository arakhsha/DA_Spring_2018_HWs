#A
movies.sep.gen %>% 
  group_by(Genres) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  hchart(type = 'column', hcaes(x = Genres, y = count), name = 'count') %>% 
  hc_xAxis(title = list(text = "Genre")) %>% 
  hc_yAxis(title = list(text = "Number of Movies"))


#B
library(psych)
library(corrplot)
codedGenres = dummy.code(movies.sep.gen$Genres) %>% 
  cbind(data.frame(MovieID = movies.sep.gen$MovieID)) %>% 
  group_by(MovieID) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  select(-MovieID)
corr_mat = cor(codedGenres, use="pairwise.complete.obs")
corrplot.mixed(corr_mat, tl.col="black", tl.pos = "lt", tl.cex = 0.8,
               number.cex = 0.7, number.digits = 1, lower.col = 'black')

#C
ratings %>% 
  left_join(movies.sep.gen) %>% 
  filter(Genres != '(no genres listed)') %>% 
  group_by(Genres) %>% 
  summarise(avg.rating = mean(Rating, na.rm = T)) %>% 
  arrange(-avg.rating) %>% 
  drop_na() %>% 
  hchart(type = 'column', hcaes(x = Genres, y = avg.rating), name = 'Average Rating') %>% 
  hc_yAxis(title = list(text = 'Average Rating')) %>%
  hc_xAxis(title = list(text = 'Genre')) 



#D
library(RcppRoll)
periodsRating <- ratings %>% 
  left_join(movies.sep.gen) %>% 
  group_by(year) %>% 
  arrange(year) %>% 
  summarise(avgRating = mean(Rating, na.rm = T)) %>% 
  mutate(avgRating = round(roll_mean(avgRating, n = 5, fill = 0, align = 'right'), 2) ) %>% 
  filter(year >= 1920) %>% 
  mutate(start = year - 4) %>% 
  drop_na()
periodsRating %>% 
  hchart(type = 'line', hcaes(x = year, y = avgRating), name = 'Average Rating') %>% 
  hc_yAxis(title = list(text = 'Average Rating')) %>% 
  hc_tooltip(pointFormat = "<b> {point.start} -{point.year}</b> <br/>
            Average Rating: {point.avgRating}")
end = periodsRating %>% top_n(1, avgRating) %>% .$year
paste('Best 5 year period:', end - 4, "-", end)

