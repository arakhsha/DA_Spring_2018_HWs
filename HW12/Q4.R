library(arules)
intrests = ratings %>% 
  group_by(UserID) %>% 
  arrange(-Rating) %>% 
  slice(1:(n()/2)) %>% 
  select(UserID, MovieID)

trans = as(split(intrests$MovieID, intrests$UserID), "transactions")

reg = c(
        '(.*Castle in the Sky.*',
        '.*Cast Away.*',
        '.*No Country for Old Men.*',
        '.*Memento.*)') %>% 
  paste(collapse = ')|(')

movieList = movies %>% 
  filter(grepl(Title, pattern = reg)) %>% 
  .$MovieID


rules <- apriori(
  trans,
  parameter = list(support = 0.003, confidence = 0.5),
  appearance = list(lhs = movieList, default = 'rhs')
)
rules = subset(rules, lhs %in% as.character(movieList))
rules = sort(rules, by = "support", decreasing = T)

recoms = data.frame(
    MovieID = labels(rhs(rules)) %>% 
      str_sub(2, -2) %>% 
      as.numeric(), 
    rules@quality,
    stringsAsFactors = F
  ) %>% 
  left_join(movies) %>% 
  arrange(-lift) %>% 
  slice(1:10) %>% 
  select(Title, `Average Rating` = avg.rating, support, confidence, lift)
kable(recoms, caption = "Highest Recommended Movies")
