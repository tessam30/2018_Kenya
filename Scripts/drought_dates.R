
drought <- data.frame(
     Drought = c(1, 2, 3, 4, 5),
       start = c("2004-01-01", "2006-01-01", "2009-01-01", "2011-01-01",
                 "2017-01-01"),
         end = c("2005-01-01", "2007-01-01", "2010-01-01", "2012-01-01",
                 "2018-01-01"))

drought <- drought %>% 
  mutate_at(vars(start, end), as.Date)

