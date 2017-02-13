install.packages(c("dplyr", "tidyr", "RSQLite"))

library(dplyr)
library(tidyr)

# DATA WRANGLING CHEAT SHEET http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
# PODGLĄDANIE DANYCH
iris

iris_ <- tbl_df(iris)
iris_

glimpse(iris_)

# PRZEFASONOWYWANIE DANYCH
distinct(iris_)
iris2 <- add_rownames(iris)
iris2

iris_long <- gather(iris2, variable, value, -rowname)
View(iris_long)

iris_wide <- spread(iris_long, variable, value)
arrange(iris_wide, rowname)
arrange(iris_wide, as.numeric(rowname))
arrange(iris_wide, desc(as.numeric(rowname)))

iris_united <- unite(iris2, Petal, Petal.Length, Petal.Width, sep='_')
iris_united
separate(iris_united, Petal, c('Petal.Length', 'Petal.Width'), sep='_')

# WYBIERANIE RZĘDÓW
filter(iris2, Sepal.Width > 3)
distinct(iris2)
sample_n(iris2, 10)
sample_frac(iris2, .1)
sample_frac(iris2, 1, replace = T)
slice(iris2, 3:6)

# WYBIERANIE KOLUMN
iris2
select(iris2, Petal.Length, Species)
select(iris2, -rowname)
select(iris2, pl=Petal.Length, sp=Species)


select(iris2, Sepal.Length:Petal.Length)
select(iris2, contains('.'))
select(iris2, starts_with('Petal'))
select(iris2, ends_with('Length'))
select(iris2, one_of(c('Petal.Length', 'pl')))

# PODSUMOWYWANIE
summarise(iris2, mean_sl = mean(Sepal.Length), mean_sw = mean(Sepal.Width))
summarise_each(iris2, funs(mean, median), starts_with('Sepal'))

smr <- summarise(iris2, 
                 first(Sepal.Length), 
                 last(Sepal.Length),
                 nth(Sepal.Length, 3),
                 IQR(Sepal.Length),
                 min(Sepal.Length),
                 max(Sepal.Length),
                 mean(Sepal.Length),
                 median(Sepal.Length),
                 var(Sepal.Length),
                 sd(Sepal.Length),
                 n(),
                 n_distinct(Species))
View(smr)

# DODAWANIE NOWYCH ZMIENNYCH
mutate(iris2, sepal = Sepal.Length + Sepal.Width)
mutate_each(iris2, funs(sqrt), ends_with('Width'))
transmute(iris2, 
          sepal = Sepal.Length + Sepal.Width,
          petal = Petal.Length + Petal.Width)

transmute(iris2,
          Sepal.Length,
          lag(Sepal.Length),
          lead(Sepal.Length))

transmute(arrange(iris2, Sepal.Length),
          Sepal.Length,
          dense_rank(Sepal.Length),
          min_rank(Sepal.Length),
          percent_rank(Sepal.Length),
          cume_dist(Sepal.Length),
          row_number(),
          ntile(Sepal.Length, 30))

transmute(arrange(iris2, Sepal.Length),
          Sepal.Length,
          cumall(Sepal.Length < 4.6),
          cumany(Sepal.Length > 4.4),
          cummean(Sepal.Length),
          cumprod(Sepal.Length))

transmute(arrange(iris2, Sepal.Length),
          Sepal.Width,
          Sepal.Length,
          min(Sepal.Length, Sepal.Width),
          pmin(Sepal.Length, Sepal.Width))

mutate(sample_frac(iris2, 1), 
       n(),
       Species == 'setosa',
       ecdf(Sepal.Length)(Sepal.Length),
       cume_dist(Sepal.Length))

# GRUPOWANIE
iris_grouped <- group_by(iris2, Species)
iris_grouped

smr_grouped <- summarise(iris_grouped, 
                 first(Sepal.Length), 
                 last(Sepal.Length),
                 nth(Sepal.Length, 3),
                 IQR(Sepal.Length),
                 min(Sepal.Length),
                 max(Sepal.Length),
                 mean(Sepal.Length),
                 median(Sepal.Length),
                 var(Sepal.Length),
                 sd(Sepal.Length),
                 n(),
                 n_distinct(Species))
View(smr_grouped)

# https://cran.r-project.org/web/packages/dplyr/vignettes/window-functions.html
mutate(iris_grouped, Sepal.Length/mean(Sepal.Length))
filter(iris_grouped, Sepal.Length > mean(Sepal.Length))

# OPERACJE DWUTABELOWE
# Joiny dodające nowe kolumny
inner_join(iris2, smr_grouped)
inner_join(rename(iris2, species = Species), smr_grouped)
inner_join(rename(iris2, species = Species), smr_grouped, by = c('species'='Species'))

inner_join(filter(iris2, Species != 'virginica'), 
           filter(smr_grouped, Species != 'setosa'))
left_join(filter(iris2, Species != 'virginica'), 
          filter(smr_grouped, Species != 'setosa'))
right_join(filter(iris2, Species != 'virginica'), 
           filter(smr_grouped, Species != 'setosa'))
full_join(filter(iris2, Species != 'virginica'), 
          filter(smr_grouped, Species != 'setosa'))

# Joiny filtrujące
semi_join(filter(iris2, Species != 'virginica'), 
          filter(smr_grouped, Species != 'setosa'))
anti_join(filter(iris2, Species != 'virginica'), 
          filter(smr_grouped, Species != 'setosa'))

# Operacje na zbiorach
filter(iris2, Sepal.Length < 6) 
filter(iris2, Sepal.Length > 5)
intersect(filter(iris2, Sepal.Length < 6),
          filter(iris2, Sepal.Length > 5))
setdiff(filter(iris2, Sepal.Length < 6),
        filter(iris2, Sepal.Length > 5))
union(filter(iris2, Sepal.Length < 6), 
      filter(iris2, Sepal.Length > 5))

# Sklejanie
bind_rows(iris2, iris2)
bind_cols(iris2,
          transmute(iris2, 
                    sepal = Sepal.Length + Sepal.Width,
                    petal = Petal.Length + Petal.Width))

# !!!!!!!!!!!!!!!!!!!!!!!!!!
# PIPING
# !!!!!!!!!!!!!!!!!!!!!!!!!!

plot(iris)
glm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)

# jest równoważne do:
iris %>% plot()
iris %>% glm(Petal.Length ~ Sepal.Length + Sepal.Width, data = .)

# ale po co to komu?
iris_grouped <- group_by(iris, Species)
iris_summarised <- summarise(iris_grouped, avg = mean(Sepal.Width))
iris_arranged <- arrange(iris_summarised, avg)

arrange(
  summarise(group_by(iris, Species), 
            avg = mean(Sepal.Width)), 
  avg)

iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)

# (NIE)STANDARDOWA EWALUACJA 
# https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
summarise(iris_grouped, mean(Sepal.Length))
summarise_(iris_grouped, ~mean(Sepal.Length))
summarise_(iris_grouped, quote(mean(Sepal.Length)))
summarise_(iris_grouped, 'mean(Sepal.Length)')

# I można z tym (prawie) wszystkim wynieść się na bazę danych!
# https://cran.r-project.org/web/packages/dplyr/vignettes/databases.html
library(RSQLite)
my_db <- src_sqlite("my_db.sqlite3", create = T)

iris_sqlite <- copy_to(my_db, iris, temporary = F)
t <- tbl(my_db, sql("SELECT * FROM iris where Species <> 'setosa'"))
t
collect(t)

filter(iris_sqlite, Species != 'setosa')
collect(filter(iris_sqlite, Species != 'setosa'))
