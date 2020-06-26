# Business Science DS4B 101

A repo to document learning. See also [Thread](https://twitter.com/paulapivat/status/1276415450713210880?s=20) on :bird:

## Week 1 

### Starting a project

- Work with project_name.R in RStudio for best results
- Clear session: RStudio > Session > New Session
- Create an outline using `----` to highlight sections heading in project_name.R
- install multiple packages:

````
multiple_package <- c("package1", "package2", "package3", "package4")
install.packages(multiple_package)
````
- Break workflow into multi-parts: load, import, examine, join, wrangle, insights, write files

### Using RStudio

- Click on data.frame in Environment Pane (same as View(df)) 
- use built-in filter function for any data.frame
- When need to set directory, use `tab` between parentheses (path = "") to navigate directory level

### Short Cuts

- `alt` & `-` makes `<-`
- `ctrl` & `enter` runs the highlighted code in project_name.R

### Tidyverse

- join two columns with different names (`left_join` will leave non-match as `NA` rather than [delete like inner_join](https://dplyr.tidyverse.org/reference/join.html#:~:text=inner_join(),left_join()))

````
left_join(df1, df2, by = c("col1" = "col2"))
````

- `separate()`

````
separate(col = multi-cat_column, 
        into = c('category.1', 'category.2', 'category.3'),
        sep = ' - ',
        remove = TRUE)
````

