### The Diamond Challenge  
## Coder: Nick Kasper 
## Organize and tidy the data from the diamonds challenge and return a summary
##table 

# Step 1: Load Packages ----
## Needed Packages: tidyverse
library(tidyverse)

newDiamonds <- diamonds |> #clean out any extra emtpy cells, or 0s
  filter_out(x == 0 | y == 0 | z == 0) |>
  drop_na() |>
  select(cut, x, y, z)

stackDiamonds <- newDiamonds |> #put the x, y, y, columns into variable 
  pivot_longer( #keep the numbers under values
    cols = c(x, y, z), 
    names_to = "Variable",
    values_to = "value"
  )

cutSummary <- stackDiamonds |> #use the data to get all the summary numbers 
  group_by(cut, Variable) |>
  summarize(
    count = length(value), 
    min = round(min(value), 3), 
    Q1 = round(quantile(value, 0.25), 3), 
    Median = round(median(value), 3), 
    Q3 = round(quantile(value, 0.75), 3), 
    Max = round(max(value), 3), 
    MAD = round(mad(value), 3), 
    Mean = round(mean(value), 3), 
    SD = round(sd(value), 3), 
  ) |>
arrange(Variable, cut)

