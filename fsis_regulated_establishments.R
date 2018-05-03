library(tidyverse)
library(readxl)
library(ggplot2)
library(ggmap)
library(openintro)
library(Hmisc)

#read file
file <- "MPI_Directory_by_Establishment_Name.xls"
df <- read_excel(file)
rm(file)

#mutate data
#first, I inititialized a dummy variable indicating whether or not an establishment processes meat, poultry, or egg
#second, I created a variable to indicate the overlap.
df$meat <- sapply("Meat", grepl, df$Activities)
df$poultry <- sapply("Poultry", grepl, df$Activities)
df$egg <- sapply("Egg", grepl, df$Activities)
df_2 <- df %>%
  select(company = Company, operation_start = GrantDate, state = State, city = City, zip = Zip, meat, poultry, egg) %>%
  mutate(egg = ifelse(egg == T, 1, 0),
         meat = ifelse(meat == T, 1, 0),
         poultry = ifelse(poultry == T, 1, 0),
         company = as.factor(company),
         state = as.factor(state),
         city = as.factor(city),
         zip = as.factor(zip),
         products = case_when(
           egg == 1 & meat == 1 & poultry == 1 ~ "all",
           egg == 1 & meat == 1 & poultry == 0 ~ "egg and meat",                
           egg == 1 & meat == 0 & poultry == 1 ~ "egg and poultry",
           egg == 1 & meat == 0 & poultry == 0 ~ "egg",
           egg == 0 & meat == 1 & poultry == 0 ~ "meat",
           egg == 0 & meat == 1 & poultry == 1 ~ "meat and poultry",
           egg == 0 & meat == 0 & poultry == 1 ~ "poultry"
         ))
attr(df_2$egg, "dimnames") <- NULL
attr(df_2$meat, "dimnames") <- NULL
attr(df_2$poultry, "dimnames") <- NULL

#generate descriptive statistics -- counting the number of slaughter establishments in each state by product
df_descriptives <- df_2 %>%
  group_by(state) %>%
  filter(!is.na(state)) %>%
  summarise(egg = sum(as.numeric(egg)),
            meat = sum(as.numeric(meat)),
            poultry = sum(as.numeric(poultry))) %>%
  as_tibble

#initialize plotting tibble -- removing missing data from product variable
df_plot <- df_2 %>% filter(!is.na(products))

#bar chart (number of each type of product by state)
state <- ggplot(data = df_plot, aes(x = state))
state + geom_bar(aes(fill = products))

#pie chart (share of establishments producing overlapping categories)
pie <- ggplot(data = df_plot, aes(x = factor(1), fill = factor(products))) + geom_bar(width = 1)
pie + coord_polar("y", start = 0)


#usa map
world <- map_data("world") 
usa <- world %>% filter(region == "USA",
              between(lat, 25, 50),
              between(long, -150, 60))

states <- map_data("state")
states$region <-  state2abbr(capitalize(states$region)) 
names(states)[5] <- "state"
df_plot_states <- df_descriptives
df_plot_states <- merge(states, df_plot_states, by = "state")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

usa_base <- ggplot(data = usa, aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

#map of meat producers
usa_base + 
  geom_polygon(data = df_plot_states, aes(fill = meat), color = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43", space = "Lab",
                      na.value = "grey50", guide = "colourbar") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

#map of poultry producers
usa_base + 
  geom_polygon(data = df_plot_states, aes(fill = poultry), color = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43", space = "Lab",
                      na.value = "grey50", guide = "colourbar") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

#map of egg producers
usa_base + 
  geom_polygon(data = df_plot_states, aes(fill = egg), color = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43", space = "Lab",
                      na.value = "grey50", guide = "colourbar") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

