
library(tidyverse)
library(FactoMineR) #PCA
library(brms) #Bayesian Regression Models
library(tidybayes) #brms integration
library(modelr) #data_grid() and seq_range()
library(colorspace) #Colour Scales

#### Import Data ####

data <- read_rds("constituency_data.rds.gz")

#view variables
colnames(data)

#check for NAs.
nas <- nrow(filter(data, if_any(everything(), ~ is.na(.))))
print(str_glue("Rows with NAs: {nas}"))
#this is because NI, Scotland and Wales are excluded above
#because much of the social data is not available in this dataset

#### Leave vote vs lab change ####

#no correlation
correlation <- data %>%
  filter(constituency != "Chorley") %>%
  with(cor(leave_vote, lab_change)) %>%
  round(2)

#colour scheme
leave_remain <- sequential_hcl(palette = "Plasma", n = 5)[c(1,4)] %>%
  setNames(c("Leave", "Remain")) 

#Plot
data %>%
  filter(constituency != "Chorley") %>%
  mutate(leave_remain = ifelse(leave_vote > 50, "Leave", "Remain")) %>%
  ggplot(aes(x = leave_vote, y = lab_change, colour = leave_remain)) +
    geom_hline(yintercept = 0, linetype = "solid", size = 1) +
    geom_vline(xintercept = 50, linetype = "dashed") +
    geom_point() +
    scale_x_continuous(breaks = seq(20, 75, 5), name = " % Leave Vote 2016") +
    scale_y_continuous(breaks = seq(-25, 15, 5), name = "Change in % Labour Vote, 2017-2019") +
    labs(title = "Change in Labour Vote 2017 - 2019 against Leave Vote, by Constituency",
         caption = str_glue("correlation = {correlation}")) +
    scale_colour_manual(values = leave_remain, name = NULL) +
    theme_minimal()

ggsave("graphs/Leave_Labour.svg", device = "svg")


#### PCA of Constituencies ####

#Fit a PCA
constituencies_pca <- data %>%
  select(`0-9`:median_wage) %>%
  PCA(scale.unit = TRUE, ncp = 5, graph = FALSE)

#60% of variance explained in the first two dimensions
summary(constituencies_pca)

#look at variable association
as_tibble(constituencies_pca$var$coord, rownames = "var") %>%
  ggplot() +
  geom_segment(aes(xend = Dim.1, yend = Dim.2), 
               x = 0, y = 0, arrow = arrow(), colour = "grey60") +
  geom_text(aes(x = Dim.1, y = Dim.2, label = var), nudge_y = .05) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(title = "Correlation of Variables") +
  theme_minimal()

#explain variance of the first two dimensions
as_tibble(constituencies_pca$var$contrib, rownames = "var")  %>%
  arrange(-Dim.1)

#Dim.1 is inverse percentage of older people, home owners
#Dim.2 is wage and magerial professional proportion#

#Add dimension values to original dataset
pca_dataset <- data %>%
  add_column(as_tibble(constituencies_pca$ind$coord)) %>%
  mutate(Dim.1 = -Dim.1) %>% #reverse so more older people is positive
  filter(! constituency %in% c("Chorley", "Cities Of London and Westminster"))

#### Plots of PCA ####

#Coloured by Lab-Con Vote
ggplot(pca_dataset, aes(x = Dim.1, y = Dim.2, colour = lab19 - con19)) + 
  geom_point() +
  geom_text(aes(label=constituency), nudge_y = 0.5, check_overlap = TRUE) +
  scale_colour_continuous_diverging(palette = "Blue-Red 2", name = "Vote (%)") +
  labs(x = "Older People and Home Owners",
       y = "Higher income, more Managerial / Professional",
       title = "Constituency PCA, coloured by 2019 Labour-Conservative Vote") +
  theme_minimal() 

ggsave("graphs/constituency_pca_parties.svg", device = "svg")

#Coloured by Brexit Vote
ggplot(pca_dataset, aes(x = Dim.1, y = Dim.2, colour = leave_vote)) + 
  geom_point() +
  geom_text(aes(label=constituency), nudge_y = 0.5, check_overlap = TRUE) +
  scale_colour_continuous_sequential(palette = "Plasma", name = "Leave Vote (%)") +
  labs(x = "Older People and Home Owners",
       y = "Higher income, more Managerial / Professional",
       title = "Constituency PCA, coloured by 2016 Leave Vote") +
  theme_minimal() 

ggsave("graphs/constituency_pca_leave.svg", device = "svg")

#Coloured by Lab vote change
ggplot(pca_dataset, aes(x = Dim.1, y = Dim.2, colour = lab_change)) + 
  geom_point() +
  geom_text(aes(label=constituency), nudge_y = 0.5, check_overlap = TRUE) +
  scale_colour_continuous_sequential(palette = "Burg", rev = F, p1 = 2, p2 = 2,
                                     name = "Change in Labour Vote (%)") +
  labs(x = "Older People and Home Owners",
       y = "Higher income, more Managerial / Professional",
       title = "Constituency PCA, showing change in Labour Vote 2017 - 2019") +
  theme_minimal()

ggsave("graphs/constituency_pca_lab_change.svg", device = "svg")


#### Model ####

#On investigation, Dim.1 and leave vote mask each other. So do Dim.2 only:

#make small dataset
model_data <- select(pca_dataset, leave_vote, Dim.2, lab_change)

#fit model with brms
vote_model <- brm(lab_change ~ 1 + Dim.2 * leave_vote, 
                  family = "student",
                  data = model_data,
                  prior = c(prior(normal(0, 5), class = "b"),
                            prior(exponential(1), class = "sigma"),
                            prior(exponential(1), class = "nu")),
                  chains = 4, cores = 4, iter = 2000, warmup = 500,
                  file = "cache/vote_model.rds")

#Check confidence intervals
summary(vote_model)

## Visualise results

#make categorical version of leave vote for the plot panels
model_data_cut <- model_data %>%
  mutate(leave_cuts = cut_number(leave_vote, n = 4),
         `Leave %` = leave_cuts)

#add in posterior draws
vote_fits <- model_data %>%
  #10 egs of Dim.2
  data_grid(Dim.2 = seq_range(Dim.2, 10), leave_vote) %>%
  #add in the cuts
  left_join(select(model_data_cut, leave_cuts, leave_vote), by = "leave_vote") %>%
  #100 draws per group, based on mean value
  group_by(leave_cuts, Dim.2) %>%
  summarise(leave_vote = mean(leave_vote), .groups = "drop") %>%
  add_linpred_draws(vote_model, ndraws = 100) 

#plot
vote_fits %>%
  rename(`Leave %` = leave_cuts) %>%
  ggplot(aes(x = Dim.2)) +
    geom_line(aes(y = .linpred, group = interaction(.draw, leave_vote)), alpha = .1) +
    geom_point(data = model_data_cut, aes(y = lab_change, colour = leave_vote)) +
    facet_wrap(~`Leave %`, labeller = label_both) +
    scale_colour_continuous_sequential(palette = "Plasma") +
    labs(y = "Change in % Labour Vote 2017 - 2019",
         x = "Higher income and more Managerial / Professional (Dim.2 on PCA)",
         title = "Predicting Labour Vote Change 2017 - 2019, by Quartile",
         subtitle = "Interaction of Leave Vote with a combined measure of Income and Class",
         caption = "Showing 100 posterior draws per panel.") +
    theme_minimal()

ggsave("graphs/dim2_leave_model.svg", device = "svg")
