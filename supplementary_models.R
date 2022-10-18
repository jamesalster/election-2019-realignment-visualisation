
source("visualising_2019_realignment.R")

#### Models on relation of PCA to leave_vote and lab_change ####

## Model Leave vote

leave_vote_model <- brm(leave_vote ~ 1 + Dim.1*Dim.2,
                        family = "student",
                        data = pca_dataset,
                        prior = c(prior(normal(0, 5), class = "b"),
                                  prior(exponential(1), class = "sigma"),
                                  prior(exponential(1), class = "nu")),
                        chains = 4, cores = 4, iter = 1000, warmup = 500,
                        file = "cache/leave_vote_model.rds")

summary(leave_vote_model)

#Dim.1 increases (more old people), leave vote increases
#Dim.2 increases (more high qualif/income), leave vote decreases
#as Dim.2 gets larger, (more high qualif/income), the effect of Dim.1 gets weaker

#visualise effect of Dim.1 on leave_vote ~ Dim.1
pca_dataset_cut <- pca_dataset %>%
  mutate(Dim.2_cut = cut_number(Dim.2, )) 

leave_vote_fits <- pca_dataset %>%
  data_grid(Dim.1 = seq_range(Dim.1, 10), Dim.2) %>%
  left_join(select(pca_dataset_cut, Dim.2, Dim.2_cut), by = "Dim.2") %>%
  add_linpred_draws(leave_vote_model, ndraws = 1)
  
leave_vote_fits %>%
  ggplot(aes(x = Dim.1)) +
  geom_point(data = pca_dataset, aes(y = leave_vote), colour = "grey") +
  geom_line(aes(y = .linpred, group = interaction(.draw, Dim.2)), alpha = .2) +
  facet_wrap(~Dim.2_cut) +
  theme_minimal()

#Predictions of leave vote in terms of the dimensions
leave_vote_preds <- pca_dataset %>%
  data_grid(Dim.1 = seq_range(Dim.1, 10), Dim.2 = seq_range(Dim.2, 10)) %>%
  add_epred_draws(leave_vote_model) 

leave_vote_preds %>%
  group_by(Dim.1, Dim.2) %>%
  summarise(.epred = mean(.epred)) %>%
  ggplot(aes(x = Dim.1, y = Dim.2, fill = .epred)) +
  geom_tile() +
  scale_fill_continuous_diverging("Blue-Yellow 2", rev = T, mid = 50)

## Model change in labour vote

lab_change_model <- brm(lab_change ~ 1 + Dim.1*Dim.2,
                        family = "student",
                        data = pca_dataset,
                        prior = c(prior(normal(0, 5), class = "b"),
                                  prior(exponential(1), class = "sigma"),
                                  prior(exponential(1), class = "nu")),
                        chains = 4, cores = 4, iter = 1000, warmup = 500,
                        file = "cache/lab_change_model.rds")

summary(lab_change_model)

#This time both increase
#so Dim.1 increases, more lab vote
#Dim.2 increases, more lab.vote
#As one increases, the effect of the other on labour vote also increases

#Visualise model predictions
lab_change_preds <- pca_dataset %>%
  data_grid(Dim.1 = seq_range(Dim.1, 20), Dim.2 = seq_range(Dim.2, 20)) %>%
  add_epred_draws(lab_change_model, ndraws = 10) 

lab_change_preds %>%
  group_by(Dim.1, Dim.2) %>%
  summarise(.epred = mean(.epred)) %>%
  ggplot() +
  geom_tile(aes(x = Dim.1, y = Dim.2, fill = .epred)) +
  geom_point(data = pca_dataset, aes(x = Dim.1, y= Dim.2),size = 1) +
  geom_point(data = pca_dataset, aes(x = Dim.1, y= Dim.2, colour = lab17 - con17)) +
  scale_fill_continuous_diverging("Blue-Red 2", rev = F, mid = 0, p1 = 3, p2 = 3) +
  scale_colour_continuous_diverging("Blue-Red 2")


#### The final model from visualising_2019_realignment.R, but the other way around ####

#make categorical version vote for the plot panels
model_data_cut <- model_data %>%
  mutate(dim2_cuts = cut_number(Dim.2, n = 4))

vote_fits <- model_data %>%
  #10 egs of Dim.2
  data_grid(leave_vote = seq_range(leave_vote, 10), Dim.2) %>%
  #add in the cuts
  left_join(select(model_data_cut, dim2_cuts, Dim.2), by = "Dim.2") %>%
  add_linpred_draws(vote_model, ndraws = 1) 

vote_fits %>%
  ggplot(aes(x = leave_vote)) +
  geom_line(aes(y = .linpred, group = interaction(.draw, Dim.2)), alpha = .1) +
  geom_point(data = model_data_cut, aes(y = lab_change, colour = Dim.2)) +
  facet_wrap(~dim2_cuts, labeller = label_both) +
  scale_colour_continuous_sequential(palette = "Viridis") +
  labs(y = "Change in % Labour Vote 2017 - 2019",
       x = "% Leave Vote in 2016",
       title = "Predicting Labour Vote Change 2017 - 2019",
       subtitle = "Interaction of Leave Vote with a combined measure of Income and Class") +
  theme_minimal()

#seen this way round: leave voters vote MORE labour where Dim.2 is high (so more educ)
#and less labour where Dim2 is low
#small effect