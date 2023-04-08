#--------------------(1)
ggplot(data = mtcars, aes(x = hp, y = wt)) +
  geom_point(aes(size = cyl, color = as.factor(am)), 
             alpha = 0.5) +
  labs(title = "Relation of horsepower and weight", 
       x = "horsepower (hp)", 
       y = "weight (wt)", 
       color = "Transmission", 
       size = "Cylinder's count") +
  scale_color_manual(values = c("green", "red"),                    
                     labels = c("Auto", "Mech"))+
  scale_size(name = "Cylinder's count", range = c(5, 15))
#--------------------(2)
ggplot(data = mtcars, aes(x = hp)) + 
  geom_histogram(fill = "brown", 
                 color = "black", 
                 bins = 6) +  
  labs(title = "Gross horsepower", 
       x = "Horsepower", 
       y = "count") + 
  facet_grid(~am, 
             labeller = labeller(am = c("0" = "Automatic", 
                                        "1" = "Mechanic")))
#--------------------(3)
ggplot(sleep, aes(x = group, y = extra, 
                  group = as.factor(group), fill = group)) +
  geom_boxplot() +
  labs(title = "Distribution of 'extra'", 
       x = "groups", 
       y = "extra") + 
  scale_fill_manual(values = c("yellow", "lightblue"))
