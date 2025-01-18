daDat <- fread('test_stats_rand_norm_std.txt')

gp <- ggplot() + 
  geom_line(data=data.table(x=seq(min(daDat$z_box), max(daDat$z_box), length=1000)) %>% mutate(y=dnorm(x, mean=0, sd=1)), aes(x=x, y=y), col='black', linewidth=15) +
  geom_density(data=daDat, aes(x=z_box), col='red', fill=NA, alpha=0.5, linewidth=1) + 
  geom_density(data=daDat, aes(x=z_probit), col='green', fill=NA, alpha=0.5, linewidth=1) + 
  xlim(c(-3,3)) +
  ylim(c(0,0.4))
print(gp)
