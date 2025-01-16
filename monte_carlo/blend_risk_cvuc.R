
## I ran blend_risk twice -- once correlated and once uncorrelated.  I
## put the results in blend_risk_cor.txt & blend_risk_uncor.txt.  Then
## I stuck them together like this:
##
## echo '     trial        hp             balance  Method' > blend_risk.txt
##
## grep -v trial blend_risk_cor.txt | sed 's/$/   Correlated/'   >> blend_risk.txt
##
## grep -v trial blend_risk_uncor.txt | sed 's/$/  Uncorrelated/' >> blend_risk.txt

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set this to your favorite image viewer, or TRUE to attempt to find one automatically, or FALSE to not load images
imageV <- TRUE

# Try and find an image viewer
if (imageV == TRUE) {
  if (.Platform$OS == "windows") {
    imageV <- "explorer"
  } else {
    for(piv in c("/usr/bin/display", "/usr/bin/pqiv", "/usr/bin/nomacs"))
      if(file.exists(piv))
        imageV <- piv
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
a<-fread('blend_risk.txt')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
successDat <- a %>%
  group_by(hp, Method) %>% 
  summarize(success=100-100*sum(balance<=0)/length(balance), .groups='drop') %>% 
  mutate(hp=as.integer(hp))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(data=successDat) + 
  geom_line(aes(x=hp, y=success, col=Method), linewidth=1) +
  labs(title='Correlated vs Uncorrelated Monte Carlo',
       subtitle='Chances of making it 50 years on 4M withdrawing 100K annually adjusted by inflation on a mix of bonds and S&P',
       x='Percentage of portfolio in S&P vs. 10 Year US Treasury Bonds', 
       y='Chance Of Success')
fname <- "blend_risk_cvuc.png"
ggsave(fname, width=12, height=10)
if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
