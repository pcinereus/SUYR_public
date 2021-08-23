
wq.list <- with(wq, list(Dt.num = modelr::seq_range(Dt.num, n=100),
                        Region=levels(Region)))
newdata <- wq.gamm3c %>%
    emmeans(~Dt.num, at=wq.list,
            type='response',
            #data=wq.gamm3c$model
            ) %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw()

## Partial residuals
wq.presid <- with(wq.gamm3c$model,
                  data.frame(Dt.num, Mnth))  %>%
    mutate(Pred = predict(wq.gamm3c, exclude='s(reef.alias)', type='link'),
           Resid = wq.gamm3c$resid,
           Partial.obs = exp(Pred + Resid))
  Resid=exp(
    as.vector(predict(wq.gamm3c,  exclude='s(reef.alias)',  type='link')) +
    wq.gamm3c$residuals
  )
)

head(wq.presid)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
  geom_line() +
  geom_point(data=wq.presid, aes(y=Partial.obs)) +
  geom_point(data=wq,  aes(y=NOx, x=date_decimal(Dt.num)),  color='red') +
  scale_x_datetime('') +
  scale_y_continuous(breaks=seq(0,10,by=2), limits=c(0,10))+
  theme_bw() 
