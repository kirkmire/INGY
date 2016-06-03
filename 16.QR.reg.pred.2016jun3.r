


f <- rq(LL_both$inc~sqrt(LL_both$Height_Total.x), tau=.05)
predict(f,newdata=airq)


f <- rq(Ozone ~ ., data=airquality, tau=1:19/20)
fp <- predict(f, newdata=airq, stepfun = TRUE)
fpr <- rearrange(fp)
plot(fp[[2]],main = "Ozone Prediction")
par(col="red")
lines(fpr[[2]])
legend(.2,20,c("raw","cooked"),lty = c(1,1),col=c("black","red"))
