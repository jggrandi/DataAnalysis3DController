<!---
  ```{r, eval=FALSE}

COMMENTS:
  
  boxplot(tapply(dAvgPerTask$T1, dAvgPerTask$Members, summary),xlab="Team members",ylab="Time",main="Task 1")
```


```{r box plots,echo=FALSE}
boxplot(tapply(dAvgPerTask$T1, dAvgPerTask$Members, summary),xlab="Team members",ylab="Time",main="Task 1")

boxplot(tapply(dAvgPerTask$T2, dAvgPerTask$Members, summary),xlab="Team members",ylab="Time",main="Task 2")

boxplot(tapply(dAvgPerTask$T3, dAvgPerTask$Members, summary),xlab="Team members",ylab="Time",main="Task 3")

boxplot(tapply(dAvgPerTask$T4, dAvgPerTask$Members, summary),xlab="Team members",ylab="Time",main="Task 4")
```
Errors per task vs. team members:
  
  ```{r box plots2,echo=FALSE}
boxplot(tapply(dAvgPerTask$E1, dAvgPerTask$Members, summary),xlab="Team members",ylab="Error",main="Task 1")

boxplot(tapply(dAvgPerTask$E2, dAvgPerTask$Members, summary),xlab="Team members",ylab="Error",main="Task 2")

boxplot(tapply(dAvgPerTask$E3, dAvgPerTask$Members, summary),xlab="Team members",ylab="Error",main="Task 3")

boxplot(tapply(dAvgPerTask$E4, dAvgPerTask$Members, summary),xlab="Team members",ylab="Error",main="Task 4")
```
-->
  