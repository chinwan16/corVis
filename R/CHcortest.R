

assocMethods


assoc_tibble(cor(iris[,1:4]), measure_type="pearson")


assoc_tibble(iris)

tbl_cor(iris)
tbl_cor(iris, method="spearman")


tbl_dcor(iris)
tbl_mine(iris)
tbl_polycor(iris)

tbl_uncertainty(iris)

tbl_tau(iris, method="A")
tbl_tau(iris, method="B")
tbl_tau(iris, method="C")
tbl_tau(iris, method="W")  # the warning here is not from my code.

tbl_cancor(iris) # see for example, chapter 15 of the book by Hardle, Applied multivariate statistical methods

tbl_easy(iris)
tbl_easy(iris, method="auto")


sym_assoc(tbl_cor(iris[,1:3]))

matrix_assoc(tbl_cor(iris))



calc_assoc(iris)
calc_assoc_by(iris, by="Species")





m <- calc_assoc(iris)
order_assoc(m)  # default method uses average linkage

order_assoc(calc_assoc_by(iris, by="Species")) # default method uses average linkage , on overall measure

order_assoc(calc_assoc_by(iris, by="Species"), method="max_diff") # default method uses average linkage , on max diff


spearman_assoc <- default_assoc()
spearman_assoc$argList[[1]] <- list(method="spearman")

dist_assoc <- default_assoc()
dist_assoc$funName[1] <- "tbl_dcor"


ez_assoc <- tribble(
  ~funName, ~typeX, ~typeY, ~argList,
  "tbl_easy", "numeric", "numeric", list(method="distance"),
  "tbl_easy", "ordered", "ordered", list(method="polychoric"),
  "tbl_cancor",  "factor", "numeric", NULL,
  "tbl_cancor", "other", "other",NULL)

a <- calc_assoc(iris, types=spearman_assoc)
ad <- calc_assoc(iris, types=dist_assoc)

calc_assoc(iris, types=ez_assoc)
pairwise_summary_plot(a)
pairwise_summary_plot(a, fill="measure")
pairwise_summary_plot(a, fill="lightblue")

pairwise_summary_plot(calc_assoc(iris), a,fill="measure_type")

b <- calc_assoc_by(iris, by="Species")
pairwise_summary_plot(b)
pairwise_summary_plot(b, fill="measure_type")
pairwise_summary_plot(b, fill="lightblue")

ap <- calc_assoc(iris[,1:4])
as <- calc_assoc(iris[,1:4], types=spearman_assoc)
ad <- calc_assoc(iris[,1:4], types=spearman_assoc)
pairwise_summary_plot(a <- rbind(ap,as), group_var = "measure_type")
order_assoc(a, group_var="measure_type")



library(palmerpenguins)




r <- calc_assoc(penguins, type=spearman_assoc)
pairwise_summary_plot(r)

# test ordering

penguins1 <- mutate(penguins, species = as.ordered(species), island = as.ordered(island))
#calc_assoc(penguins1[, 1:4]) not sure why it is giving an error

calc_assoc(penguins1[, 1:5], types=ez_assoc)

# would be interesting to add uncertainty as calculated by easy correlation and encorporate in the visualisation.
