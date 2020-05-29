results$model_full[[4]] %>% summary(rotate = "none")

173.979 + 91.988

136.759 + 66.698 + 44.507

get_prop_var <- function(object){
	F <- object@Fit$F
	SS <- apply(F^2, 2, sum)
	round(SS / nrow(F), 2)
}

prop_var_rotated <- function(model, rotation){

	if(model@Model$nfact == 1) return(NA)

	F <- model@Fit$F
	rotF <- mirt:::Rotate(F, "varimax")
	round(apply(rotF$loadings^2, 2, sum) / nrow(F), 2)
}

model@Internals

results %>%
	select(factors, itemtype, model_full) %>%
	mutate(
		prop_var = model_full %>% map_dbl(prop_var_rotated, "varimax")
	)

results %>%
	select(factors, itemtype, model_full) %>%
	mutate(
		unrotated = model_full %>% map(get_prop_var),
		rotated = model_full %>% map(prop_var_rotated, "varimax"),
		unrotated_sum = unrotated %>% map_dbl(sum),
		rotated_sum = rotated %>% map_dbl(sum)
	) %>%
	select(-model_full) %>%
	View()


