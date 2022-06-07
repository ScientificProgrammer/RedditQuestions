# https://www.reddit.com/r/RStudio/comments/usf9sv/how_do_i_superimpose_a_curve_with_normal/

#Define parameters here
random_seed_value <- 1099
num_samples       <- 1520
min_val           <- 8
max_val           <- 12
dat_params        <- data.frame(
                        n_values     = c(5, 30, 76),
                        color_values = c("red", "green", "blue")
                    )

# Define functions here.
GenerateResultSet <- function(p_num_samples, p_min_val, p_max_val) {
    set.seed(random_seed_value)
    rslt <- runif(
        n = p_num_samples,
        min = p_min_val,
        max = p_max_val
    )
    return(rslt)
}

# Preallocate a list to hold the results. For a small data set
# like this one, preallocation doesn't do much for performance.
# However, when your data sets get large, preallocation will
# result in a huge performance boost.
amostra <- vector(mode = "list", length = length(dat_params$n_values))

# Turn off all graphic devices
graphics.off()

# Set up three separate plots in a matrix of 3 x 1.
par(mfrow = c(3, 1))

# Build the data structures and plot them
for (i in 1:length(dat_params$n_values)) {
    amostra[[i]]$n_value     <- dat_params$n_values[i]
    amostra[[i]]$data_set    <- GenerateResultSet(dat_params$n_values[i], min_val, max_val)
    amostra[[i]]$mean_value  <- mean(amostra[[i]]$data_set)

    hist(
        x = amostra[[i]]$data_set,
        freq = FALSE,
        breaks = seq(min_val - 1, max_val + 1),
        ann = TRUE,
        axes = TRUE,
        col = dat_params$color_values[i],
        xlab = "Bin Values",
        ylab = "Frequência relativa",
        main = paste0("Histogram for uniform distribution between ", min_val, " and ", max_val))
        sub = paste0("number of sampled values = ", amostra[[i]]$n_value)

    par(new = TRUE)

    plot(
        density(amostra[[i]]$data_set),
        ann = FALSE,
        axes = FALSE
    )

    par(new = FALSE)
}

# View the structure of the amostra list object
str(amostra)
