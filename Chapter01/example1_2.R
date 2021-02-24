data <- read.table("d-ibm3dx7008.txt", header=TRUE)
attach(data)
N <- dim(data)[1]

sample_mean <- sum(rtn) / N
sample_variance <- sum((rtn - sample_mean)**2) / (N-1)
standard_deviation <- sqrt(sample_variance)
minimum <- min(rtn)
maximum <- max(rtn)

t_statistic <- sample_mean / (standard_deviation / sqrt(N))
t_statistic


log_rtn <- log(1+rtn)
log_sample_mean <- sum(log_rtn) / N
log_sample_variance <- sum((log_rtn - log_sample_mean)**2) / (N)
log_standard_deviation <- sqrt(log_sample_variance)
log_sample_skewness <- sum((log_rtn - log_sample_mean)**3) / (log_standard_deviation**3 * (N))
log_sample_kurtosis <- sum((log_rtn - log_sample_mean)**4) / (log_standard_deviation**4 * (N))
log_excess_sample_kurtosis <- log_sample_kurtosis - 3
minimum <- min(log_rtn)
maximum <- max(log_rtn)

log_skewness_test_statistic <- log_sample_skewness / sqrt(6/N)
log_kurtosis_test_statistic <- (log_sample_kurtosis - 3) / sqrt(24/N)
JB_statistic <- log_skewness_test_statistic**2 + log_kurtosis_test_statistic**2
JB_statistic