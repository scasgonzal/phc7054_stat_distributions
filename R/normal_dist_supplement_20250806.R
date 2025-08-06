# Draft of Exercise Solution for Normal Distribution Q2
# Gabriel Odom
# 2025-08-06

VarianceRatios <- function(sample_num, target_variance) {
  
  out_num <- map_dbl(
    .x = 2:length(sample_num),
    .f = ~{
      shortSamp <- sample_num[1:.x]
      ((.x - 1) * var(shortSamp) / .x) / target_variance - 0.5
    }
  )
  
  tibble(sampSize = 2:length(sample_num), VarRatio = out_num)
  
}

# Test
VarianceRatios(
  sample_num = rnorm(30),
  target_variance = 1
)

simRes_df <- map(
  .x = 1:10000,
  .f = ~{
    x <- rnorm(30)
    VarianceRatios(sample_num = x, target_variance = 1)
  }
) %>% 
  bind_rows(.id = "simRun") %>% 
  mutate(RatioIsPositive = VarRatio > 0) %>% 
  select(-VarRatio) %>% 
  pivot_wider(names_from = sampSize, values_from = RatioIsPositive)

colMeans(simRes_df[,-1])
# With a sample size of 3, we are 50% sure that the Hessian will be PD. With 9
# samples, we are 80% sure. With 19 samples, we are 95% sure.

