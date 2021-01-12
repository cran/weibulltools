# confint_betabinom remains stable

    # A tibble: 102 x 9
            x  rank   prob lower_bound upper_bound distribution bounds direction
        <dbl> <dbl>  <dbl>       <dbl>       <dbl> <chr>        <chr>  <chr>    
     1 20000.  1.16 0.0831     0.00462       0.334 weibull      two_s~ y        
     2 20707.  1.21 0.0873     0.00529       0.340 weibull      two_s~ y        
     3 21414.  1.25 0.0915     0.00602       0.347 weibull      two_s~ y        
     4 22121.  1.30 0.0958     0.00680       0.353 weibull      two_s~ y        
     5 22813.  1.34 0.1        0.00762       0.359 weibull      two_s~ y        
     6 22828.  1.34 0.100      0.00764       0.359 weibull      two_s~ y        
     7 23535.  1.39 0.104      0.00854       0.366 weibull      two_s~ y        
     8 24242.  1.43 0.109      0.00950       0.372 weibull      two_s~ y        
     9 24949.  1.48 0.113      0.0105        0.378 weibull      two_s~ y        
    10 25657.  1.52 0.118      0.0116        0.385 weibull      two_s~ y        
    # ... with 92 more rows, and 1 more variable: cdf_estimation_method <chr>

---

    # A tibble: 102 x 9
           x  rank   prob lower_bound upper_bound distribution bounds direction
       <dbl> <dbl>  <dbl>       <dbl>       <dbl> <chr>        <chr>  <chr>    
     1  94.0  1.23 0.0128    0.000769      0.0561 weibull3     two_s~ y        
     2  96.0  1.77 0.0203    0.00243       0.0696 weibull3     two_s~ y        
     3  98.0  2.39 0.0288    0.00523       0.0837 weibull3     two_s~ y        
     4 100.   3.07 0.0383    0.00912       0.0983 weibull3     two_s~ y        
     5 102.   3.82 0.0486    0.0140        0.113  weibull3     two_s~ y        
     6 104.   4.61 0.0595    0.0199        0.129  weibull3     two_s~ y        
     7 106.   5.45 0.0711    0.0266        0.145  weibull3     two_s~ y        
     8 108.   6.33 0.0832    0.0340        0.161  weibull3     two_s~ y        
     9 110.   7.24 0.0959    0.0422        0.177  weibull3     two_s~ y        
    10 111.   7.54 0.1       0.0449        0.182  weibull3     two_s~ y        
    # ... with 92 more rows, and 1 more variable: cdf_estimation_method <chr>

---

    # A tibble: 102 x 9
            x  rank   prob lower_bound upper_bound distribution bounds direction
        <dbl> <dbl>  <dbl>       <dbl>       <dbl> <chr>        <chr>  <chr>    
     1 20000.  1.16 0.0831       2748.      56897. weibull      two_s~ x        
     2 20707.  1.21 0.0873       3013.      57814. weibull      two_s~ x        
     3 21414.  1.25 0.0915       3288.      58731. weibull      two_s~ x        
     4 22121.  1.30 0.0958       3573.      59647. weibull      two_s~ x        
     5 22813.  1.34 0.1          3862.      60544. weibull      two_s~ x        
     6 22828.  1.34 0.100        3868.      60563. weibull      two_s~ x        
     7 23535.  1.39 0.104        4172.      61478. weibull      two_s~ x        
     8 24242.  1.43 0.109        4485.      62393. weibull      two_s~ x        
     9 24949.  1.48 0.113        4807.      63307. weibull      two_s~ x        
    10 25657.  1.52 0.118        5136.      64220. weibull      two_s~ x        
    # ... with 92 more rows, and 1 more variable: cdf_estimation_method <chr>

# confint_fisher remains stable

    # A tibble: 102 x 9
            x   prob std_err lower_bound upper_bound distribution bounds direction
        <dbl>  <dbl>   <dbl>       <dbl>       <dbl> <chr>        <chr>  <chr>    
     1 20000. 0.0455   1.16      0.00474       0.367 weibull      two_s~ y        
     2 20707. 0.0486   1.14      0.00532       0.372 weibull      two_s~ y        
     3 21414. 0.0517   1.12      0.00595       0.377 weibull      two_s~ y        
     4 22121. 0.0549   1.09      0.00662       0.382 weibull      two_s~ y        
     5 22828. 0.0582   1.07      0.00734       0.387 weibull      two_s~ y        
     6 23535. 0.0616   1.05      0.00811       0.391 weibull      two_s~ y        
     7 24242. 0.0651   1.03      0.00894       0.396 weibull      two_s~ y        
     8 24949. 0.0687   1.01      0.00982       0.401 weibull      two_s~ y        
     9 25657. 0.0723   0.988     0.0108        0.406 weibull      two_s~ y        
    10 26364. 0.0760   0.970     0.0117        0.411 weibull      two_s~ y        
    # ... with 92 more rows, and 1 more variable: cdf_estimation_method <chr>

---

    # A tibble: 102 x 9
            x   prob std_err lower_bound upper_bound distribution bounds direction
        <dbl>  <dbl>   <dbl>       <dbl>       <dbl> <chr>        <chr>  <chr>    
     1 20000. 0.0455  12177.       6064.      65958. weibull      two_s~ x        
     2 20707. 0.0486  12336.       6442.      66558. weibull      two_s~ x        
     3 21414. 0.0517  12487.       6829.      67151. weibull      two_s~ x        
     4 22121. 0.0549  12631.       7224.      67739. weibull      two_s~ x        
     5 22828. 0.0582  12768.       7628.      68322. weibull      two_s~ x        
     6 23535. 0.0616  12899.       8039.      68901. weibull      two_s~ x        
     7 24242. 0.0651  13023.       8459.      69476. weibull      two_s~ x        
     8 24949. 0.0687  13141.       8886.      70048. weibull      two_s~ x        
     9 25657. 0.0723  13254.       9321.      70617. weibull      two_s~ x        
    10 26364. 0.0760  13361.       9764.      71185. weibull      two_s~ x        
    # ... with 92 more rows, and 1 more variable: cdf_estimation_method <chr>

# delta_method remains stable

     [1] 1.6833107 1.1648293 0.8825416 0.7048333 0.5925678 0.5287449 0.5023443
     [8] 0.5034109 0.5228434 0.5534371

# predict_quantile remains stable

    [1]  14.87862  48.17385 123.56206

---

    [1]  24.87862  58.17385 133.56206

# predict_prob remains stable

    [1] 0.01016299 0.09931657 0.50245502

---

    [1] 0.01016299 0.09931657 0.50245502

