### Load clean data, assuming we are in the src directory
clean_yield_data <- read.csv("data_clean/clean_yield_data.csv")

### t-test of Weights by Field type: is there significant difference in 
### tomato yield in the different fields?
t_test_Weight_Field <- with(clean_yield_data, t.test(Weight ~ Field))
t_test_Weight_Field                             

### Write test result to plain text file 
### capture.output(t_test_Weight_Field, file = "results/test_results.txt")

