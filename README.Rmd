# Functions for modelling response errors in precision tasks

Translation of [Paul Bays'](http://www.psychol.cam.ac.uk/people/paul-bays) Matlab functions for modelling precision data into R. Bays' guide to the functions and their usage can be found [here](http://www.paulbays.com/code/JV10/). All the function names are the same as the Matlab functions. The code is also as close as possible to the Matlab code 'under the hood'. The model implemented is described in:

The precision of visual working memory is set by allocation of a shared resource. Bays PM, Catalao RFG & Husain M. *Journal of Vision* 9(10): 7, 1-11 (2009).

To use the functions here the easiest thing to do is have `source("mixture_model_functions.R")` at the top of your script, making sure that file is in your working directory. The individual functions are also provided as seperate files but be aware that they depend on one another; you'll need to have most of them available in your working directory.

**If you use this code please refer to the paper above and Paul Bays' website [bayslab.com](bayslab.com).**

## Overview of functions

Below is a list of all the functions provided here. Most of them exist to be called by other functions. I've given more details on the main functions below. You can also see Paul Bays' guide linked above. Bear in mind there are some differences in outputs.

Function name | Use           | Input             | Output
:------------:|:-------------:|:-----------------:|:------------:
cmean         | Circular mean | Vector of radians | Single value
cstd          | Circular SD   | Vector of radians | Single value
randomvonmises| Random samples from a Von Mises distribution | N, Mu, concentration K | Vector of samples
vonmisespdf   | PDF for Von Mises distribution | Vector of radians, Mu, concentration K | Vector of probabilites
k2sd         | Convert from Von Mises K to circular SD | (Vector of) K | (Vector of) SD
sd2k | Convert circular SD to Von Mises K | (Vector of) SD | (Vector of) K 
wrap | Degrees to radians from -*pi* to *pi* | Vector of degrees | Vector of radians
JV10_error | Calculate bias and precision | See below | See below
JV10_likelihood | Estimate mixture model likelihood | Starting parameters, responses, targets (Non-targets) | Dataframe with liklihood and log-liklihood
JV10_function | The main modelling function | Responses, targets (Non-targets), (Starting values) | List of parameters and log-likelihood
JV10_fit | High level function that runs JV10_function for a range of starting values | See below | Dataframe or list (see below)
JV10_df | Runs JV10_fit over a dataframe | See below | See below
JV10_df_error | Runs JV10_error over a datframe | See below | See below

## Estimating bias and precision

If we want to calculate bias (central tendency) or precision (inverse SD) all you need to do is call

```
JV10_error(X, Tg)
```

where X is a vector of responses and Tg is a vector of targets. Importantly these inputs must be in the range -*pi* to *pi*. To get your degrees to this range use the `wrap` function. `JV10_error` return a `dataframe` with two columns `P` for the precision value and `B` for the bias value. If the vector of target orientations isn't included then it defaults to 0. When calling wrap you should divide the input by `180*pi` if the range of unique values is 0 to 360 degrees. If the range of unique values is 0 to 180 degrees (e.g. bar orientations) then divide by `90*pi`.
  
If you want to apply this function to a dataframe you can use:

```
JV10_df_error(d, id.var = "id", tar.var = "target", res.var = "response")
```
where d is a data.frame, id.var is the columns that identifies your participants, tar.var is the name of the target orientations (in radians) and res.var is the name of the columns for response orientations also in radians. 

## Mixture modelling

To do the mixture modelling described in Bays et al. 2009 you call `JV10_fit(X, Tg, NT, return.ll)`. This function has two required argument and two optional argument. The required arguments are `X` (a vector of response orientations in radians) and `Tg` (a vector of target orientations in radians). `NT` is the orientation(s) on your non-target items and should be a vector/matrix with the same number of rows as X and Tg. Clearly the first rows of X, Tg, and NT should be the single trial, and so on. `return.ll` is a logical argument for whether you want the log-likelihood of the final estimates to be returned. `return.ll` defaults to `TRUE` meaning that `JV10_fit` will return a list containing `B` (a dataframe of parameter estimates) and `LL` the log-likelihood. If `return.ll = FALSE` then you will just get the dataframe `B` back.

### Working with a dataframe

I've written a function JV10_df for cases where you want to apply the mixture modelling to a dataframe.

```
estimates <- JV10_df(df, id.var = "id", tar.var = "target", res.var = "response", nt.vars = NULL)
```
The `df` argument is your dataframe. `id.var` is the name of the column that contains a factor that you want to estimate the parameters for each level of. `tar.var` is the name of your target column and `res.var` is the name of your response variable. Use `nt.vars` if you have non-targets. This can either be a single column or a set of columns, e.g. `c("nt1", "nt2")`. You don't get the logliklihood back here but I may update this at some point.

The file `"test_analysis.R"` shows what using this function would look like and runs in conjunction with the file `"test_data.csv"`.

## Errors

I've tested the code against the Matlab version but if you find any issues let me know (e.d.j.berry14@leeds.ac.uk). Should you use this code for a paper you might want to double-check there haven't been any updates/error-fixes posted here before publishing. 
