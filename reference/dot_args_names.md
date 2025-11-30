# Extract names for variadic block arguments

Helper function for variadic blocks. Processes ...args names to handle
numeric indices vs named arguments.

## Usage

``` r
dot_args_names(x)
```

## Arguments

- x:

  List with names (typically ...args)

## Value

Character vector of names, or NULL if all numeric
