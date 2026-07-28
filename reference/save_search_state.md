# Save Search State to Models Folder

Wrapper function that saves search state files to the models folder
instead of project root, keeping all SCM artifacts together

## Usage

``` r
save_search_state(search_state, filename)
```

## Arguments

- search_state:

  Search state object to save

- filename:

  Name of the file (without path)

## Value

Invisible path to saved file
