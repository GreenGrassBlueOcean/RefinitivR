# Initialize DataStream Connection

Initialize DataStream Connection

## Usage

``` r
DataStreamConnect(DatastreamUserName = NA, DatastreamPassword = NA)
```

## Arguments

- DatastreamUserName:

  Refinitiv DataStream username

- DatastreamPassword:

  Refinitiv DataStream password

## Value

a DataStream R5 object

## Examples

``` r
if (FALSE) { # \dontrun{
DatastreamUserName <- "Your datastream username"
DatastreamPassword <- "Your datastream password"
DataStream <- DataStreamConnect(DatastreamUserName, DatastreamPassword)
} # }
```
