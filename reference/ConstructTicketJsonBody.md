# rewrite JSON body in case a waiting ticket is assigned so that the correct json is requested

rewrite JSON body in case a waiting ticket is assigned so that the
correct json is requested

## Usage

``` r
ConstructTicketJsonBody(query, ticket, debug)
```

## Arguments

- query:

  httr2_response

- ticket:

  character hash code

- debug:

  boolean print revised json message with ticket number

## Value

revised json body with ticket hash

## Examples
