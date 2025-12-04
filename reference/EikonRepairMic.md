# Often operating Mics are missing from the Eikon api, this function does repair these missing operating Mics based upon an internal list of codes.

Often operating Mics are missing from the Eikon api, this function does
repair these missing operating Mics based upon an internal list of
codes.

## Usage

``` r
EikonRepairMic(Fundamentals_Data)
```

## Arguments

- Fundamentals_Data:

  a data.frame containing at leasts the columns "RDN_EXCHD2" and
  "Operating MIC"

## Value

the corrected data.frame in which the column "Operating MIC" empty
string and NA elements are replaced with an operating MIC based on
RDN_EXCHD2

## Examples

``` r
if (FALSE) { # \dontrun{
DataStream <- Refinitiv::DataStreamConnect(DatastreamUserName = DatastreamUserName,
                       DatastreamPassword = DatastreamPassword)
Stoxx1800Constits <- DataStream$listRequest(instrument = "LDJS180E",
                       datatype = c("RIC", "NAME"), requestDate = "0D")
Eikon <- Refinitiv::EikonConnect()
EikonDataWithFailingOPeratingMics <- EikonGetData(EikonObject = Eikon,
      rics = Stoxx1800Constits$RIC,
     Eikonformulas = c( "RDN_EXCHD2", "TR.OperatingMIC", "TR.CompanyName"))
EikonDataWithRepairedOPeratingMics <- EikonRepairMic(EikonDataWithFailingOPeratingMics)
} # }
```
