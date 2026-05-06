# Scalar parameter keys for the DGS Succession DAMM-McNIP block

Scalar parameters required by the DGS Succession parser, in the exact
order the parser reads them. Names match the keyword strings in the
upstream `InputParameterParser.cs`. The user-guide parameters
`SoilMoistureA` and `SoilMoistureB` are intentionally omitted: they are
commented out in the parser and not consumed by v2.0. The user-guide
name `ActEnergyDOCDepoly` is misnamed; the parser uses
`ActEnergyDOCUptake`.

## Usage

``` r
.dgsDammMcNIPKeys
```
