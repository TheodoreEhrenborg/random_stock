# Random stock

## Motivation

## Commands

``` bash
pdftotext -layout data/oeic-interim-long-report.pdf
```


The data I want starts on line 9259 and ends on line 14546
That's a difference of 5287, so I want 5288 lines
Hence I run the command

``` bash
lake exe random_stock $( head -n 14546 data/oeic-interim-long-report.txt | tail -n 5288)
```

## Tests

## Flaws
Some entries are "-" (or some other dash), I guess because the company is worthless?
But then the company in the other column can't be parsed either
Similarly, each country has a row with total holdings, and that row makes its sibling
column unparseable
