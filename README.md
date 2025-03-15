# Random stock

For tax reasons, I had to make an index fund manually by buying lots of individual stocks.
Hence I needed a way to pick stocks at random, with a chance proportional to their market capitalization (specifically in the Vanguard FTSE Global All Cap Index Fund).

## Usage

First convert the data to txt format:

```bash
pdftotext -layout data/oeic-interim-long-report.pdf
```

The relevant data starts on line 9259 and ends on line 14546
That's a difference of 5287, so we want 5288 lines
Hence run the command

```bash
lake exe random_stock $( head -n 14546 data/oeic-interim-long-report.txt | tail -n 5288)
```

## Running tests

```bash
lake exe Tests.Tests
lake exe Tests.Tests1
```

## Flaws

- Some data entries are "-" (or some other dash), I guess because the company is worthless? But then the company in the other column can't be parsed either
- Similarly, each country has a row with total holdings, and that row prevents its sibling column from being parsed
- I haven't made the code production-quality. There are a bunch of unused variables, etc
