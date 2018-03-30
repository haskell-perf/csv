# csv + blitz

Simple comparative benchmarks for CSV parsing libraries.

This includes a simple CSV parser in src/Blitz.hs which is 4x more
memory efficient than cassava and twice as fast, especially as field
sizes increase.

## Generated data with:

The script generates a file of 1,000 rows, of 20 fields with `[a-zA-z0-9]` and
`[," \r\n\t]`, ensuring that the full range of CSV is tested:

    stack ghc -- Generate.hs -o generate && ./generate > in.csv

## file (space)

| Case                                      | Allocated   | Max       | Live       | GCs |
|-------------------------------------------|-------------|-----------|------------|-----|
| blitz/parseFile                           | 2,640,880   | 392       | 14,368     |   2 |
| cassava/decode/Vector ByteString          | 8,248,240   | 9,200     | 23,584     |   4 |
| cassava/decode/[ByteString]               | 10,733,192  | 9,128     | 23,392     |   6 |
| lazy-csv/parseCsv/[ByteString]            | 17,848,264  | 1,121,216 | 1,191,072  |  16 |
| csv-conduit/readCSVFile/[ByteString]      | 26,996,264  | 1,204,088 | 1,249,992  |  25 |
| csv-conduit/readCSVFile/Vector ByteString | 27,925,016  | 1,199,352 | 1,245,680  |  26 |
| csv-conduit/readCSVFile/[String]          | 34,269,552  | 2,551,104 | 3,811,736  |  32 |
| csv/Text.CSV/parseCSVFromFile             | 136,234,984 | 3,862,416 | 13,219,632 | 133 |
| sv/Data.Sv.Parse/attoparsecText           | 109,833,808 | 5,126,984 | 12,525,480 | 105 |
| sv/Data.Sv.Parse/attoparsecByteString     | 99,504,720  | 7,143,560 | 18,072,344 |  95 |
| sv/Data.Sv.Parse/trifecta                 | 270,308,144 | 9,468,328 | 24,254,240 | 259 |


<!-- RESULTS -->

## file (time)

|Name||
|---|---|
|blitz/parseFile|1.243 ms|
|cassava/decode/Vector ByteString|2.340 ms|
|cassava/decode/[ByteString]|3.174 ms|
|lazy-csv/parseCsv/[ByteString]|4.222 ms|
|csv-conduit/readCSVFile/[ByteString]|11.02 ms|
|csv-conduit/readCSVFile/Vector ByteString|11.90 ms|
|csv-conduit/readCSVFile/[String]|17.39 ms|
|csv/Text.CSV/parseCSVFromFile|41.47 ms|
|sv/Data.Sv.Parse/attoparsecText|59.64 ms|
|sv/Data.Sv.Parse/attoparsecByteString|69.10 ms|
|sv/Data.Sv.Parse/trifecta|107.2 ms|
