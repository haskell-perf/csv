# csv

Simple comparative benchmarks for CSV parsing libraries.

The "file" benchmarks are IO-based: reading from a file into a list or
vector of rows which is then forced with a deepseq. Conveniently, the
differences between each library are reproducibly distinct.

## Generated data with:

The script generates a file of 1,000 rows, of 20 fields with `[a-zA-z0-9]` and
`[," \r\n\t]`, ensuring that the full range of CSV is tested:

    stack exec generate > in.csv

## file (space)

| Case                                      | Allocated   | Max       | Live      | GCs |
|-------------------------------------------|-------------|-----------|-----------|-----|
| cassava/decode/Vector ByteString          | 8,187,688   | 9,328     | 20,584    |   4 |
| cassava/decode/[ByteString]               | 10,424,808  | 9,256     | 20,392    |   6 |
| sv/Data.Sv/parseDecodeFromFile            | 13,893,912  | 1,191,464 | 1,256,184 |  10 |
| lazy-csv/parseCsv/[ByteString]            | 17,851,008  | 1,121,544 | 1,185,336 |  16 |
| csv-conduit/readCSVFile/[ByteString]      | 27,012,448  | 1,204,504 | 1,244,456 |  25 |
| csv-conduit/readCSVFile/Vector ByteString | 27,973,968  | 1,231,656 | 1,303,896 |  26 |
| csv-conduit/readCSVFile/[String]          | 34,319,272  | 2,828,712 | 4,134,544 |  32 |
| csv/Text.CSV/parseCSVFromFile             | 141,654,888 | 3,376,504 | 8,840,696 | 140 |

<!-- RESULTS -->

## file (time)

|Name||
|---|---|
|cassava/decode/Vector ByteString|1.666 ms|
|cassava/decode/[ByteString]|2.191 ms|
|lazy-csv/parseCsv/[ByteString]|3.034 ms|
|sv/Data.Sv/parseDecodeFromFile|3.838 ms|
|csv-conduit/readCSVFile/[ByteString]|8.016 ms|
|csv-conduit/readCSVFile/Vector ByteString|8.482 ms|
|csv-conduit/readCSVFile/[String]|12.47 ms|
|csv/Text.CSV/parseCSVFromFile|29.56 ms|
