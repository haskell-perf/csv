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

| Case                                      | Allocated   | Max       | Live       | GCs |
|-------------------------------------------|-------------|-----------|------------|-----|
| cassava/decode/Vector ByteString          | 8,248,072   | 9,200     | 21,952     |   4 |
| cassava/decode/[ByteString]               | 10,733,024  | 9,128     | 21,760     |   6 |
| lazy-csv/parseCsv/[ByteString]            | 17,850,680  | 1,121,416 | 1,188,072  |  16 |
| csv-conduit/readCSVFile/[ByteString]      | 26,999,080  | 1,203,616 | 1,246,064  |  25 |
| csv-conduit/readCSVFile/Vector ByteString | 27,927,832  | 1,199,048 | 1,241,920  |  26 |
| csv-conduit/readCSVFile/[String]          | 34,273,120  | 2,547,808 | 3,793,160  |  32 |
| csv/Text.CSV/parseCSVFromFile             | 136,246,608 | 3,863,224 | 13,062,488 | 133 |
| sv/Data.Sv.Parse/attoparsecText           | 109,841,544 | 5,216,576 | 12,495,160 | 105 |
| sv/Data.Sv.Parse/attoparsecByteString     | 99,511,624  | 7,149,104 | 18,078,928 |  95 |
| sv/Data.Sv.Parse/trifecta                 | 270,332,368 | 9,278,552 | 23,874,512 | 259 |

<!-- RESULTS -->

## file (time)

|Name||
|---|---|
|cassava/decode/Vector ByteString|1.612 ms|
|cassava/decode/[ByteString]|2.137 ms|
|lazy-csv/parseCsv/[ByteString]|2.815 ms|
|csv-conduit/readCSVFile/[ByteString]|7.661 ms|
|csv-conduit/readCSVFile/Vector ByteString|8.027 ms|
|csv-conduit/readCSVFile/[String]|12.15 ms|
|csv/Text.CSV/parseCSVFromFile|29.21 ms|
|sv/Data.Sv.Parse/attoparsecText|38.08 ms|
|sv/Data.Sv.Parse/attoparsecByteString|44.54 ms|
|sv/Data.Sv.Parse/trifecta|71.36 ms|
