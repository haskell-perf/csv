# csv

Simple comparative benchmarks for CSV parsing libraries.

The "file" benchmarks are IO-based: reading from a file into a list or
vector of rows which is then forced with a deepseq. Conveniently, the
differences between each library are reproducibly distinct.

See the [blitz](https://github.com/haskell-perf/csv/tree/blitz) branch
for a simple CSV parser which is more efficient than any of the below.

## Generated data with:

The script generates a file of 1,000 rows, of 20 fields with `[a-zA-z0-9]` and
`[," \r\n\t]`, ensuring that the full range of CSV is tested:

    stack ghc -- Generate.hs -o generate && ./generate > in.csv

## file (space)

|Case                                      |  Allocated|        Max|       Live|  GCs|
|------------------------------------------|-----------|-----------|-----------|-----|
|cassava/decode/Vector ByteString | 8,248,128 | 9,200 | 23,584 | 4|
|cassava/decode/[ByteString] | 10,733,080 | 9,128 | 23,392 | 6|
|lazy-csv/parseCsv/[ByteString] | 17,850,680 | 1,121,416 | 1,191,336 | 16|
|csv-conduit/readCSVFile/[ByteString] | 26,999,288 | 1,203,824 | 1,249,744 | 25|
|csv-conduit/readCSVFile/Vector ByteString | 27,928,040 | 1,199,256 | 1,245,600 | 26|
|csv-conduit/readCSVFile/[String] | 34,273,176 | 2,548,016 | 3,808,568 | 32|
|csv/Text.CSV/parseCSVFromFile | 136,246,608 | 3,863,224 | 13,073,912 | 133|
|sv/Data.Sv.Parse/attoparsecText | 109,841,312 | 5,127,032 | 12,525,960 | 105|
|sv/Data.Sv.Parse/attoparsecByteString | 99,511,432 | 7,149,176 | 18,077,320 | 95|
|sv/Data.Sv.Parse/trifecta | 270,332,176 | 9,476,144 | 24,269,824 | 259|


<!-- RESULTS -->

## file (time)

|Name||
|---|---|
|cassava/decode/Vector ByteString|2.360 ms|
|cassava/decode/[ByteString]|3.185 ms|
|lazy-csv/parseCsv/[ByteString]|4.410 ms|
|csv-conduit/readCSVFile/[ByteString]|11.29 ms|
|csv-conduit/readCSVFile/Vector ByteString|11.96 ms|
|csv-conduit/readCSVFile/[String]|17.88 ms|
|csv/Text.CSV/parseCSVFromFile|42.22 ms|
|sv/Data.Sv.Parse/attoparsecText|61.06 ms|
|sv/Data.Sv.Parse/attoparsecByteString|72.07 ms|
|sv/Data.Sv.Parse/trifecta|113.1 ms|
