# csv

Simple comparative benchmarks for CSV parsing libraries.

The "file" benchmarks are IO-based: reading from a file into a list or
vector of rows which is then forced with a deepseq. Conveniently, the
differences between each library are reproducibly distinct.

## file (space)

|Case                                      |  Allocated|        Max|       Live|  GCs|
|------------------------------------------|-----------|-----------|-----------|-----|
|cassava/decode/Vector ByteString          |    964,872|      9,200|     23,200|    0|
|cassava/decode/[ByteString]               |  1,596,064|      9,128|     23,008|    1|
|lazy-csv/parseCsv/[ByteString]            |  3,294,072|     40,992|     54,944|    3|
|csv-conduit/readCSVFile/[ByteString]      |  3,462,744|     14,176|     28,272|    3|
|csv-conduit/readCSVFile/Vector ByteString |  3,762,496|     14,360|     28,576|    3|
|csv-conduit/readCSVFile/[String]          |  4,396,888|     14,320|     28,320|    4|
|csv/Text.CSV/parseCSVFromFile             | 15,254,792|     41,512|     55,440|   14|
|sv/Data.Sv.Parse/attoparsecText           | 15,424,760|    811,304|    844,040|   14|
|sv/Data.Sv.Parse/attoparsecByteString     | 14,038,912|  1,492,152|  1,525,040|   13|
|sv/Data.Sv.Parse/trifecta                 | 37,249,392|  1,124,096|  2,290,080|   35|

<!-- RESULTS -->

## file (time)

|Name||
|---|---|
|cassava/decode/Vector ByteString|0.224 ms|
|cassava/decode/[ByteString]|0.351 ms|
|lazy-csv/parseCsv/[ByteString]|0.538 ms|
|csv-conduit/readCSVFile/[ByteString]|0.950 ms|
|csv-conduit/readCSVFile/Vector ByteString|1.065 ms|
|csv-conduit/readCSVFile/[String]|1.371 ms|
|csv/Text.CSV/parseCSVFromFile|3.511 ms|
|sv/Data.Sv.Parse/attoparsecText|6.988 ms|
|sv/Data.Sv.Parse/attoparsecByteString|7.377 ms|
|sv/Data.Sv.Parse/trifecta|11.90 ms|

