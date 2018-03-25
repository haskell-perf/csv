# csv

Simple comparative benchmarks for CSV parsing libraries.

The "file" benchmarks are IO-based: reading from a file into a list or
vector of rows which is then forced with a deepseq. Conveniently, the
differences between each library are reproducibly distinct.

## file (space)

|Case|Allocated|Max|Live|GCs|
|---|---|---|---|---|
|cassava/decode/Vector ByteString          |    964,872|      9,200|     22,432|    0|
|cassava/decode/[ByteString]               |  1,596,064|      9,128|     22,240|    1|
|csv-conduit/readCSVFile/[ByteString]      |  3,462,736|     14,168|     27,496|    3|
|csv-conduit/readCSVFile/Vector ByteString |  3,762,488|     14,352|     27,800|    3|
|csv-conduit/readCSVFile/[String]          |  4,396,880|     14,312|     27,544|    4|
|csv/Text.CSV/parseCSVFromFile             | 15,254,792|     41,512|     54,672|   14|
|sv/Data.Sv.Parse/attoparsecText           | 15,424,752|    811,296|    842,488|   14|
|sv/Data.Sv.Parse/attoparsecByteString     | 14,038,904|  1,492,152|  1,523,496|   13|
|sv/Data.Sv.Parse/trifecta                 | 37,249,384|  1,124,096|  2,287,760|   35|

<!-- RESULTS -->

## file (time)

|Name||
|---|---|
|cassava/decode/Vector ByteString|0.270 ms|
|cassava/decode/[ByteString]|0.408 ms|
|csv-conduit/readCSVFile/[ByteString]|1.232 ms|
|csv-conduit/readCSVFile/Vector ByteString|1.397 ms|
|csv-conduit/readCSVFile/[String]|1.762 ms|
|csv/Text.CSV/parseCSVFromFile|3.780 ms|
|sv/Data.Sv.Parse/attoparsec Text|8.873 ms|
|sv/Data.Sv.Parse/attoparsec ByteString|9.413 ms|
|sv/Data.Sv.Parse/trifecta|14.76 ms|
