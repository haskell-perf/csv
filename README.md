# csv

Simple comparative benchmarks for CSV parsing libraries.

<!-- RESULTS -->

## file

|Name||
|---|---|
|String/cassava/decode/[ByteString]|0.408 ms|
|String/csv-conduit/readCSVFile/[ByteString]|1.276 ms|
|String/csv-conduit/readCSVFile/Vector ByteString|1.488 ms|
|String/csv-conduit/readCSVFile/[String]|1.759 ms|
|String/csv|3.768 ms|
|String/sv/Data.Sv.Parse/attoparsecText|8.856 ms|
|String/sv/Data.Sv.Parse/attoparsecByteString|9.394 ms|
|String/sv/Data.Sv.Parse/trifecta|14.77 ms|
