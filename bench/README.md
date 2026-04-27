# Benchmark

A really really initial version of benchmark.

## Results

on Macbook Air M1

### Lokto_dream

```sh
$ wrk -c 1 -t 1 -d30s http://localhost:3000/plaintext
Running 30s test @ http://localhost:3000/plaintext
  1 threads and 1 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    32.46us   10.34us   1.24ms   93.96%
    Req/Sec    30.42k   522.98    30.92k    98.67%
  911132 requests in 30.10s, 45.18MB read
Requests/sec:  30270.84
Transfer/sec:      1.50MB

$ dune exec bench/lokto_dream_bench.exe
^Cminor_collections:      7042
major_collections:      11
compactions:            0
forced_major_collections: 1

minor_words:    66329
promoted_words: 18153
major_words:    18186

top_heap_words: 816709
heap_words:     482604
live_words:      17607
free_words:     463697
largest_free:        0
fragments:        1300

live_blocks: 2498
free_blocks: 0
heap_chunks: 0
```

```sh
$ wrk -c 8 -t 8 -d30s http://localhost:3000/plaintext
Running 30s test @ http://localhost:3000/plaintext
  8 threads and 8 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   144.69us  132.99us   1.92ms   84.68%
    Req/Sec     8.44k     5.70k   27.04k    50.92%
  2019816 requests in 30.10s, 100.16MB read
Requests/sec:  67105.97
Transfer/sec:      3.33MB

$ dune exec bench/lokto_dream_bench.exe
^Cminor_collections:      3423
major_collections:      20
compactions:            0
forced_major_collections: 1

minor_words:    80571
promoted_words: 231121
major_words:    231154

top_heap_words: 873569
heap_words:     474412
live_words:      17612
free_words:     455536
largest_free:        0
fragments:        1264

live_blocks: 2498
free_blocks: 0
heap_chunks: 0
```


### Dream

```sh
$ wrk -c 1 -t 1 -d30s http://localhost:8080/plaintext
Running 30s test @ http://localhost:8080/plaintext
  1 threads and 1 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    28.22us    4.97us   1.01ms   95.38%
    Req/Sec    34.44k   664.56    35.03k    99.00%
  1031570 requests in 30.10s, 51.16MB read
Requests/sec:  34272.36
Transfer/sec:      1.70MB

$ dune exec bench/dream_bench.exe
^Cminor_collections:      9124
major_collections:      40
compactions:            0
forced_major_collections: 1

minor_words:    2385072473
promoted_words:    3474039
major_words:       3481707

top_heap_words: 385023
heap_words:     143359
live_words:      50297
free_words:      92080
largest_free:        0
fragments:         982

live_blocks: 7288
free_blocks: 0
heap_chunks: 0
```

```sh
$ wrk -c 8 -t 8 -d30s http://localhost:8080/plaintext
Running 30s test @ http://localhost:8080/plaintext
  8 threads and 8 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    91.37us   35.03us   2.09ms   69.62%
    Req/Sec    10.84k     2.46k   15.43k    57.31%
  2597211 requests in 30.10s, 128.80MB read
Requests/sec:  86286.22
Transfer/sec:      4.28MB

$ dune exec bench/dream_bench.exe
^Cminor_collections:      22762
major_collections:      396
compactions:            0
forced_major_collections: 1

minor_words:    5913588520
promoted_words:   39530891
major_words:      39538559

top_heap_words: 368639
heap_words:     143359
live_words:      50297
free_words:      92080
largest_free:        0
fragments:         982

live_blocks: 7288
free_blocks: 0
heap_chunks: 0
```
