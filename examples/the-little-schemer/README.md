
The code example comes from _*The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme, my implementation of Scheme in [Go (`gosch`)](https://github.com/twolodzko/gosch)
and [OCaml (`loco`)](https://github.com/twolodzko/loco).

```shell
$ hyperfine -m 100 --warmup 10 \
    'gosch run-all.scm' \
    'scheme --quiet < run-all.scm' \
    'loco run-all.scm' \
    '../../rusch run-all.scm'
Benchmark 1: gosch run-all.scm
  Time (mean ± σ):      72.4 ms ±   5.1 ms    [User: 83.2 ms, System: 6.6 ms]
  Range (min … max):    64.3 ms …  93.4 ms    100 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     212.3 ms ±  15.1 ms    [User: 165.6 ms, System: 46.5 ms]
  Range (min … max):   202.3 ms … 297.3 ms    100 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 3: loco run-all.scm
  Time (mean ± σ):      18.1 ms ±   0.9 ms    [User: 16.9 ms, System: 1.3 ms]
  Range (min … max):    17.3 ms …  23.7 ms    127 runs
 
  Warning: The first benchmarking run for this command was significantly slower than the rest (22.4 ms). This could be caused by (filesystem) caches that were not filled until after the first run. You should consider using the '--warmup' option to fill those caches before the actual benchmark. Alternatively, use the '--prepare' option to clear the caches before each timing run.
 
Benchmark 4: ../../rusch run-all.scm
  Time (mean ± σ):      20.2 ms ±   1.4 ms    [User: 19.7 ms, System: 0.7 ms]
  Range (min … max):    19.4 ms …  30.5 ms    142 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Summary
  'loco run-all.scm' ran
    1.12 ± 0.09 times faster than '../../rusch run-all.scm'
    4.01 ± 0.35 times faster than 'gosch run-all.scm'
   11.75 ± 1.02 times faster than 'scheme --quiet < run-all.scm'
```
