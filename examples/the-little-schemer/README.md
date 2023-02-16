
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
  Time (mean ± σ):      68.8 ms ±   4.5 ms    [User: 84.2 ms, System: 3.5 ms]
  Range (min … max):    61.8 ms …  78.5 ms    100 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     204.9 ms ±   2.2 ms    [User: 161.8 ms, System: 43.1 ms]
  Range (min … max):   200.2 ms … 214.0 ms    100 runs
 
Benchmark 3: loco run-all.scm
  Time (mean ± σ):      15.3 ms ±   5.6 ms    [User: 18.4 ms, System: 0.1 ms]
  Range (min … max):    13.0 ms …  69.1 ms    138 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 4: ../../rusch run-all.scm
  Time (mean ± σ):      12.4 ms ±   1.3 ms    [User: 16.6 ms, System: 0.0 ms]
  Range (min … max):    11.2 ms …  18.5 ms    164 runs
 
Summary
  '../../rusch run-all.scm' ran
    1.23 ± 0.47 times faster than 'loco run-all.scm'
    5.56 ± 0.70 times faster than 'gosch run-all.scm'
   16.56 ± 1.79 times faster than 'scheme --quiet < run-all.scm'
```
