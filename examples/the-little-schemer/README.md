
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
  Time (mean ± σ):      69.8 ms ±   8.2 ms    [User: 78.7 ms, System: 6.5 ms]
  Range (min … max):    62.9 ms … 123.9 ms    100 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     207.5 ms ±   5.7 ms    [User: 160.6 ms, System: 46.9 ms]
  Range (min … max):   202.5 ms … 257.9 ms    100 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 3: loco run-all.scm
  Time (mean ± σ):      17.8 ms ±   0.5 ms    [User: 16.6 ms, System: 1.3 ms]
  Range (min … max):    17.2 ms …  21.8 ms    157 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 4: ../../rusch run-all.scm
  Time (mean ± σ):      21.5 ms ±   0.8 ms    [User: 21.2 ms, System: 0.5 ms]
  Range (min … max):    20.9 ms …  28.0 ms    132 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Summary
  'loco run-all.scm' ran
    1.21 ± 0.06 times faster than '../../rusch run-all.scm'
    3.92 ± 0.48 times faster than 'gosch run-all.scm'
   11.66 ± 0.48 times faster than 'scheme --quiet < run-all.scm'
```
