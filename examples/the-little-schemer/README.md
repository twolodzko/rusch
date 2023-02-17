
The code example comes from _*The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme, my implementation of Scheme in [Go (`gosch`)](https://github.com/twolodzko/gosch)
and [OCaml (`loco`)](https://github.com/twolodzko/loco).

```shell
$ hyperfine -m 100 --warmup 10 \
    '../../rusch run-all.scm' \
    'loco run-all.scm' \
    'gosch run-all.scm' \
    'scheme --quiet < run-all.scm' 
Benchmark 1: ../../rusch run-all.scm
  Time (mean ± σ):      16.4 ms ±   1.3 ms    [User: 16.3 ms, System: 0.6 ms]
  Range (min … max):    15.0 ms …  23.4 ms    500 runs
 
Benchmark 2: loco run-all.scm
  Time (mean ± σ):      18.6 ms ±   2.1 ms    [User: 17.7 ms, System: 1.2 ms]
  Range (min … max):    16.9 ms …  36.1 ms    500 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 3: gosch run-all.scm
  Time (mean ± σ):      74.0 ms ±   4.5 ms    [User: 84.1 ms, System: 8.0 ms]
  Range (min … max):    65.5 ms … 103.7 ms    500 runs
 
Benchmark 4: scheme --quiet < run-all.scm
  Time (mean ± σ):     219.4 ms ±  11.2 ms    [User: 168.3 ms, System: 50.9 ms]
  Range (min … max):   209.0 ms … 362.5 ms    500 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Summary
  '../../rusch run-all.scm' ran
    1.13 ± 0.16 times faster than 'loco run-all.scm'
    4.51 ± 0.46 times faster than 'gosch run-all.scm'
   13.38 ± 1.29 times faster than 'scheme --quiet < run-all.scm'
```
