# workerBench Results

In this repo you can find the raw data and plots for the bachelors thesis "Achieving Low Latency and High Throughput in Real-Time Web Applications Using Web Workers" that were measured using the benchmark suite provided at [https://github.com/ch2i5/worker-bench](https://github.com/ch2i5/worker-bench)

The ZIP archives contain workload benchmarks for different browsers that were conducted on a given device. These measurements are provided as CSV-files.
After extracting the archives you can use the "deviceanalysis.R" script to (re-)generate the PDF plots available in the "graphs" directory.

Devices, operating systems and browsers used to generate data:
| |Chrome 126 |Firefox 127.0.1<br>(no Atomics)|Safari 17.5|
|---|:---:|:---:|:---:|
| Mac Mini M1, <br>16GB LPDDR4X-4266MT/s SDRAM, <br>macOS Sonoma 14.5 | ✅ | ✅ | ✅ |
| Lenovo L390 Yoga i7-8565U, <br>16GB DDR4-2400MHz SDRAM, <br>Windows 11 Pro 23H2 | ✅ | ✅ | ❌ |
| Lenovo L390 Yoga i7-8565U, <br>16GB DDR4-2400MHz SDRAM, <br>Windows 11 Pro 23H2 | ✅ | ✅ | ❌ |
| iPad 9th Gen A13-Bionic, <br>3GB LPDDR4X SDRAM, <br>iPadOS 17.5.1 | ❌ | ❌ | ✅ |

Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]

This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
