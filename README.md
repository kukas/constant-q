# Constant Q transform
Naive [Constant Q transform](http://academics.wellesley.edu/Physics/brown/pubs/cq1stPaper.pdf) implementation in Haskell. CQT is more suitable for melodic input data than discrete Fourier transform because the transform resolution in lower frequency bands is bigger - in other words the bin frequency/resolution ratio remains constant.

## Usage

```
Usage: constant-q-exe [OPTIONS]... -i INPUTFILE -o OUTPUTFILE
Process INPUTFILE (in WAV format) and save the Constant Q transform spectrogram to OUTPUTFILE (png image format).
Options:
  -min FREQ Set minimum frequency to FREQ (default 55.0)
  -max FREQ Set maximum frequency to FREQ (default 11000.0)
  -b NUM    Set the number of frequency bins in one octave (default 48)
  -p NUM    Set the hop size (default 1024)
  -q NUM    Set the Q factor (quality of frequency resolution) (default 72)
  -h    Print a help message and exit
```
