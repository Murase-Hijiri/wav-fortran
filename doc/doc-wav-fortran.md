# Subroutines
## `lpcm_wav_read`
```Fortran
subroutine lpcm_wav_read(filename, wave_data, fs, bitq_out)
  character(len=*), intent(in) :: filename
  real(real64), allocatable, intent(inout) :: wave_data(:)
  integer(int32), intent(inout) :: fs
  integer(int16), intent(inout), optional :: bitq_out
  ...
end subroutine lpcm_wav_read
```
#### `filename`
Name of file to read.
#### `wave_data`
Data of the audio.
Length of `wave_data` is number of sample points.
Values range from `-1.0d0` to `1.0d0`.
#### `fs`
Sampling rate of the audio.
#### `bitq_out` (optional)
Quantization bit rate of the audio.

## `lpcm_wav_write`
```Fortran
subroutine lpcm_wav_write(filename, wave_data, fs, bitq)
  character(len=*), intent(in) :: filename
  real(real64), intent(in) :: wave_data(:)
  integer(int32), intent(in) :: fs
  integer(int16), intent(in) :: bitq
end subroutine lpcm_wav_write
```
#### `filename`
Name of file to write.
#### `wave_data`
Data of the audio.
Length of `wave_data` is number of sample points.
Values should be ranged from `-1.0d0` to `1.0d0` so that the waveform is not distorted.
#### `fs`
Sampling rate of the audio.
#### `bitq`
Quantization bit rate of the audio.
