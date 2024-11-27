program demo
  use, intrinsic :: iso_fortran_env
  use wav_fortran, only : lpcm_wav_read, lpcm_wav_write
  implicit none

  real(real64), parameter :: demo_freq = 1.0d3, demo_sec = 3.0d0
  real(real64), allocatable :: demo_sine(:)

  integer(int32) :: sampling_rate
  integer(int16) :: bitq
  integer(int32) :: i, len
  real(real64) :: pi

  pi = 4.0d0*atan(1.0d0)

  sampling_rate = 44100

  len = int(sampling_rate * demo_sec)
  allocate(demo_sine(len))

  do i = 1, len
    demo_sine(i) = 0.6d0*sin((2*pi*demo_freq*i)/sampling_rate)
  end do

  ! write a wav file (16bit)
  bitq = 16
  call lpcm_wav_write("./example/demo_sine_16bit.wav", demo_sine, sampling_rate, bitq)
  deallocate(demo_sine)

  ! read the wav file (16bit) and save into a wav file (8bit)

  call lpcm_wav_read("./example/demo_sine_16bit.wav", demo_sine, sampling_rate)
  bitq = 8
  call lpcm_wav_write("./example/demo_sine_8bit.wav", demo_sine, sampling_rate, bitq)
  deallocate(demo_sine)

end program demo
