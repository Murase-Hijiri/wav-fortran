module wav_fortran
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  public :: lpcm_wav_read
  public :: lpcm_wav_write

contains

  function bin3byte_to_int32(bin3byte) result(retval)
    integer(int8), intent(in) :: bin3byte(3)
    integer(int32) :: retval

    retval = 0
    call mvbits(int(bin3byte(1)), 0, 8, retval, 0)
    call mvbits(int(bin3byte(2)), 0, 8, retval, 8)
    call mvbits(int(bin3byte(3)), 0, 16, retval, 16)
  end function bin3byte_to_int32

  subroutine int32_to_bin3byte(var_int, bin3byte)
    integer(int32), intent(in) :: var_int
    integer(int8), intent(inout) :: bin3byte(3)

    integer(int32) :: int32_each2byte(3)
    
    bin3byte(1) = iand(var_int, Z'FF')
    bin3byte(2) = iand(ishft(var_int, -8), Z'FF')
    bin3byte(3) = iand(ishft(var_int, -16), Z'FF')
  end subroutine int32_to_bin3byte

  function wave_clip(val) result(retval)
    real(real64), intent(in) :: val
    real(real64) :: retval

    retval = val
    if (val > 1.0d0) retval = 1.0d0
    if (val < -1.0d0) retval = -1.0d0
  end function wave_clip

  subroutine lpcm_wav_read(filename, wave_data, fs, bitq_out)
    character(len=*), intent(in) :: filename
    real(real64), allocatable, intent(inout) :: wave_data(:)
    integer(int32), intent(inout) :: fs
    integer(int16), intent(inout), optional :: bitq_out

    character(4) :: char_4byte
    integer(int32) :: unit_num, var32, LENGTH, NUM_MAX, i
    integer(int16) :: var16, bitq
    integer(int8) :: var8, var8_3byte(3)

    open(newunit=unit_num, file=filename, status="old", form="unformatted", access="stream")

    read(unit_num) char_4byte ! "RIFF"
    if (char_4byte /= "RIFF") then
      print *, "Error: The input file is NOT RIFF-formatted."
      return
    end if

    read(unit_num) var32 ! file size - 8byte

    read(unit_num) char_4byte ! "WAVE"
    if (char_4byte /= "WAVE") then
      print *, "Error: The input file is NOT a WAVE file."
      return
    end if

    read(unit_num) char_4byte ! "fmt "
    if (char_4byte /= "fmt ") then
      print *, "Error: The input file dos NOT have format chunk."
      return
    end if

    read(unit_num) var32 ! size of fmt chunk - 8byte
    read(unit_num) var16 ! format
    if (var16 /= 1) then
      ! format is NOT LPCM
      print *, "Error: Format ", var16, " NOT supported"
      return
    end if

    read(unit_num) var16 ! channnel
    if (var16 /= 1) then
      print *, "Error: Only MONO signal is Supported."
      return
    end if

    read(unit_num) var32 ! sampling freq
    fs = var32

    read(unit_num) var32 ! size per sec
    read(unit_num) var16 ! block size

    read(unit_num) var16 ! quantization bits
    bitq = var16
    if (present(bitq_out)) bitq_out = var16
    if (bitq/=8 .and. bitq/=16 .and. bitq/=24 .and. bitq/=32) then
      print *, "Error: Bitrate is NOT supported."
      return
    end if

    read(unit_num) char_4byte ! "data"
    if (char_4byte /= "data") then
      print *, "Error: Data chunk does NOT exist or Format is NOT supported."
      return
    end if

    read(unit_num) var32 ! datasize
    LENGTH = var32/(bitq/8)

    if (allocated(wave_data)) deallocate(wave_data)
    allocate(wave_data(LENGTH))

    NUM_MAX = 2**(bitq-1)
    select case (bitq)
      case (8)
        do i = 1, LENGTH
          read(unit_num) var8
          wave_data(i) = var8*1.0d0/NUM_MAX - 1.0d0
        end do
      case (16)
        do i = 1, LENGTH
          read(unit_num) var16
          wave_data(i) = var16*1.0d0/NUM_MAX
        end do
      case (24)
        do i = 1, LENGTH
          read(unit_num) var8_3byte
          wave_data(i) = bin3byte_to_int32(var8_3byte)*1.0d0/NUM_MAX
        end do
      case(32)
        do i = 1, LENGTH
          read(unit_num) var32
          wave_data(i) = var32*1.0d0/NUM_MAX
        end do
    end select

    close(unit_num)
  end subroutine lpcm_wav_read

  subroutine lpcm_wav_write(filename, wave_data, fs, bitq)
    character(len=*), intent(in) :: filename
    real(real64), intent(in) :: wave_data(:)
    integer(int32), intent(in) :: fs
    integer(int16), intent(in) :: bitq

    integer(int32) :: unit_num, datasize, LENGTH, NUM_MAX, i
    integer(int16) :: var16
    integer(int8) :: var8, var8_3byte(3)

    open(newunit=unit_num, file=filename, status="replace", form="unformatted", access="stream")
    
    LENGTH = size(wave_data)
    datasize = (bitq/8)*LENGTH
    
    write(unit_num) "RIFF"
    write(unit_num) datasize+36
    write(unit_num) "WAVE"
    write(unit_num) "fmt "
    write(unit_num) int(16)
    var16 = 1
    write(unit_num) var16 ! format
    write(unit_num) var16 ! channel
    write(unit_num) fs
    write(unit_num) int((bitq/8) * fs)! bytes per sec
    var16 = bitq/8
    write(unit_num) var16 ! block size
    write(unit_num) bitq
    write(unit_num) "data"
    write(unit_num) datasize

    NUM_MAX = 2**(bitq-1)
    select case (bitq)
      case (8)
        do i = 1, LENGTH
          var8 = int(wave_clip((wave_data(i) + 1.0d0)) * NUM_MAX)
          write(unit_num) var8
        end do
      case (16)
        do i = 1, LENGTH
          var16 = int(wave_clip(wave_data(i)) * NUM_MAX)
          write(unit_num) var16
        end do
      case (24)
        do i = 1, LENGTH
          call int32_to_bin3byte(int(wave_clip(wave_data(i)) * NUM_MAX), var8_3byte)
          write(unit_num) var8_3byte
        end do
      case(32)
        do i = 1, LENGTH
          write(unit_num) int(wave_clip(wave_data(i)) * NUM_MAX)
        end do
    end select

    close(unit_num)
  end subroutine lpcm_wav_write

end module wav_fortran
