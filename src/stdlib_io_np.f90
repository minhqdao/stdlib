! SPDX-Identifier: MIT


!> Description of the npy format taken from
!> https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
!>
!>## Format Version 1.0
!>
!> The first 6 bytes are a magic string: exactly \x93NUMPY.
!>
!> The next 1 byte is an unsigned byte:
!> the major version number of the file format, e.g. \x01.
!>
!> The next 1 byte is an unsigned byte:
!> the minor version number of the file format, e.g. \x00.
!> Note: the version of the file format is not tied to the version of the numpy package.
!>
!> The next 2 bytes form a little-endian unsigned short int:
!> the length of the header data HEADER_LEN.
!>
!> The next HEADER_LEN bytes form the header data describing the array’s format.
!> It is an ASCII string which contains a Python literal expression of a dictionary.
!> It is terminated by a newline (\n) and padded with spaces (\x20) to make the total
!> of len(magic string) + 2 + len(length) + HEADER_LEN be evenly divisible by 64 for
!> alignment purposes.
!>
!> The dictionary contains three keys:
!>
!> - “descr”: dtype.descr
!>   An object that can be passed as an argument to the numpy.dtype constructor
!>   to create the array’s dtype.
!>
!> - “fortran_order”: bool
!>   Whether the array data is Fortran-contiguous or not. Since Fortran-contiguous
!>   arrays are a common form of non-C-contiguity, we allow them to be written directly
!>   to disk for efficiency.
!>
!> - “shape”: tuple of int
!>   The shape of the array.
!>
!> For repeatability and readability, the dictionary keys are sorted in alphabetic order.
!> This is for convenience only. A writer SHOULD implement this if possible. A reader MUST
!> NOT depend on this.
!>
!> Following the header comes the array data. If the dtype contains Python objects
!> (i.e. dtype.hasobject is True), then the data is a Python pickle of the array.
!> Otherwise the data is the contiguous (either C- or Fortran-, depending on fortran_order)
!> bytes of the array. Consumers can figure out the number of bytes by multiplying the
!> number of elements given by the shape (noting that shape=() means there is 1 element)
!> by dtype.itemsize.
!>
!>## Format Version 2.0
!>
!> The version 1.0 format only allowed the array header to have a total size of 65535 bytes.
!> This can be exceeded by structured arrays with a large number of columns.
!> The version 2.0 format extends the header size to 4 GiB. numpy.save will automatically
!> save in 2.0 format if the data requires it, else it will always use the more compatible
!> 1.0 format.
!>
!> The description of the fourth element of the header therefore has become:
!> “The next 4 bytes form a little-endian unsigned int: the length of the header data
!> HEADER_LEN.”
!>
!>## Format Version 3.0
!>
!> This version replaces the ASCII string (which in practice was latin1) with a
!> utf8-encoded string, so supports structured types with any unicode field names.
module stdlib_io_np
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    use stdlib_array, only: t_array_wrapper
    implicit none
    private

    public :: load_npy, save_npy, load_npz, save_npz, add_array

    character(len=*), parameter :: &
        type_iint8 = "<i1", type_iint16 = "<i2", type_iint32 = "<i4", type_iint64 = "<i8", &
        type_rsp = "<f4", type_rdp = "<f8", type_rxdp = "<f10", type_rqp = "<f16", &
        type_csp = "<c8", type_cdp = "<c16", type_cxdp = "<c20", type_cqp = "<c32", &
        nl = achar(10), magic_number = char(int(z"93")), magic_string = "NUMPY"

    !> Version: experimental
    !>
    !> Load multidimensional array in npy format
    !> ([Specification](../page/specs/stdlib_io.html#load_npy))
    interface load_npy
        module subroutine load_npy_rsp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rsp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rsp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rsp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rsp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rsp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rsp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_rdp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint8_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint16_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint32_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_iint64_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_csp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine load_npy_cdp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
    end interface

    !> Version: experimental
    !>
    !> Save multidimensional array in npy format
    !> ([Specification](../page/specs/stdlib_io.html#save_npy))
    interface save_npy
        module subroutine save_npy_rsp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rsp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rsp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rsp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rsp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rsp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rsp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_rdp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint8_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint16_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint32_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_iint64_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_csp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_5(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_6(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
        module subroutine save_npy_cdp_7(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:,:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end
    end interface

    !> Version: experimental
    !>
    !> Load multiple multidimensional arrays from a (compressed) npz file.
    !> ([Specification](../page/specs/stdlib_io.html#load_npz))
    interface load_npz
        module subroutine load_npz_to_arrays(filename, arrays, iostat, iomsg, tmp_dir)
            character(len=*), intent(in) :: filename
            type(t_array_wrapper), allocatable, intent(out) :: arrays(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
            character(len=*), intent(in), optional :: tmp_dir
        end
    end interface

    !> Version: experimental
    !>
    !> Save multidimensional arrays to a compressed or an uncompressed npz file.
    !> ([Specification](../page/specs/stdlib_io.html#save_npz))
    interface save_npz
        module subroutine save_npz_from_arrays(filename, arrays, iostat, iomsg, compressed)
            character(len=*), intent(in) :: filename
            type(t_array_wrapper), intent(in) :: arrays(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
            !> If true, the file is saved in compressed format. The default is false.
            logical, intent(in), optional :: compressed
        end
    end interface

    interface allocate_array_from_shape
        module subroutine allocate_array_from_shape_rsp_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rsp_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rsp_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rsp_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rsp_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rsp_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rsp_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_rdp_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint8_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint16_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint32_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_iint64_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_csp_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_1 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_2 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_3 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_4 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_5 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_6 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
        module subroutine allocate_array_from_shape_cdp_7 (array, vshape, stat)
            !> Instance of the array to be allocated.
            complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
            !> Dimensions to allocate for.
            integer, intent(in) :: vshape(:)
            !> Status of allocate.
            integer, intent(out) :: stat
        end
    end interface

contains

    subroutine add_array()
    end
end
