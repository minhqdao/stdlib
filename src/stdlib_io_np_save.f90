! SPDX-Identifier: MIT


!> Implementation of saving multidimensional arrays to npy files
submodule(stdlib_io_np) stdlib_io_np_save
    use stdlib_error, only: error_stop
    use stdlib_strings, only: to_string
    implicit none

contains

    !> Generate magic header string for npy format
    pure function magic_header(major, minor) result(str)
        !> Major version of npy format
        integer, intent(in) :: major
        !> Minor version of npy format
        integer, intent(in) :: minor
        !> Magic string for npy format
        character(len=8) :: str

        str = magic_number//magic_string//achar(major)//achar(minor)
    end

    !> Generate header for npy format
    pure function npy_header(vtype, vshape) result(str)
        !> Type of variable
        character(len=*), intent(in) :: vtype
        !> Shape of variable
        integer, intent(in) :: vshape(:)
        !> Header string for npy format
        character(len=:), allocatable :: str

        integer, parameter :: len_v10 = 8 + 2, len_v20 = 8 + 4, block_size = 64

        str = &
            "{'descr': '"//vtype// &
            "', 'fortran_order': True, 'shape': "// &
            shape_str(vshape)//", }"

        if (len(str) + len_v10 >= 65535) then
            str = str// &
            & repeat(" ", block_size - mod(len(str) + len_v20 + 1, block_size))//nl
            str = magic_header(2, 0)//to_bytes_i4(int(len(str)))//str
        else
            str = str// &
            & repeat(" ", block_size - mod(len(str) + len_v10 + 1, block_size))//nl
            str = magic_header(1, 0)//to_bytes_i2(int(len(str)))//str
        end if
    end

    !> Write integer as byte string in little endian encoding
    pure function to_bytes_i4(val) result(str)
        !> Integer value to convert to bytes
        integer, intent(in) :: val
        !> String of bytes
        character(len=4) :: str

        str = achar(mod(val, 256**1))// &
        & achar(mod(val, 256**2)/256**1)// &
        & achar(mod(val, 256**3)/256**2)// &
        & achar(val/256**3)
    end

    !> Write integer as byte string in little endian encoding, 2-byte truncated version
    pure function to_bytes_i2(val) result(str)
        !> Integer value to convert to bytes
        integer, intent(in) :: val
        !> String of bytes
        character(len=2) :: str

        str = achar(mod(val, 2**8))// &
        & achar(mod(val, 2**16)/2**8)
    end

    !> Print array shape as tuple of int
    pure function shape_str(vshape) result(str)
        !> Shape of variable
        integer, intent(in) :: vshape(:)
        !> Shape string for npy format
        character(len=:), allocatable :: str

        integer :: i

        str = "("
        do i = 1, size(vshape)
            str = str//to_string(vshape(i))//", "
        end do
        str = str//")"
    end

    !> Save 1-dimensional array in npy format
    module subroutine save_npy_rsp_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_rsp_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_rsp_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_rsp_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_rsp_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_rsp_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_rsp_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_rdp_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_rdp_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_rdp_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_rdp_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_rdp_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_rdp_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_rdp_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_iint8_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_iint8_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_iint8_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_iint8_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_iint8_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_iint8_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_iint8_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_iint16_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_iint16_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_iint16_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_iint16_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_iint16_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_iint16_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_iint16_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_iint32_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_iint32_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_iint32_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_iint32_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_iint32_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_iint32_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_iint32_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_iint64_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_iint64_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_iint64_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_iint64_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_iint64_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_iint64_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_iint64_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_csp_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_csp_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_csp_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_csp_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_csp_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_csp_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_csp_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 1-dimensional array in npy format
    module subroutine save_npy_cdp_1 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 2-dimensional array in npy format
    module subroutine save_npy_cdp_2 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 3-dimensional array in npy format
    module subroutine save_npy_cdp_3 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 4-dimensional array in npy format
    module subroutine save_npy_cdp_4 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 5-dimensional array in npy format
    module subroutine save_npy_cdp_5 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 6-dimensional array in npy format
    module subroutine save_npy_cdp_6 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end
    !> Save 7-dimensional array in npy format
    module subroutine save_npy_cdp_7 (filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer :: io, stat

        open (newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        if (stat == 0) then
            write (io, iostat=stat) npy_header(vtype, shape(array))
        end if
        if (stat == 0) then
            write (io, iostat=stat) array
        end if
        close (io, iostat=stat)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            call error_stop("Failed to write array to file '"//filename//"'")
        end if

        if (present(iomsg)) then
            if (stat /= 0) then
                iomsg = "Failed to write array to file '"//filename//"'"
            end if
        end if
    end

    !> Version: experimental
    !>
    !> Save multidimensional arrays to a compressed or an uncompressed npz file.
    !> ([Specification](../page/specs/stdlib_io.html#save_npz))
    module subroutine save_npz_from_arrays(filename, arrays, compressed, iostat, iomsg)
        character(len=*), intent(in) :: filename
        type(t_array_wrapper), intent(in) :: arrays(*)
        !> If true, the file is saved in compressed format. The default is false.
        logical, intent(in), optional :: compressed
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        logical :: is_compressed

        if (present(compressed)) then
            is_compressed = compressed
        else
            is_compressed = .false.
        end if
    end
end
