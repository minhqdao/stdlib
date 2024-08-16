! SPDX-Identifier: MIT


!> Implementation of saving multidimensional arrays to npy files
submodule(stdlib_io_np) stdlib_io_np_save
    use stdlib_array
    use stdlib_error, only: error_stop
    use stdlib_filesystem, only: run
    use stdlib_strings, only: to_string
    use stdlib_string_type, only: string_type, as_string => char
    use stdlib_io_zip, only: zip
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
    module subroutine save_npy_rsp_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rsp_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rsp_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rsp_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rsp_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rsp_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rsp_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_rdp_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint8_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint16_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint32_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_iint64_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_csp_7(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_1(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_2(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_3(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_4(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_5(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_6(filename, array, iostat, iomsg)
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
    module subroutine save_npy_cdp_7(filename, array, iostat, iomsg)
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

    module subroutine add_array_rsp_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rsp_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rsp_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rsp_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rsp_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rsp_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rsp_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rsp_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_rdp_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_rdp_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint8_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint8_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint16_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint16_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint32_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint32_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_iint64_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_iint64_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_csp_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_csp_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_1(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_1) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_2(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_2) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_3(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_3) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_4(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_4) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_5(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_5) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_6(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_6) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end
    module subroutine add_array_cdp_7(arrays, array, stat, msg, name)
        !> Array of arrays to which the array is to be added.
        type(t_array_wrapper), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i
        type(t_array_cdp_7) :: t_arr
        type(t_array_wrapper) :: wrapper

        if (present(stat)) stat = 0

        if (present(name)) then
            if (trim(name) == '') then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array name cannot be empty."
                return
            end if
            t_arr%name = name
        else
            if (allocated(arrays)) then
                t_arr%name = "arr_"//to_string(size(arrays))//".npy"
            else
                t_arr%name = "arr_0.npy"
            end if
        end if

        allocate(t_arr%values, source=array)
        if (.not. allocated(arrays)) then
            allocate(arrays(1))
            allocate(arrays(1)%array, source=t_arr)
            return
        end if

        do i = 1, size(arrays)
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(wrapper%array, source=t_arr)

print *, 'wrapper: '
select type (typed_array => wrapper%array)
  class is (t_array_rdp_2)
    print *, typed_array%values
  class is (t_array_cdp_1)
    print *, typed_array%values
  class default
end select

        arrays = [arrays, wrapper]

print *, 'after allocating arrays'
do i = 1, size(arrays)
    print *, arrays(i)%array%name
    select type (typed_array => arrays(i)%array)
      class is (t_array_rdp_2)
        print *, typed_array%values
      class is (t_array_cdp_1)
        print *, typed_array%values
      class default
    end select
end do
    end

    !> Version: experimental
    !>
    !> Save multidimensional arrays to a compressed or an uncompressed npz file.
    !> ([Specification](../page/specs/stdlib_io.html#save_npz))
    module subroutine save_arrays_to_npz(filename, arrays, iostat, iomsg, compressed)
        !> Name of the npz file to save to.
        character(len=*), intent(in) :: filename
        !> Arrays to be saved.
        type(t_array_wrapper), intent(in) :: arrays(:)
        !> Optional error status of saving, zero on success.
        integer, intent(out), optional :: iostat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: iomsg
        !> If true, the file is saved in compressed format. The default is false.
        logical, intent(in), optional :: compressed

        integer :: i, j, stat
        logical :: is_compressed
        character(len=:), allocatable :: msg
        type(string_type), allocatable :: files(:)

        if (present(iostat)) iostat = 0
        stat = 0

        if (present(compressed)) then
            is_compressed = compressed
        else
            is_compressed = .false.
        end if

        allocate(files(0))
        do i = 1, size(arrays)
            select type (typed_array => arrays(i)%array)
              class is (t_array_rsp_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rsp_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rsp_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rsp_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rsp_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rsp_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rsp_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_rdp_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint8_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint16_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint32_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_iint64_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_csp_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_1)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_2)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_3)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_4)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_5)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_6)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class is (t_array_cdp_7)
                do j = 1, size(files)
                    if (as_string(files(j)) == typed_array%name) then
                        if (present(iostat)) iostat = 1
                        if (present(iomsg)) iomsg = "Error saving array to file '"//filename// &
                            "': Array with the same name '"//typed_array%name//"' already exists."
                        call delete_files(files)
                        return
                    end if
                end do

                call save_npy(typed_array%name, typed_array%values, stat, msg)
                if (stat /= 0) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = msg
                    call delete_files(files)
                    return
                end if

                files = [files, string_type(typed_array%name)]
              class default
                if (present(iostat)) iostat = 1
                if (present(iomsg)) iomsg = "Error saving array to file '"//filename//"': Unsupported array type."
                call delete_files(files)
                return
            end select
        end do

        call zip(filename, files, stat, msg, is_compressed)
        if (stat /= 0) then
            if (present(iostat)) iostat = stat
            if (present(iomsg)) iomsg = msg
            call delete_files(files)
            return
        end if

        call delete_files(files)
    end

    subroutine delete_files(files)
        type(string_type), allocatable, intent(in) :: files(:)

        integer :: i, unit

        do i = 1, size(files)
            open(newunit=unit, file=as_string(files(i)))
            close(unit, status="delete")
        end do
    end
end
