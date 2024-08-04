! SPDX-Identifier: MIT


!> Implementation of loading npy files into multidimensional arrays
submodule(stdlib_io_np) stdlib_io_np_load
    use stdlib_error, only: error_stop
    use stdlib_strings, only: to_string, starts_with
    use stdlib_string_type, only: string_type
    use stdlib_io_zip, only: unzip, zip_prefix, zip_suffix, t_unzipped_bundle, t_unzipped_file
    use stdlib_array
    implicit none

contains

    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_rsp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_rsp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_rsp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_rsp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_rsp_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_rsp_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_rsp_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rsp
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_rdp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_rdp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_rdp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_rdp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_rdp_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_rdp_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_rdp_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_rdp
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint8_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint8_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint8_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint8_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_iint8_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_iint8_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_iint8_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint8
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint16_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint16_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint16_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint16_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_iint16_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_iint16_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_iint16_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint16
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint32_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint32_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint32_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint32_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_iint32_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_iint32_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_iint32_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint32
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_iint64_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_iint64_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_iint64_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_iint64_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_iint64_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_iint64_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_iint64_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_iint64
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_csp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_csp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_csp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_csp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_csp_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_csp_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_csp_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_csp
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 1-dimensional array from a npy file
    module subroutine load_npy_cdp_1(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 1

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 2-dimensional array from a npy file
    module subroutine load_npy_cdp_2(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 2

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 3-dimensional array from a npy file
    module subroutine load_npy_cdp_3(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 3

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 4-dimensional array from a npy file
    module subroutine load_npy_cdp_4(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 4

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 5-dimensional array from a npy file
    module subroutine load_npy_cdp_5(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 5

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 6-dimensional array from a npy file
    module subroutine load_npy_cdp_6(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 6

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end
    !> Load a 7-dimensional array from a npy file
    module subroutine load_npy_cdp_7(filename, array, iostat, iomsg)
        !> Name of the npy file to load from
        character(len=*), intent(in) :: filename
        !> Array to be loaded from the npy file
        complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        !> Error status of loading, zero on success
        integer, intent(out), optional :: iostat
        !> Associated error message in case of non-zero status code
        character(len=:), allocatable, intent(out), optional :: iomsg

        character(len=*), parameter :: vtype = type_cdp
        integer, parameter :: rank = 7

        integer :: io, stat
        character(len=:), allocatable :: msg

        open(newunit=io, file=filename, form="unformatted", access="stream", iostat=stat)
        catch: block
            character(len=:), allocatable :: this_type
            integer, allocatable :: vshape(:)

            call get_descriptor(io, filename, this_type, vshape, stat, msg)
            if (stat /= 0) exit catch

            if (this_type /= vtype) then
                stat = 1
                msg = "File '"//filename//"' contains data of type '"//this_type//"', "//&
                & "but expected '"//vtype//"'"
                exit catch
            end if

            if (size(vshape) /= rank) then
                stat = 1
                msg = "File '"//filename//"' contains data of rank "//&
                & to_string(size(vshape))//", but expected "//&
                & to_string(rank)
                exit catch
            end if

            call allocate_array_from_shape(array, vshape, stat)
            if (stat /= 0) then
                msg = "Failed to allocate array of type '"//vtype//"' "//&
                & "with total size of "//to_string(product(vshape))
                exit catch
            end if

            read(io, iostat=stat) array
        end block catch
        close(io)

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read array from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read array from file '"//filename//"'")
            end if
        end if

        if (present(iomsg).and.allocated(msg)) call move_alloc(msg, iomsg)
    end

    module subroutine allocate_array_from_shape_rsp_1(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rsp_2(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rsp_3(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rsp_4(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rsp_5(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rsp_6(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rsp_7(array, vshape, stat)
        real(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_1(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_2(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_3(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_4(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_5(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_6(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_rdp_7(array, vshape, stat)
        real(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_1(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_2(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_3(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_4(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_5(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_6(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint8_7(array, vshape, stat)
        integer(int8), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_1(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_2(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_3(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_4(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_5(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_6(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint16_7(array, vshape, stat)
        integer(int16), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_1(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_2(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_3(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_4(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_5(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_6(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint32_7(array, vshape, stat)
        integer(int32), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_1(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_2(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_3(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_4(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_5(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_6(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_iint64_7(array, vshape, stat)
        integer(int64), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_1(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_2(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_3(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_4(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_5(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_6(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_csp_7(array, vshape, stat)
        complex(sp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_1(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_2(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_3(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_4(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_5(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_6(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6)), &
        &    stat=stat)
    end
    module subroutine allocate_array_from_shape_cdp_7(array, vshape, stat)
        complex(dp), allocatable, intent(out) :: array(:,:,:,:,:,:,:)
        integer, intent(in) :: vshape(:)
        integer, intent(out) :: stat

        allocate(array( &
        &    vshape(1), &
        &    vshape(2), &
        &    vshape(3), &
        &    vshape(4), &
        &    vshape(5), &
        &    vshape(6), &
        &    vshape(7)), &
        &    stat=stat)
    end

    !> Version: experimental
    !>
    !> Load multidimensional arrays from a compressed or uncompressed npz file.
    !> ([Specification](../page/specs/stdlib_io.html#load_npz))
    module subroutine load_npz_to_arrays(filename, arrays, iostat, iomsg)
        character(len=*), intent(in) :: filename
        type(t_array_wrapper), allocatable, intent(out) :: arrays(:)
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        type(t_unzipped_bundle) :: unzipped_bundle
        integer :: stat
        character(len=:), allocatable :: msg

        call unzip(filename, unzipped_bundle, stat, msg)
        if (stat == 0) then
            call load_unzipped_bundle_to_arrays(unzipped_bundle, arrays, stat, msg)
        else
            call identify_unzip_problem(filename, stat, msg)
        end if

        if (present(iostat)) then
            iostat = stat
        else if (stat /= 0) then
            if (allocated(msg)) then
                call error_stop("Failed to read arrays from file '"//filename//"'"//nl//msg)
            else
                call error_stop("Failed to read arrays from file '"//filename//"'")
            end if
        end if

        if (present(iomsg) .and. allocated(msg)) call move_alloc(msg, iomsg)
    end

    subroutine load_unzipped_bundle_to_arrays(unzipped_bundle, arrays, stat, msg)
        type(t_unzipped_bundle), intent(in) :: unzipped_bundle
        type(t_array_wrapper), allocatable, intent(out) :: arrays(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        integer :: i, io
        integer, allocatable :: vshape(:)
        character(len=:), allocatable :: this_type

        allocate (arrays(size(unzipped_bundle%files)))

        do i = 1, size(unzipped_bundle%files)
            open (newunit=io, status='scratch', form='unformatted', access='stream', iostat=stat, iomsg=msg)
            if (stat /= 0) return

            write (io, iostat=stat) unzipped_bundle%files(i)%data
            if (stat /= 0) then
                msg = 'Failed to write unzipped data to scratch file.'
                close (io, status='delete'); return
            end if

            rewind (io)
            call get_descriptor(io, unzipped_bundle%files(i)%name, this_type, vshape, stat, msg)
            if (stat /= 0) return

            select case (this_type)
              case (type_rsp)
                select case (size(vshape))
                  case (1)
                    block
                        real(sp), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        real(sp), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        real(sp), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        real(sp), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        real(sp), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        real(sp), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        real(sp), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_rdp)
                select case (size(vshape))
                  case (1)
                    block
                        real(dp), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        real(dp), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        real(dp), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        real(dp), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        real(dp), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        real(dp), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        real(dp), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_iint8)
                select case (size(vshape))
                  case (1)
                    block
                        integer(int8), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        integer(int8), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        integer(int8), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        integer(int8), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        integer(int8), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        integer(int8), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        integer(int8), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_iint16)
                select case (size(vshape))
                  case (1)
                    block
                        integer(int16), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        integer(int16), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        integer(int16), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        integer(int16), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        integer(int16), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        integer(int16), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        integer(int16), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_iint32)
                select case (size(vshape))
                  case (1)
                    block
                        integer(int32), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        integer(int32), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        integer(int32), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        integer(int32), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        integer(int32), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        integer(int32), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        integer(int32), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_iint64)
                select case (size(vshape))
                  case (1)
                    block
                        integer(int64), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        integer(int64), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        integer(int64), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        integer(int64), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        integer(int64), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        integer(int64), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        integer(int64), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_csp)
                select case (size(vshape))
                  case (1)
                    block
                        complex(sp), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        complex(sp), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        complex(sp), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        complex(sp), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        complex(sp), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        complex(sp), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        complex(sp), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case (type_cdp)
                select case (size(vshape))
                  case (1)
                    block
                        complex(dp), allocatable :: array(:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (2)
                    block
                        complex(dp), allocatable :: array(:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (3)
                    block
                        complex(dp), allocatable :: array(:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (4)
                    block
                        complex(dp), allocatable :: array(:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (5)
                    block
                        complex(dp), allocatable :: array(:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (6)
                    block
                        complex(dp), allocatable :: array(:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case (7)
                    block
                        complex(dp), allocatable :: array(:,:,:,:,:,:,:)

                        call allocate_array_from_shape(array, vshape, stat)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"'."; return
                        end if

                        read (io, iostat=stat) array
                        if (stat /= 0) then
                            msg = "Failed to read array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        ! call arrays(i)%allocate_array(stat, msg)
                        if (stat /= 0) then
                            msg = "Failed to allocate array of type '"//this_type//"' "//&
                            & 'with total size of '//to_string(product(vshape)); return
                        end if

                        arrays(i)%array%name = unzipped_bundle%files(i)%name
                    end block
                  case default
                    stat = 1; msg = 'Unsupported rank for array of type '//this_type//': '// &
                    & to_string(size(vshape))//'.'; return
                end select
              case default
                stat = 1; msg = 'Unsupported array type: '//this_type//'.'; return
            end select

            close (io, status='delete')
            if (stat /= 0) return
        end do
    end

    !> Open file and try to identify the cause of the error that occurred during unzip.
    subroutine identify_unzip_problem(filename, stat, msg)
        character(len=*), intent(in) :: filename
        integer, intent(inout) :: stat
        character(len=:), allocatable, intent(inout) :: msg

        logical :: exists
        integer :: io_unit, prev_stat
        character(len=:), allocatable :: prev_msg

        ! Keep track of the previous status and message in case no reason can be found.
        prev_stat = stat
        if (allocated(msg)) call move_alloc(msg, prev_msg)

        inquire (file=filename, exist=exists)
        if (.not. exists) then
            stat = 1; msg = 'File does not exist: '//filename//'.'; return
        end if
        open (newunit=io_unit, file=filename, form='unformatted', access='stream', &
        & status='old', action='read', iostat=stat, iomsg=msg)
        if (stat /= 0) return

        call verify_header(io_unit, stat, msg)
        if (stat /= 0) return

        ! Restore previous status and message if no reason could be found.
        stat = prev_stat; msg = 'Failed to unzip file: '//filename//nl//prev_msg
    end

    subroutine verify_header(io_unit, stat, msg)
        integer, intent(in) :: io_unit
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        integer :: file_size
        character(len=len(zip_prefix)) :: header

        inquire (io_unit, size=file_size)
        if (file_size < len(zip_suffix)) then
            stat = 1; msg = 'File is too small to be an npz file.'; return
        end if

        read (io_unit, iostat=stat) header
        if (stat /= 0) then
            msg = 'Failed to read header from file'; return
        end if

        if (header == zip_suffix) then
            stat = 1; msg = 'Empty npz file.'; return
        end if

        if (header /= zip_prefix) then
            stat = 1; msg = 'Not an npz file.'; return
        end if
    end

    !> Read the npy header from a binary file and retrieve the descriptor string.
    subroutine get_descriptor(io, filename, vtype, vshape, stat, msg)
        !> Unformatted, stream accessed unit
        integer, intent(in) :: io
        !> Filename for error reporting
        character(len=*), intent(in) :: filename
        !> Type of data saved in npy file
        character(len=:), allocatable, intent(out) :: vtype
        !> Shape descriptor of the
        integer, allocatable, intent(out) :: vshape(:)
        !> Status of operation
        integer, intent(out) :: stat
        !> Associated error message in case of non-zero status
        character(len=:), allocatable, intent(out) :: msg

        integer :: major, header_len, i
        character(len=:), allocatable :: dict
        character(len=8) :: header
        character :: buf(4)
        logical :: fortran_order

        ! stat should be zero if no error occurred
        stat = 0

        read(io, iostat=stat) header
        if (stat /= 0) return

        call parse_header(header, major, stat, msg)
        if (stat /= 0) return

        read(io, iostat=stat) buf(1:merge(4, 2, major > 1))
        if (stat /= 0) return

        if (major > 1) then
            header_len = ichar(buf(1)) &
            &      + ichar(buf(2)) * 256**1 &
            &      + ichar(buf(3)) * 256**2 &
            &      + ichar(buf(4)) * 256**3
        else
            header_len = ichar(buf(1)) &
            &      + ichar(buf(2)) * 256**1
        end if
        allocate(character(header_len) :: dict, stat=stat)
        if (stat /= 0) return

        read(io, iostat=stat) dict
        if (stat /= 0) return

        if (dict(header_len:header_len) /= nl) then
            stat = 1
            msg = "Descriptor length does not match"
            return
        end if

        if (scan(dict, achar(0)) > 0) then
            stat = 1
            msg = "Nul byte not allowed in descriptor string"
            return
        end if

        call parse_descriptor(trim(dict(:len(dict)-1)), filename, &
        & vtype, fortran_order, vshape, stat, msg)
        if (stat /= 0) return

        if (.not.fortran_order) then
            vshape = [(vshape(i), i = size(vshape), 1, -1)]
        end if
    end


    !> Parse the first eight bytes of the npy header to verify the data
    subroutine parse_header(header, major, stat, msg)
        !> Header of the binary file
        character(len=*), intent(in) :: header
        !> Major version of the npy format
        integer, intent(out) :: major
        !> Status of operation
        integer, intent(out) :: stat
        !> Associated error message in case of non-zero status
        character(len=:), allocatable, intent(out) :: msg

        integer :: minor

        ! stat should be zero if no error occurred
        stat = 0

        if (header(1:1) /= magic_number) then
            stat = 1
            msg = "Expected z'93' but got z'"//to_string(ichar(header(1:1)))//"' "//&
            & "as first byte"
            return
        end if

        if (header(2:6) /= magic_string) then
            stat = 1
            msg = "Expected identifier '"//magic_string//"'"
            return
        end if

        major = ichar(header(7:7))
        if (.not.any(major == [1, 2, 3])) then
            stat = 1
            msg = "Unsupported format major version number '"//to_string(major)//"'"
            return
        end if

        minor = ichar(header(8:8))
        if (minor /= 0) then
            stat = 1
            msg = "Unsupported format version "// &
            & "'"//to_string(major)//"."//to_string(minor)//"'"
            return
        end if
    end

    !> Parse the descriptor in the npy header. This routine implements a minimal
    !> non-recursive parser for serialized Python dictionaries.
    subroutine parse_descriptor(input, filename, vtype, fortran_order, vshape, stat, msg)
        !> Input string to parse as descriptor
        character(len=*), intent(in) :: input
        !> Filename for error reporting
        character(len=*), intent(in) :: filename
        !> Type of the data stored, retrieved from field `descr`
        character(len=:), allocatable, intent(out) :: vtype
        !> Whether the data is in left layout, retrieved from field `fortran_order`
        logical, intent(out) :: fortran_order
        !> Shape of the stored data, retrieved from field `shape`
        integer, allocatable, intent(out) :: vshape(:)
        !> Status of operation
        integer, intent(out) :: stat
        !> Associated error message in case of non-zero status
        character(len=:), allocatable, intent(out) :: msg

        enum, bind(c)
            enumerator :: invalid, string, lbrace, rbrace, comma, colon, &
                lparen, rparen, bool, literal, space
        end enum

        type :: token_type
            integer :: first, last, kind
        end type token_type

        integer :: pos
        character(len=:), allocatable :: key
        type(token_type) :: token, last
        logical :: has_descr, has_shape, has_fortran_order

        has_descr = .false.
        has_shape = .false.
        has_fortran_order = .false.
        pos = 0
        call next_token(input, pos, token, [lbrace], stat, msg)
        if (stat /= 0) return

        last = token_type(pos, pos, comma)
        do while (pos < len(input))
            call get_token(input, pos, token)
            select case(token%kind)
              case(space)
                continue
              case(comma)
                if (token%kind == last%kind) then
                    stat = 1
                    msg = make_message(filename, input, token%first, token%last, &
                    & "Comma cannot appear at this point")
                    return
                end if
                last = token
              case(rbrace)
                exit
              case(string)
                if (token%kind == last%kind) then
                    stat = 1
                    msg = make_message(filename, input, token%first, token%last, &
                    & "String cannot appear at this point")
                    return
                end if
                last = token

                key = input(token%first+1:token%last-1)
                call next_token(input, pos, token, [colon], stat, msg)
                if (stat /= 0) return

                if (key == "descr" .and. has_descr &
                & .or. key == "fortran_order" .and. has_fortran_order &
                & .or. key == "shape" .and. has_shape) then
                    stat = 1
                    msg = make_message(filename, input, last%first, last%last, &
                    & "Duplicate entry for '"//key//"' found")
                    return
                end if

                select case(key)
                  case("descr")
                    call next_token(input, pos, token, [string], stat, msg)
                    if (stat /= 0) return

                    vtype = input(token%first+1:token%last-1)
                    has_descr = .true.

                  case("fortran_order")
                    call next_token(input, pos, token, [bool], stat, msg)
                    if (stat /= 0) return

                    fortran_order = input(token%first:token%last) == "True"
                    has_fortran_order = .true.

                  case("shape")
                    call parse_tuple(input, pos, vshape, stat, msg)

                    has_shape = .true.

                  case default
                    stat = 1
                    msg = make_message(filename, input, last%first, last%last, &
                    & "Invalid entry '"//key//"' in dictionary encountered")
                    return
                end select
              case default
                stat = 1
                msg = make_message(filename, input, token%first, token%last, &
                & "Invalid token encountered")
                return
            end select
        end do

        if (.not.has_descr) then
            stat = 1
            msg = make_message(filename, input, 1, pos, &
            & "Dictionary does not contain required entry 'descr'")
        end if

        if (.not.has_shape) then
            stat = 1
            msg = make_message(filename, input, 1, pos, &
            & "Dictionary does not contain required entry 'shape'")
        end if

        if (.not.has_fortran_order) then
            stat = 1
            msg = make_message(filename, input, 1, pos, &
            & "Dictionary does not contain required entry 'fortran_order'")
        end if

    contains

        function make_message(filename, input, first, last, message) result(str)
            !> Filename for context
            character(len=*), intent(in) :: filename
            !> Input string to parse
            character(len=*), intent(in) :: input
            !> Offset in the input
            integer, intent(in) :: first, last
            !> Error message
            character(len=*), intent(in) :: message
            !> Final output message
            character(len=:), allocatable :: str

            character(len=*), parameter :: nl = new_line('a')

            str = message // nl // &
            & " --> " // filename // ":1:" // to_string(first) // "-" // to_string(last) // nl // &
            & "  |" // nl // &
            & "1 | " // input // nl // &
            & "  |" // repeat(" ", first) // repeat("^", last - first + 1) // nl // &
            & "  |"
        end

        !> Parse a tuple of integers into an array of integers
        subroutine parse_tuple(input, pos, tuple, stat, msg)
            !> Input string to parse
            character(len=*), intent(in) :: input
            !> Offset in the input, will be advanced after reading
            integer, intent(inout) :: pos
            !> Array representing tuple of integers
            integer, allocatable, intent(out) :: tuple(:)
            !> Status of operation
            integer, intent(out) :: stat
            !> Associated error message in case of non-zero status
            character(len=:), allocatable, intent(out) :: msg

            type(token_type) :: token
            integer :: last, itmp

            allocate(tuple(0), stat=stat)
            if (stat /= 0) return

            call next_token(input, pos, token, [lparen], stat, msg)
            if (stat /= 0) return

            last = comma
            do while (pos < len(input))
                call get_token(input, pos, token)
                select case(token%kind)
                  case(space)
                    continue
                  case(literal)
                    if (token%kind == last) then
                        stat = 1
                        msg = make_message(filename, input, token%first, token%last, &
                        & "Invalid token encountered")
                        return
                    end if
                    last = token%kind
                    read(input(token%first:token%last), *, iostat=stat) itmp
                    if (stat /= 0) then
                        return
                    end if
                    tuple = [tuple, itmp]
                  case(comma)
                    if (token%kind == last) then
                        stat = 1
                        msg = make_message(filename, input, token%first, token%last, &
                        & "Invalid token encountered")
                        return
                    end if
                    last = token%kind
                  case(rparen)
                    exit
                  case default
                    stat = 1
                    msg = make_message(filename, input, token%first, token%last, &
                    & "Invalid token encountered")
                    return
                end select
            end do
        end

        !> Get the next allowed token
        subroutine next_token(input, pos, token, allowed_token, stat, msg)
            !> Input string to parse
            character(len=*), intent(in) :: input
            !> Current offset in the input string
            integer, intent(inout) :: pos
            !> Last token parsed
            type(token_type), intent(out) :: token
            !> Tokens allowed in the current context
            integer, intent(in) :: allowed_token(:)
            !> Status of operation
            integer, intent(out) :: stat
            !> Associated error message in case of non-zero status
            character(len=:), allocatable, intent(out) :: msg

            stat = pos
            do while (pos < len(input))
                call get_token(input, pos, token)
                if (token%kind == space) then
                    continue
                else if (any(token%kind == allowed_token)) then
                    stat = 0
                    exit
                else
                    stat = 1
                    msg = make_message(filename, input, token%first, token%last, &
                    & "Invalid token encountered")
                    exit
                end if
            end do
        end

        !> Tokenize input string
        subroutine get_token(input, pos, token)
            !> Input strin to tokenize
            character(len=*), intent(in) :: input
            !> Offset in input string, will be advanced
            integer, intent(inout) :: pos
            !> Returned token from the next position
            type(token_type), intent(out) :: token

            character :: quote

            pos = pos + 1
            select case(input(pos:pos))
              case("""", "'")
                quote = input(pos:pos)
                token%first = pos
                pos = pos + 1
                do while (pos <= len(input))
                    if (input(pos:pos) == quote) then
                        token%last = pos
                        exit
                    else
                        pos = pos + 1
                    end if
                end do
                token%kind = string
              case("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
                token%first = pos
                do while (pos <= len(input))
                    if (.not.any(input(pos:pos) == ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])) then
                        pos = pos - 1
                        token%last = pos
                        exit
                    else
                        pos = pos + 1
                    end if
                end do
                token%kind = literal
              case("T")
                if (starts_with(input(pos:), "True")) then
                    token = token_type(pos, pos+3, bool)
                    pos = pos + 3
                else
                    token = token_type(pos, pos, invalid)
                end if
              case("F")
                if (starts_with(input(pos:), "False")) then
                    token = token_type(pos, pos+4, bool)
                    pos = pos + 4
                else
                    token = token_type(pos, pos, invalid)
                end if
              case("{")
                token = token_type(pos, pos, lbrace)
              case("}")
                token = token_type(pos, pos, rbrace)
              case(",")
                token = token_type(pos, pos, comma)
              case(":")
                token = token_type(pos, pos, colon)
              case("(")
                token = token_type(pos, pos, lparen)
              case(")")
                token = token_type(pos, pos, rparen)
              case(" ", nl)
                token = token_type(pos, pos, space)
              case default
                token = token_type(pos, pos, invalid)
            end select
        end
    end
end
