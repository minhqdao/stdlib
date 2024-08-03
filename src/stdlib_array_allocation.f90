! SPDX-Identifier: MIT


submodule(stdlib_array) stdlib_array_allocation
    implicit none

contains

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rsp_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rsp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rsp_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_rdp_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_rdp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_rdp_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint8_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint8_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint8_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint16_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint16_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint16_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint32_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint32_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint32_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_iint64_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_iint64_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_iint64_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_csp_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_csp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_csp_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_1(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_1)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_2(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_2)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_3(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_3)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_4(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_4)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_5(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_5)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_6(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_6)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

    !> Allocate an instance of the array within the wrapper.
    module subroutine allocate_array_cdp_7(wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate(t_array_cdp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = "Failed to allocate array"; return
        end if

        ! select type (typed_array => wrapper%array)
        !   class is (t_array_cdp_7)
        !     typed_array%values = source_array
        !   class default
        !     msg = 'Failed to allocate values.'; stat = 1; return
        ! end select
    end

end
