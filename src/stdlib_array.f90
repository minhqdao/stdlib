! SPDX-Identifier: MIT


!> Module for index manipulation and general array handling
!>
!> The specification of this module is available [here](../page/specs/stdlib_array.html).
module stdlib_array
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    implicit none
    private

    public :: trueloc, falseloc

    !> Helper class to allocate t_array as an abstract type.
    type, public :: t_array_wrapper
        class(t_array), allocatable :: array
    contains
        generic :: allocate_array => allocate_array_rsp_1
        procedure :: allocate_array_rsp_1
        generic :: allocate_array => allocate_array_rsp_2
        procedure :: allocate_array_rsp_2
        generic :: allocate_array => allocate_array_rsp_3
        procedure :: allocate_array_rsp_3
        generic :: allocate_array => allocate_array_rsp_4
        procedure :: allocate_array_rsp_4
        generic :: allocate_array => allocate_array_rsp_5
        procedure :: allocate_array_rsp_5
        generic :: allocate_array => allocate_array_rsp_6
        procedure :: allocate_array_rsp_6
        generic :: allocate_array => allocate_array_rsp_7
        procedure :: allocate_array_rsp_7
        generic :: allocate_array => allocate_array_rdp_1
        procedure :: allocate_array_rdp_1
        generic :: allocate_array => allocate_array_rdp_2
        procedure :: allocate_array_rdp_2
        generic :: allocate_array => allocate_array_rdp_3
        procedure :: allocate_array_rdp_3
        generic :: allocate_array => allocate_array_rdp_4
        procedure :: allocate_array_rdp_4
        generic :: allocate_array => allocate_array_rdp_5
        procedure :: allocate_array_rdp_5
        generic :: allocate_array => allocate_array_rdp_6
        procedure :: allocate_array_rdp_6
        generic :: allocate_array => allocate_array_rdp_7
        procedure :: allocate_array_rdp_7
        generic :: allocate_array => allocate_array_iint8_1
        procedure :: allocate_array_iint8_1
        generic :: allocate_array => allocate_array_iint8_2
        procedure :: allocate_array_iint8_2
        generic :: allocate_array => allocate_array_iint8_3
        procedure :: allocate_array_iint8_3
        generic :: allocate_array => allocate_array_iint8_4
        procedure :: allocate_array_iint8_4
        generic :: allocate_array => allocate_array_iint8_5
        procedure :: allocate_array_iint8_5
        generic :: allocate_array => allocate_array_iint8_6
        procedure :: allocate_array_iint8_6
        generic :: allocate_array => allocate_array_iint8_7
        procedure :: allocate_array_iint8_7
        generic :: allocate_array => allocate_array_iint16_1
        procedure :: allocate_array_iint16_1
        generic :: allocate_array => allocate_array_iint16_2
        procedure :: allocate_array_iint16_2
        generic :: allocate_array => allocate_array_iint16_3
        procedure :: allocate_array_iint16_3
        generic :: allocate_array => allocate_array_iint16_4
        procedure :: allocate_array_iint16_4
        generic :: allocate_array => allocate_array_iint16_5
        procedure :: allocate_array_iint16_5
        generic :: allocate_array => allocate_array_iint16_6
        procedure :: allocate_array_iint16_6
        generic :: allocate_array => allocate_array_iint16_7
        procedure :: allocate_array_iint16_7
        generic :: allocate_array => allocate_array_iint32_1
        procedure :: allocate_array_iint32_1
        generic :: allocate_array => allocate_array_iint32_2
        procedure :: allocate_array_iint32_2
        generic :: allocate_array => allocate_array_iint32_3
        procedure :: allocate_array_iint32_3
        generic :: allocate_array => allocate_array_iint32_4
        procedure :: allocate_array_iint32_4
        generic :: allocate_array => allocate_array_iint32_5
        procedure :: allocate_array_iint32_5
        generic :: allocate_array => allocate_array_iint32_6
        procedure :: allocate_array_iint32_6
        generic :: allocate_array => allocate_array_iint32_7
        procedure :: allocate_array_iint32_7
        generic :: allocate_array => allocate_array_iint64_1
        procedure :: allocate_array_iint64_1
        generic :: allocate_array => allocate_array_iint64_2
        procedure :: allocate_array_iint64_2
        generic :: allocate_array => allocate_array_iint64_3
        procedure :: allocate_array_iint64_3
        generic :: allocate_array => allocate_array_iint64_4
        procedure :: allocate_array_iint64_4
        generic :: allocate_array => allocate_array_iint64_5
        procedure :: allocate_array_iint64_5
        generic :: allocate_array => allocate_array_iint64_6
        procedure :: allocate_array_iint64_6
        generic :: allocate_array => allocate_array_iint64_7
        procedure :: allocate_array_iint64_7
        generic :: allocate_array => allocate_array_csp_1
        procedure :: allocate_array_csp_1
        generic :: allocate_array => allocate_array_csp_2
        procedure :: allocate_array_csp_2
        generic :: allocate_array => allocate_array_csp_3
        procedure :: allocate_array_csp_3
        generic :: allocate_array => allocate_array_csp_4
        procedure :: allocate_array_csp_4
        generic :: allocate_array => allocate_array_csp_5
        procedure :: allocate_array_csp_5
        generic :: allocate_array => allocate_array_csp_6
        procedure :: allocate_array_csp_6
        generic :: allocate_array => allocate_array_csp_7
        procedure :: allocate_array_csp_7
        generic :: allocate_array => allocate_array_cdp_1
        procedure :: allocate_array_cdp_1
        generic :: allocate_array => allocate_array_cdp_2
        procedure :: allocate_array_cdp_2
        generic :: allocate_array => allocate_array_cdp_3
        procedure :: allocate_array_cdp_3
        generic :: allocate_array => allocate_array_cdp_4
        procedure :: allocate_array_cdp_4
        generic :: allocate_array => allocate_array_cdp_5
        procedure :: allocate_array_cdp_5
        generic :: allocate_array => allocate_array_cdp_6
        procedure :: allocate_array_cdp_6
        generic :: allocate_array => allocate_array_cdp_7
        procedure :: allocate_array_cdp_7
    end type

    type, abstract, public :: t_array
        character(:), allocatable :: name
    end type

    type, extends(t_array), public :: t_array_rsp_1
        real(sp), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_rsp_2
        real(sp), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_rsp_3
        real(sp), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_rsp_4
        real(sp), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rsp_5
        real(sp), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rsp_6
        real(sp), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rsp_7
        real(sp), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rdp_1
        real(dp), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_rdp_2
        real(dp), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_rdp_3
        real(dp), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_rdp_4
        real(dp), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rdp_5
        real(dp), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rdp_6
        real(dp), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_rdp_7
        real(dp), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint8_1
        integer(int8), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_iint8_2
        integer(int8), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_iint8_3
        integer(int8), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint8_4
        integer(int8), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint8_5
        integer(int8), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint8_6
        integer(int8), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint8_7
        integer(int8), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint16_1
        integer(int16), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_iint16_2
        integer(int16), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_iint16_3
        integer(int16), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint16_4
        integer(int16), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint16_5
        integer(int16), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint16_6
        integer(int16), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint16_7
        integer(int16), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint32_1
        integer(int32), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_iint32_2
        integer(int32), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_iint32_3
        integer(int32), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint32_4
        integer(int32), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint32_5
        integer(int32), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint32_6
        integer(int32), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint32_7
        integer(int32), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint64_1
        integer(int64), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_iint64_2
        integer(int64), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_iint64_3
        integer(int64), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint64_4
        integer(int64), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint64_5
        integer(int64), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint64_6
        integer(int64), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_iint64_7
        integer(int64), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_csp_1
        complex(sp), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_csp_2
        complex(sp), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_csp_3
        complex(sp), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_csp_4
        complex(sp), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_csp_5
        complex(sp), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_csp_6
        complex(sp), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_csp_7
        complex(sp), allocatable :: values(:,:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_cdp_1
        complex(dp), allocatable :: values(:)
    end type
    type, extends(t_array), public :: t_array_cdp_2
        complex(dp), allocatable :: values(:,:)
    end type
    type, extends(t_array), public :: t_array_cdp_3
        complex(dp), allocatable :: values(:,:,:)
    end type
    type, extends(t_array), public :: t_array_cdp_4
        complex(dp), allocatable :: values(:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_cdp_5
        complex(dp), allocatable :: values(:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_cdp_6
        complex(dp), allocatable :: values(:,:,:,:,:,:)
    end type
    type, extends(t_array), public :: t_array_cdp_7
        complex(dp), allocatable :: values(:,:,:,:,:,:,:)
    end type

contains

    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rsp_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(sp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rsp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rsp_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_rdp_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        real(dp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_rdp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_rdp_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint8_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int8), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint8_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint8_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint16_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int16), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint16_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint16_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint32_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int32), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint32_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint32_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_iint64_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        integer(int64), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_iint64_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_iint64_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_csp_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(sp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_csp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_csp_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_1 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_1 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_1)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_2 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_2 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_2)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_3 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_3 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_3)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_4 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_4 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_4)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_5 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_5 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_5)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_6 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_6 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_6)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end
    !> Allocate an instance of the array within the wrapper.
    subroutine allocate_array_cdp_7 (wrapper, source_array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        complex(dp), intent(in) :: source_array(:,:,:,:,:,:,:)
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        allocate (t_array_cdp_7 :: wrapper%array, stat=stat)
        if (stat /= 0) then
            msg = 'Failed to allocate array.'; return
        end if

        select type (typed_array => wrapper%array)
          class is (t_array_cdp_7)
            typed_array%values = source_array
          class default
            msg = 'Failed to allocate values.'; stat = 1; return
        end select
    end

    !> Version: experimental
    !>
    !> Return the positions of the true elements in array.
    !> [Specification](../page/specs/stdlib_array.html#trueloc)
    pure function trueloc(array, lbound) result(loc)
        !> Mask of logicals
        logical, intent(in) :: array(:)
        !> Lower bound of array to index
        integer, intent(in), optional :: lbound
        !> Locations of true elements
        integer :: loc(count(array))

        call logicalloc(loc, array, .true., lbound)
    end

    !> Version: experimental
    !>
    !> Return the positions of the false elements in array.
    !> [Specification](../page/specs/stdlib_array.html#falseloc)
    pure function falseloc(array, lbound) result(loc)
        !> Mask of logicals
        logical, intent(in) :: array(:)
        !> Lower bound of array to index
        integer, intent(in), optional :: lbound
        !> Locations of false elements
        integer :: loc(count(.not. array))

        call logicalloc(loc, array, .false., lbound)
    end

    !> Return the positions of the truthy elements in array
    pure subroutine logicalloc(loc, array, truth, lbound)
        !> Locations of truthy elements
        integer, intent(out) :: loc(:)
        !> Mask of logicals
        logical, intent(in) :: array(:)
        !> Truthy value
        logical, intent(in) :: truth
        !> Lower bound of array to index
        integer, intent(in), optional :: lbound
        integer :: i, pos, offset

        offset = 0
        if (present(lbound)) offset = lbound - 1

        i = 0
        do pos = 1, size(array)
            if (array(pos) .eqv. truth) then
                i = i + 1
                loc(i) = pos + offset
            end if
        end do
    end
end
