! SPDX-Identifier: MIT


!> Module for index manipulation and general array handling
!>
!> The specification of this module is available [here](../page/specs/stdlib_array.html).
module stdlib_array
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    use stdlib_strings, only: to_string
    implicit none
    private

    public :: add_array, trueloc, falseloc

    !> Version: experimental
    !>
    !> Wrapper class that helps with allocation of array_type.
    !> [Specification](../page/specs/stdlib_array.html#array_wrapper_type)
    type, public :: array_wrapper_type
        !> Polymorphic array.
        class(array_type), allocatable :: array
    contains
        generic :: get_values => get_values_rsp_1
        procedure :: get_values_rsp_1
        generic :: get_values => get_values_rsp_2
        procedure :: get_values_rsp_2
        generic :: get_values => get_values_rsp_3
        procedure :: get_values_rsp_3
        generic :: get_values => get_values_rsp_4
        procedure :: get_values_rsp_4
        generic :: get_values => get_values_rsp_5
        procedure :: get_values_rsp_5
        generic :: get_values => get_values_rsp_6
        procedure :: get_values_rsp_6
        generic :: get_values => get_values_rsp_7
        procedure :: get_values_rsp_7
        generic :: get_values => get_values_rdp_1
        procedure :: get_values_rdp_1
        generic :: get_values => get_values_rdp_2
        procedure :: get_values_rdp_2
        generic :: get_values => get_values_rdp_3
        procedure :: get_values_rdp_3
        generic :: get_values => get_values_rdp_4
        procedure :: get_values_rdp_4
        generic :: get_values => get_values_rdp_5
        procedure :: get_values_rdp_5
        generic :: get_values => get_values_rdp_6
        procedure :: get_values_rdp_6
        generic :: get_values => get_values_rdp_7
        procedure :: get_values_rdp_7
        generic :: get_values => get_values_iint8_1
        procedure :: get_values_iint8_1
        generic :: get_values => get_values_iint8_2
        procedure :: get_values_iint8_2
        generic :: get_values => get_values_iint8_3
        procedure :: get_values_iint8_3
        generic :: get_values => get_values_iint8_4
        procedure :: get_values_iint8_4
        generic :: get_values => get_values_iint8_5
        procedure :: get_values_iint8_5
        generic :: get_values => get_values_iint8_6
        procedure :: get_values_iint8_6
        generic :: get_values => get_values_iint8_7
        procedure :: get_values_iint8_7
        generic :: get_values => get_values_iint16_1
        procedure :: get_values_iint16_1
        generic :: get_values => get_values_iint16_2
        procedure :: get_values_iint16_2
        generic :: get_values => get_values_iint16_3
        procedure :: get_values_iint16_3
        generic :: get_values => get_values_iint16_4
        procedure :: get_values_iint16_4
        generic :: get_values => get_values_iint16_5
        procedure :: get_values_iint16_5
        generic :: get_values => get_values_iint16_6
        procedure :: get_values_iint16_6
        generic :: get_values => get_values_iint16_7
        procedure :: get_values_iint16_7
        generic :: get_values => get_values_iint32_1
        procedure :: get_values_iint32_1
        generic :: get_values => get_values_iint32_2
        procedure :: get_values_iint32_2
        generic :: get_values => get_values_iint32_3
        procedure :: get_values_iint32_3
        generic :: get_values => get_values_iint32_4
        procedure :: get_values_iint32_4
        generic :: get_values => get_values_iint32_5
        procedure :: get_values_iint32_5
        generic :: get_values => get_values_iint32_6
        procedure :: get_values_iint32_6
        generic :: get_values => get_values_iint32_7
        procedure :: get_values_iint32_7
        generic :: get_values => get_values_iint64_1
        procedure :: get_values_iint64_1
        generic :: get_values => get_values_iint64_2
        procedure :: get_values_iint64_2
        generic :: get_values => get_values_iint64_3
        procedure :: get_values_iint64_3
        generic :: get_values => get_values_iint64_4
        procedure :: get_values_iint64_4
        generic :: get_values => get_values_iint64_5
        procedure :: get_values_iint64_5
        generic :: get_values => get_values_iint64_6
        procedure :: get_values_iint64_6
        generic :: get_values => get_values_iint64_7
        procedure :: get_values_iint64_7
        generic :: get_values => get_values_csp_1
        procedure :: get_values_csp_1
        generic :: get_values => get_values_csp_2
        procedure :: get_values_csp_2
        generic :: get_values => get_values_csp_3
        procedure :: get_values_csp_3
        generic :: get_values => get_values_csp_4
        procedure :: get_values_csp_4
        generic :: get_values => get_values_csp_5
        procedure :: get_values_csp_5
        generic :: get_values => get_values_csp_6
        procedure :: get_values_csp_6
        generic :: get_values => get_values_csp_7
        procedure :: get_values_csp_7
        generic :: get_values => get_values_cdp_1
        procedure :: get_values_cdp_1
        generic :: get_values => get_values_cdp_2
        procedure :: get_values_cdp_2
        generic :: get_values => get_values_cdp_3
        procedure :: get_values_cdp_3
        generic :: get_values => get_values_cdp_4
        procedure :: get_values_cdp_4
        generic :: get_values => get_values_cdp_5
        procedure :: get_values_cdp_5
        generic :: get_values => get_values_cdp_6
        procedure :: get_values_cdp_6
        generic :: get_values => get_values_cdp_7
        procedure :: get_values_cdp_7
    end type

    !> Version: experimental
    !>
    !> Abstract class that is extended according to the type of the underlying array.
    !> [Specification](../page/specs/stdlib_array.html#array_type)
    type, abstract, public :: array_type
        !> Name of the array.
        character(:), allocatable :: name
    end type

    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_1)
    type, extends(array_type), public :: array_type_rsp_1
        real(sp), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_2)
    type, extends(array_type), public :: array_type_rsp_2
        real(sp), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_3)
    type, extends(array_type), public :: array_type_rsp_3
        real(sp), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_4)
    type, extends(array_type), public :: array_type_rsp_4
        real(sp), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_5)
    type, extends(array_type), public :: array_type_rsp_5
        real(sp), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_6)
    type, extends(array_type), public :: array_type_rsp_6
        real(sp), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rsp_7)
    type, extends(array_type), public :: array_type_rsp_7
        real(sp), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_1)
    type, extends(array_type), public :: array_type_rdp_1
        real(dp), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_2)
    type, extends(array_type), public :: array_type_rdp_2
        real(dp), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_3)
    type, extends(array_type), public :: array_type_rdp_3
        real(dp), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_4)
    type, extends(array_type), public :: array_type_rdp_4
        real(dp), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_5)
    type, extends(array_type), public :: array_type_rdp_5
        real(dp), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_6)
    type, extends(array_type), public :: array_type_rdp_6
        real(dp), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for real(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_rdp_7)
    type, extends(array_type), public :: array_type_rdp_7
        real(dp), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_1)
    type, extends(array_type), public :: array_type_iint8_1
        integer(int8), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_2)
    type, extends(array_type), public :: array_type_iint8_2
        integer(int8), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_3)
    type, extends(array_type), public :: array_type_iint8_3
        integer(int8), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_4)
    type, extends(array_type), public :: array_type_iint8_4
        integer(int8), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_5)
    type, extends(array_type), public :: array_type_iint8_5
        integer(int8), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_6)
    type, extends(array_type), public :: array_type_iint8_6
        integer(int8), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int8) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint8_7)
    type, extends(array_type), public :: array_type_iint8_7
        integer(int8), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_1)
    type, extends(array_type), public :: array_type_iint16_1
        integer(int16), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_2)
    type, extends(array_type), public :: array_type_iint16_2
        integer(int16), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_3)
    type, extends(array_type), public :: array_type_iint16_3
        integer(int16), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_4)
    type, extends(array_type), public :: array_type_iint16_4
        integer(int16), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_5)
    type, extends(array_type), public :: array_type_iint16_5
        integer(int16), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_6)
    type, extends(array_type), public :: array_type_iint16_6
        integer(int16), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int16) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint16_7)
    type, extends(array_type), public :: array_type_iint16_7
        integer(int16), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_1)
    type, extends(array_type), public :: array_type_iint32_1
        integer(int32), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_2)
    type, extends(array_type), public :: array_type_iint32_2
        integer(int32), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_3)
    type, extends(array_type), public :: array_type_iint32_3
        integer(int32), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_4)
    type, extends(array_type), public :: array_type_iint32_4
        integer(int32), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_5)
    type, extends(array_type), public :: array_type_iint32_5
        integer(int32), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_6)
    type, extends(array_type), public :: array_type_iint32_6
        integer(int32), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int32) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint32_7)
    type, extends(array_type), public :: array_type_iint32_7
        integer(int32), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_1)
    type, extends(array_type), public :: array_type_iint64_1
        integer(int64), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_2)
    type, extends(array_type), public :: array_type_iint64_2
        integer(int64), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_3)
    type, extends(array_type), public :: array_type_iint64_3
        integer(int64), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_4)
    type, extends(array_type), public :: array_type_iint64_4
        integer(int64), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_5)
    type, extends(array_type), public :: array_type_iint64_5
        integer(int64), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_6)
    type, extends(array_type), public :: array_type_iint64_6
        integer(int64), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for integer(int64) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_iint64_7)
    type, extends(array_type), public :: array_type_iint64_7
        integer(int64), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_1)
    type, extends(array_type), public :: array_type_csp_1
        complex(sp), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_2)
    type, extends(array_type), public :: array_type_csp_2
        complex(sp), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_3)
    type, extends(array_type), public :: array_type_csp_3
        complex(sp), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_4)
    type, extends(array_type), public :: array_type_csp_4
        complex(sp), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_5)
    type, extends(array_type), public :: array_type_csp_5
        complex(sp), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_6)
    type, extends(array_type), public :: array_type_csp_6
        complex(sp), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(sp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_csp_7)
    type, extends(array_type), public :: array_type_csp_7
        complex(sp), allocatable :: values(:,:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_1)
    type, extends(array_type), public :: array_type_cdp_1
        complex(dp), allocatable :: values(:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_2)
    type, extends(array_type), public :: array_type_cdp_2
        complex(dp), allocatable :: values(:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_3)
    type, extends(array_type), public :: array_type_cdp_3
        complex(dp), allocatable :: values(:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_4)
    type, extends(array_type), public :: array_type_cdp_4
        complex(dp), allocatable :: values(:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_5)
    type, extends(array_type), public :: array_type_cdp_5
        complex(dp), allocatable :: values(:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_6)
    type, extends(array_type), public :: array_type_cdp_6
        complex(dp), allocatable :: values(:,:,:,:,:,:)
    end type
    !> Version: experimental
    !>
    !> Array type for complex(dp) arrays of rank ${rank}.
    !> Extends array_type and contains the values of the array.
    !> [Specification](../page/specs/stdlib_array.html#array_type_cdp_7)
    type, extends(array_type), public :: array_type_cdp_7
        complex(dp), allocatable :: values(:,:,:,:,:,:,:)
    end type

    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    !> [Specification](../page/specs/stdlib_array.html#add_array)
    interface add_array
        pure module subroutine add_array_rsp_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rsp_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rsp_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rsp_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rsp_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rsp_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rsp_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(sp), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_rdp_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            real(dp), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint8_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int8), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint16_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int16), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint32_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int32), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_iint64_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            integer(int64), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_csp_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(sp), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_1(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_2(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_3(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_4(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_5(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_6(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
        pure module subroutine add_array_cdp_7(arrays, array, stat, msg, name)
            !> Array of arrays to which the array is to be added.
            type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
            !> Array to be added.
            complex(dp), intent(in) :: array(:,:,:,:,:,:,:)
            !> Status of addition.
            integer, intent(out), optional :: stat
            !> Error message.
            character(len=:), allocatable, intent(out), optional :: msg
            !> Name of the array to be added. A default name will be used if not provided.
            character(len=*), intent(in), optional :: name
        end
    end interface

contains

    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rsp_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(sp), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rsp_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_rdp_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        real(dp), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_rdp_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint8_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int8), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint8_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint16_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int16), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint16_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint32_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int32), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint32_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_iint64_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        integer(int64), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_iint64_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_csp_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(sp), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_csp_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_1(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_1)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_2(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_2)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_3(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_3)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_4(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_4)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_5(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_5)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_6(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_6)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end
    !> Version: experimental
    !>
    !> Extract array values from an array wrapper.
    !> [Specification](../page/specs/stdlib_array.html#get_values)
    pure subroutine get_values_cdp_7(wrapper, values, stat, msg)
        !> Array wrapper to extract the values from.
        class(array_wrapper_type), intent(in) :: wrapper
        !> Extracted values.
        complex(dp), allocatable, intent(out) :: values(:,:,:,:,:,:,:)
        !> Optional status of the extraction.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg

        if (present(stat)) stat = 0

        select type (array_ => wrapper%array)
          class is (array_type_cdp_7)
            values = array_%values
          class default
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Array is of unexpected type."
        end select
    end

    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rsp_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(sp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rsp_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_rdp_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        real(dp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_rdp_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint8_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int8), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint8_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint16_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int16), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint16_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint32_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int32), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint32_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_iint64_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        integer(int64), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_iint64_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_csp_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(sp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_csp_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_1(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_1) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_2(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_2) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_3(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_3) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_4(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_4) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_5(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_5) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_6(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_6) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
    end
    !> Version: experimental
    !>
    !> Add an array to an array of array wrappers.
    pure module subroutine add_array_cdp_7(arrays, array, stat, msg, name)
        !> Array of wrapper arrays to which the array is to be added.
        type(array_wrapper_type), allocatable, intent(inout) :: arrays(:)
        !> Array to be added.
        complex(dp), intent(in) :: array(:,:,:,:,:,:,:)
        !> Status of addition.
        integer, intent(out), optional :: stat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> Name of the array to be added. A default name will be used if not provided.
        character(len=*), intent(in), optional :: name

        integer :: i, arr_size
        type(array_type_cdp_7) :: t_arr
        type(array_wrapper_type), allocatable :: tmp_arrays(:)

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

        arr_size = size(arrays)
        do i = 1, arr_size
            if (arrays(i)%array%name == t_arr%name) then
                if (present(stat)) stat = 1
                if (present(msg)) msg = "Array with the same name '"//t_arr%name//"' already exists."
                return
            end if
        end do

        allocate(tmp_arrays(arr_size + 1))
        tmp_arrays(:arr_size) = arrays
        allocate(tmp_arrays(arr_size + 1)%array, source=t_arr)
        call move_alloc(tmp_arrays, arrays)
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
