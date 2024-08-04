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
        procedure :: allocate_array

    end type

    type, abstract, public :: t_array
        character(:), allocatable :: name
    end type


contains
    subroutine allocate_array(wrapper, array, stat, msg)
        class(t_array_wrapper), intent(out) :: wrapper
        class(t_array), intent(in) :: array
        integer, intent(out) :: stat
        character(len=:), allocatable, intent(out) :: msg

        print *, 'hi'
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
