module test_filesystem
    use stdlib_io_filesystem
    use stdlib_string_type, only: char, string_type
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
    implicit none
    private

    public :: collect_filesystem

    character(*), parameter :: temp_list_dir = 'temp_list_dir'

contains

    !> Collect all exported unit tests
    subroutine collect_filesystem(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fs_file_not_exists", fs_file_not_exists, should_fail=.true.), &
            new_unittest("fs_file_exists", fs_file_exists), &
            new_unittest("fs_current_dir_exists", fs_current_dir_exists), &
            new_unittest("fs_run_invalid_command", fs_run_invalid_command, should_fail=.true.), &
            new_unittest("fs_run_with_invalid_option", fs_run_with_invalid_option, should_fail=.true.), &
            new_unittest("fs_run_valid_command", fs_run_valid_command), &
            new_unittest("fs_list_dir_empty", fs_list_dir_empty), &
            new_unittest("fs_list_dir_one_file", fs_list_dir_one_file), &
            new_unittest("fs_list_dir_two_files", fs_list_dir_two_files) &
            ]
    end

    subroutine fs_file_not_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing

        is_existing = exists("nonexistent")
        call check(error, is_existing, "Non-existent file should fail.")
    end

    subroutine fs_file_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing
        integer :: unit
        character(*), parameter :: filename = "file.tmp"

        open(newunit=unit, file=filename)
        close(unit)

        is_existing = exists(filename)
        call check(error, is_existing, "An existing file should not fail.")
        call delete_file(filename)
    end

    subroutine fs_current_dir_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing

        is_existing = exists(".")
        call check(error, is_existing, "Current directory should not fail.")
    end

    subroutine fs_run_invalid_command(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("invalid_command", iostat=stat)
        call check(error, stat, "Running an invalid command should fail.")
    end

    subroutine fs_run_with_invalid_option(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("whoami -X", iostat=stat)
        call check(error, stat, "Running a valid command with an invalid option should fail.")
    end

    subroutine fs_run_valid_command(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("whoami", iostat=stat)
        call check(error, stat, "Running a valid command should not fail.")
    end

    subroutine fs_list_dir_empty(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        type(string_type), allocatable :: files(:)

        call run('rm -rf '//temp_list_dir, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Removing directory '"//temp_list_dir//"' failed."); return
        end if

        call run('mkdir '//temp_list_dir, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, files, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(files) == 0, "The directory should be empty.")

        call run('rm -rf '//temp_list_dir, iostat=stat)
    end

    subroutine fs_list_dir_one_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        type(string_type), allocatable :: files(:)
        character(*), parameter :: filename = 'abc.txt'

        call run('rm -rf '//temp_list_dir, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Removing directory '"//temp_list_dir//"' failed."); return
        end if

        call run('mkdir '//temp_list_dir, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file'"//filename//"' in directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, files, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(files) == 1, "The directory should contain one file.")
        call check(error, char(files(1)) == filename, "The file should be '"//filename//"'.")

        call run('rm -rf '//temp_list_dir, iostat=stat)
    end

    subroutine fs_list_dir_two_files(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        type(string_type), allocatable :: files(:)
        character(*), parameter :: filename1 = 'abc.txt'
        character(*), parameter :: filename2 = 'xyz'

        call run('rm -rf '//temp_list_dir, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Removing directory '"//temp_list_dir//"' failed."); return
        end if

        call run('mkdir '//temp_list_dir, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename1, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file 1 in directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename2, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file 2 in directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, files, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(files) == 2, "The directory should contain two files.")
        call check(error, char(files(1)) == filename1, "The file should be '"//filename1//"'.")
        call check(error, char(files(2)) == filename2, "The file should be '"//filename2//"'.")

        call run('rm -rf '//temp_list_dir, iostat=stat)
    end

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: io

        open(newunit=io, file=filename)
        close(io, status="delete")
    end

end

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_filesystem, only : collect_filesystem
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("filesystem", collect_filesystem) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end