program example_savenpy
    use stdlib_io_np, only: save_npy
    implicit none
    real :: x(3, 2) = 1
    call save_npy('example.npy', x)
end