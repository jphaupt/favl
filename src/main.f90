program main
    use iso_fortran_env, only: dp=>real64
    use avl_tree
    implicit none
    !! simply does a sort with an AVL tree
    type(avl_tree_t) :: test_tree
    !! TODO tree constructor and procedures
    call test_tree%insert(2, 523._dp)
    call test_tree%insert(4, 2.713_dp)
    call test_tree%insert(1, 2.12_dp)
    call test_tree%insert(3, 3.14159_dp)
    call test_tree%print()
    print*, test_tree%find(2) ! 523
    ! call insert(test_tree, 23, 2.12_dp)
    ! call insert(test_tree, 25, 1.12_dp)
    ! call insert(test_tree, 2, 4.12_dp)
    print*, 'ran successfully'
end program main
