program main
    use iso_fortran_env, only: dp=>real64
    use avl_tree
    implicit none
    !! simply does a sort with an AVL tree
    type(avl_tree_t), pointer :: test_tree
    ! test_tree=>null()
    ! !! TODO tree constructor and procedures
    ! call insert(test_tree, 23, 2.12_dp)
    ! call insert(test_tree, 25, 1.12_dp)
    ! call insert(test_tree, 2, 4.12_dp)
    ! call print_tree(test_tree)
    print*, 'ran successfully'
end program main
