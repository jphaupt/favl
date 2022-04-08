program main
    use iso_fortran_env, only: dp=>real64
    use avl_tree, only: avl_tree_t
    implicit none
    !! simply does a sort with an AVL tree
    type(avl_tree_t) :: test_tree
    test_tree%insert(4)
end program main
