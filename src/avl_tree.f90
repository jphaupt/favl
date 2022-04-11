!! author: jphaupt
!!
!! Implementation of an AVL tree
!! in princple, I would prefer this to actually inherit/extend a generic BST


module avl_tree
    use iso_fortran_env, only: dp=>real64, stdout=>output_unit
    implicit none

    type avl_node_t
    private
        class(avl_node_t), pointer :: left=>null()
        class(avl_node_t), pointer :: right=>null()
        real(dp), public :: val
        integer :: key
        integer :: height

    end type avl_node_t
    type avl_tree_t
        class(avl_node_t), pointer :: root=>null()
    contains
        procedure, pass(self) :: insert=>insert_into_tree
    end type avl_tree_t

    private
    public :: avl_tree_t

contains

    pure recursive subroutine insert_into_tree(self, key, val)
        class(avl_tree_t), intent(inout) :: self
        integer, intent(in) :: key
        real(dp), intent(in) :: val
        call insert_node(self%root, key, val)
    end subroutine insert_into_tree

    pure recursive subroutine insert_node(node, key, val)
        ! TODO at the moment, this insert is for a normal BST
        ! NOT an *AVL* tree!! (...yet)
        class(avl_node_t), pointer, intent(inout) :: node
        integer, intent(in) :: key
        real(dp), intent(in) :: val
        if (.not. associated(node)) then
            allocate(node)
            ! this is a null pointer, i.e. empty tree
            ! TODO ? do I need to initialise it?
            node%val=val
            node%key=key
            nullify(node%left)
            nullify(node%right)
            ! print*, 'what'
        else if (key < node%val) then ! left insert
            call insert_node(node%left, key, val)
        else ! right insert
            call insert_node(node%right, key, val)
        end if
        ! print*, "TODO stub"
    end subroutine insert_node

    recursive subroutine print_tree(tree)
        !! prints tree in infix order
        !! this is only really for debugging purposes
        type (avl_node_t), intent(in), pointer :: tree  ! root node
        if (associated (tree)) then
            ! write(stdout,*) "left for key ", tree%key
        call print_tree (tree % left)
        ! write(stdout,fmt="(1x,i0)", advance="no") tree%key
        write(stdout,fmt="(1x,i0)") tree%key
        ! write(stdout,*) "right for key ", tree%key
        ! if(associated(tree % right)) write(stdout,*) "right"
        call print_tree (tree % right)
        end if
    end subroutine print_tree

end module avl_tree
