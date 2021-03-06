!! author: jphaupt
!!
!! Implementation of an AVL tree
!! in princple, I would prefer this to actually inherit/extend a generic BST
!! Largely modified from https://gist.github.com/Harish-R/097688ac7f48bcbadfa5
!! as well as the CASINO avl tree implementation


module avl_tree
    use iso_fortran_env, only: dp=>real64, stdout=>output_unit
    implicit none

    type avl_node_t
    private
        class(avl_node_t), pointer :: left=>null()
        class(avl_node_t), pointer :: right=>null()
        real(dp), public :: val
        integer :: key
        integer :: height=-1

    end type avl_node_t
    type avl_tree_t
        class(avl_node_t), pointer :: root=>null()
    contains
        procedure, pass(self) :: insert=>insert_into_tree
        procedure, pass(self) :: print=>print_tree
        procedure, pass(self) :: find
    end type avl_tree_t

    private
    public :: avl_tree_t

contains

    pure subroutine insert_into_tree(self, key, val)
        class(avl_tree_t), intent(inout) :: self
        integer, intent(in) :: key
        real(dp), intent(in) :: val
        call insert_node(self%root, key, val)
    end subroutine insert_into_tree

    subroutine print_tree(self)
        class(avl_tree_t), intent(in) :: self
        call print_from_node(self%root)
    end subroutine

    ! not really sure why this can't be a pure function
    real(dp) function find(self, key) result(val)
        !! searches the binary tree for the given key and returns the
        !! corresponding value if it is found
        class(avl_tree_t), intent(in) :: self
        integer, intent(in) :: key
        val = find_from_node(self%root, key)
    end function find

    !-----------NODE FUNCTIONS--------------------------------------------------
    ! not really sure why this can't be a pure function
    real(dp) recursive function find_from_node(root, key) result(val)
        class(avl_node_t), pointer, intent(in) :: root
        integer, intent(in) :: key
        if(.not. associated(root)) return ! not found
        if(root%key == key) then
            val = root%val
        else if(key < root%key) then ! left-subtree search
            val = find_from_node(root%left, key)
        else ! right-subtree search
            val = find_from_node(root%right, key)
        end if
    end function find_from_node

    pure recursive subroutine insert_node(node, key, val)
        ! insert a new node with `key` and `val` given at the given node `node`
        class(avl_node_t), pointer, intent(inout) :: node
        integer, intent(in) :: key
        real(dp), intent(in) :: val
        if (.not. associated(node)) then
            allocate(node)
            ! this is a null pointer, i.e. empty tree
            node%val=val
            node%key=key
            node%height=0
            nullify(node%left)
            nullify(node%right)
        else if (key < node%key) then ! left insert
            call insert_node(node%left, key, val)
            if(get_height(node%left) - get_height(node%right) == 2) then
                if(key < node%left%key) then
                    call singleRightRotate(node)
                else
                    call doubleRightRotate(node)
                end if
            end if
        else ! right insert
            call insert_node(node%right, key, val)
            if(get_height(node%right)-get_height(node%left)==2) then
                if(key > node%right%key) then
                    call singleLeftRotate(node)
                else
                    call doubleLeftRotate(node)
                end if
            end if
        end if
    end subroutine insert_node

    pure subroutine singleLeftRotate(root)
        class(avl_node_t), pointer, intent(inout) :: root
        class(avl_node_t), pointer :: new_root
        new_root => root%right
        root%right => new_root%left
        new_root%left => root
        root%height = max(get_height(root%left), get_height(root%right))+1
        new_root%height = max(get_height(root%right), root%height)+1
        new_root => root
    end subroutine singleLeftRotate

    pure subroutine singleRightRotate(root)
        class(avl_node_t), pointer, intent(inout) :: root
        class(avl_node_t), pointer :: new_root
        new_root => root%left
        root%left => new_root%right
        new_root%right => root
        root%height = max(get_height(root%left), get_height(root%right))+1
        new_root%height = max(get_height(new_root%left), root%height)+1
        new_root => root
    end subroutine singleRightRotate

    pure subroutine doubleLeftRotate(root)
        ! also known as left-right rotation
        class(avl_node_t), pointer, intent(inout) :: root
        call singleRightRotate(root%right)
        call singleLeftRotate(root)
    end subroutine doubleLeftRotate

    pure subroutine doubleRightRotate(root)
        ! also known as right-left rotation
        class(avl_node_t), pointer, intent(inout) :: root
        call singleLeftRotate(root%left)
        call singleRightRotate(root)
    end subroutine doubleRightRotate

    recursive subroutine print_from_node(root)
        !! prints tree in infix order
        !! this is only really for debugging purposes
        type (avl_node_t), intent(in), pointer :: root  ! root node
        if (associated (root)) then
        call print_from_node(root % left)
        write(stdout,fmt="(1x,i0,A,G0)") root%key, ', ', root%val
        call print_from_node(root % right)
        end if
    end subroutine print_from_node

    pure integer function get_height(node) result(h)
        class(avl_node_t), pointer, intent(in) :: node
        if (associated(node)) then
            h=node%height
        else
            h=-1
        end if
    end function get_height

end module avl_tree
