set(core_src
    avl_tree.f90
)

set(main_src
    main.f90
)

add_executable(main ${main_src} ${core_src})
