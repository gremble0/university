#include "inode.h"
#include "allocation.h"

#include <stdio.h>
#define PRINT_STRUCT(STRUCT) {\
    printf("id: %d\n", STRUCT -> id);\
    printf("name: %s\n", STRUCT -> name);\
    printf("is_directory: %d\n", STRUCT -> is_directory);\
    printf("is_readonly: %d\n", STRUCT -> is_readonly);\
    printf("filesize: %d\n", STRUCT -> filesize);\
    printf("num_entries: %d\n", STRUCT -> num_entries);\ 
    for (int i=0; i < STRUCT -> num_entries; i++)\
    {\
        printf("entry %d: %p\n", i, STRUCT -> entries[i]);\
    }\
}

int main()
{
    int error;

    /* format_disk() makes sure that the simulated
     * disk is empty. It creates a file named
     * block_allocation_table that contains only
     * zeros. */
    format_disk();

    /* debug_disk() write the current content of the
     * block_allocation_table that simulates whether
     * blocks on disk contain file data (1) or not (0).
     */
    debug_disk();

    printf("===================================\n");
    printf("= Create root dir                 =\n");
    printf("===================================\n");
    struct inode* root = create_dir( NULL, "/" );
    debug_fs( root );

    printf("===================================\n");
    printf("= Create kernel file in dir root = \n");
    printf("===================================\n");
    struct inode* file_kernel = create_file( root, "kernel", 1, 20000 );
    debug_fs( root );
    debug_disk();

    printf("===================================\n");
    printf("= Create dir etc in dir root      =\n");
    printf("===================================\n");
    struct inode* dir_etc = create_dir( root, "etc" );
    create_file( dir_etc, "hosts", 1, 200 );
    debug_fs( root );

    printf("===================================\n");
    printf("= Create usr/bin, usr/local/bin   =\n");
    printf("===================================\n");
    struct inode* dir_usr   = create_dir( root, "usr" );
    struct inode* dir_bin   = create_dir( dir_usr, "bin" );
    struct inode* dir_local = create_dir( dir_usr, "local" );
    struct inode* dir_lbin  = create_dir( dir_local, "bin" );
    create_file( dir_bin, "ls", 1, 14322 );
    create_file( dir_bin, "ps", 1, 13800 );
    create_file( dir_lbin, "nvcc", 1, 28000 );
    create_file( dir_lbin, "gcc", 1, 12623 );
    debug_fs( root );
    debug_disk();

    fs_shutdown( root );

    printf("++++++++++++++++++++++++++++++++++++++++++++++++\n");
    printf("+ All inodes structures have been              +\n");
    printf("+ deleted. The inode info is stored in         +\n");
    printf("+ master_file_table. The allocated file blocks +\n");
    printf("+ are stored in block_allocation_table         +\n");
    printf("++++++++++++++++++++++++++++++++++++++++++++++++\n");
}

