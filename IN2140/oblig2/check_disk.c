#include "allocation.h"

#include <stdio.h>

void alloc_n(int n)
{
    for( int i=0; i<n; i++ )
    {
        int val = allocate_block();
        if( val==-1 )
        {
            fprintf(stderr, "Failed to allocate a block\n");
        }
    }
}

int main()
{
    format_disk();
    debug_disk();
        
    alloc_n(1);
    debug_disk();

    alloc_n(10);
    debug_disk();

    free_block(3);
    debug_disk();

    alloc_n(4);
    debug_disk();

    alloc_n(5);
    debug_disk();

    alloc_n(31);
    debug_disk();

    alloc_n(2);
    debug_disk();
}

