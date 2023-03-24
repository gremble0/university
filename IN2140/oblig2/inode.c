#include "allocation.h"
#include "inode.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define iLoop for( ; ; )
#define BLOCKSIZE 4096

static int next_inode_id = 0;


struct inode *create_file(struct inode *parent, char *name, char readonly, int size_in_bytes) {
    if(find_inode_by_name(parent, name)) return NULL; 

    int num_blocks = size_in_bytes / BLOCKSIZE; 
    int blocks[num_blocks];  

    for(int i = 0; i < num_blocks; i++) {
        blocks[i] = allocate_block(); 
    }

    char *name_cp = malloc(strlen(name) + 1);
    strcpy(name_cp, name);

    struct inode *file = malloc(sizeof(struct inode));
    file->entries = malloc(sizeof(uintptr_t) *num_blocks +1);

    for(int i = 0; i < num_blocks; i++) {
        file->entries[i] = blocks[i];
    }

    file->id = next_inode_id;
    file->name = name_cp;
    file->is_directory = 0;
    file->is_readonly = readonly;
    file->filesize = size_in_bytes;
    file->num_entries = num_blocks;

    parent->entries = realloc(parent->entries, (parent->num_entries * sizeof(uintptr_t)) + sizeof(uintptr_t));
    parent->entries[parent->num_entries] = (uintptr_t) file;
    parent->num_entries++;

    next_inode_id++;
    return file;
}


struct inode *create_dir(struct inode *parent, char *name) {   
    if(parent != NULL) {
        if(find_inode_by_name(parent, name)) return NULL;
        parent->entries = realloc(parent->entries, ((parent->num_entries) * sizeof(uintptr_t)) + sizeof(uintptr_t));
        parent->num_entries++;
    }

    char *name_cp = malloc(strlen(name) + 1);
    strcpy(name_cp, name);
    struct inode *dir = malloc(sizeof(struct inode));

    dir->id = next_inode_id; 
    dir->name = name_cp;
    dir->is_directory = 1;
    dir->is_readonly = 0;
    dir->filesize = 0;
    dir->num_entries = 0;
    dir->entries = malloc(sizeof(uintptr_t*));

    if(parent != NULL) parent->entries[parent->num_entries-1] = (uintptr_t) dir;
    next_inode_id++;

    return dir;
}


struct inode *find_inode_by_name(struct inode* parent, char* name) {
    for(int i = 0; i < parent->num_entries; i++) {
        if(strcmp(((struct inode*)parent->entries[i])->name, name) == 0) {
            return (struct inode*)parent->entries[i];
        }
    }

    return NULL;
}


struct inode *load_inodes() {
    FILE *mft;
    mft = fopen("master_file_table", "r");

    fseek(mft, 0L, SEEK_END);
    long len = ftell(mft);
    fseek(mft, 0L, SEEK_SET);
    struct inode *inodes[len / 18];

    int META[2];
    int i = 0;
    iLoop { // macro
        int read = fread(META, sizeof(META), 1, mft);
        if(read != 1) { 
            if(read == 0) {
                break; 
            }
            break; 
        }

        char BUF[META[1] + 10];
        read = fread(BUF, sizeof(BUF), 1, mft);
        if(read != 1) break; 

        char *name = malloc(META[1]);
        strcpy(name, BUF);

        struct inode *node = malloc(sizeof(struct inode));
        node->name = name;
        node->id = META[0];
        node->is_directory = BUF[META[1]];
        node->is_readonly = BUF[META[1] + 1];
        node->filesize = *((int *)(BUF + META[1] + 2));
        node->num_entries = *((int *)(BUF + META[1] + 6));

        uintptr_t *entries = malloc(sizeof(uintptr_t) *(node->num_entries + 1));
        read = fread(entries, sizeof(uintptr_t), node->num_entries, mft);

        if(read != node->num_entries) break;

        node->entries = entries;
        inodes[META[0]] = node;

        i++;
        next_inode_id++;
    }

    fclose(mft);

    for(int k = 0; k < i; k++) {
        if(inodes[k]->is_directory) {
            for(int j = 0; j < inodes[k]->num_entries; j++) {
                inodes[k]->entries[j] = (uintptr_t) inodes[inodes[k]->entries[j]];
            }
        } else {
            for(int j = 0; j < inodes[k]->num_entries; j++) {
                allocate_block();
            }
        }
    }
    
    return inodes[0];
}


void fs_shutdown(struct inode *inode) {
    struct inode **entries = (struct inode**) inode->entries;
    if(inode->is_directory) {
        for(int i = 0; i < inode->num_entries; i++) {
            fs_shutdown(entries[i]);
        }
    } else {
        for(int i = 0; i < inode->num_entries; i++) {
            free_block((uintptr_t) entries[i]);
        }
    } 
    free(inode->entries);
    free(inode->name);
    free(inode);
    return;
}


/*
 * This static variable is used to change the indentation while debug_fs
 *is walking through the tree of inodes and prints information.
 */
static int indent = 0;

void debug_fs(struct inode *node) {
    if(node == NULL) return;
    for(int i=0; i<indent; i++)
        printf("  ");
    if(node->is_directory) {
        printf("%s(id %d)\n", node->name, node->id);
        indent++;
        for(int i=0; i<node->num_entries; i++) {
            struct inode *child =(struct inode*)node->entries[i];
            debug_fs(child);
        }
        indent--;
    } else {
        printf("%s(id %d size %db blocks ", node->name, node->id, node->filesize);
        for(int i=0; i<node->num_entries; i++) {
            printf("%d ",(int)node->entries[i]);
        }
        printf(")\n");
    }
}
