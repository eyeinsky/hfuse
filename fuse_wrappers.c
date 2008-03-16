#define FUSE_USE_VERSION 26

#include "fuse_wrappers.h"

int
fuse_main_wrapper (int argc,
                   char *argv[],
                   const struct fuse_operations *op)
{
    return (fuse_main (argc, argv, op, NULL));
}
